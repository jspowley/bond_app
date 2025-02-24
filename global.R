library(tidyverse)
library(tidyquant)

maturities_included <- 10

# Getting Treasury Data
tickers <- c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20","DGS30")

# Moving forward, this should have a means of reconciling time periods with less benchmarks. Perhaps we should interpolate here as well to save on recalculation.
treasury_data_api <- tq_get(tickers,
                        get = "economic.data",
                        from = "1992-01-01") %>% 
  # group_by(symbol) %>% 
  # mutate(price = (price - lag(price))) %>% 
  dplyr::filter(symbol %in% tickers) %>% 
  dplyr::mutate(symbol = symbol %>% str_extract("[0-9]+MO|[0-9]+"),
                symbol = dplyr::case_when(
                  str_detect(symbol, pattern  = "MO") ~ paste0(str_extract(symbol, "[0-9]+"), " Mo"),
                  TRUE ~ paste0(str_extract(symbol, pattern = "[0-9]+"), " Yr"))) %>% 
  dplyr::mutate(price = price / 100) %>% 
  pivot_wider(id_cols = date, names_from = symbol, values_from = price) %>%
  arrange(date) %>% 
  drop_na()
  
# We can get 2 mo, 4 mo and 2 y this way. More Data!
treasury_data_scraped <- read_treasury() %>% dplyr::rename(date = Date)

# Solving our anchoring issue for the front portion of the curve, somewhat.
overnight_data <- tq_get(c("EFFR", "SOFR", "DFEDTAR"), get = "economic.data", from = "1990-01-01") %>% 
  pivot_wider(names_from = symbol, values_from = price) %>% 
  mutate(value = ifelse(is.na(SOFR), ifelse(is.na(EFFR), DFEDTAR, EFFR), SOFR)) %>% 
  arrange(date) %>% 
  select(date, value)

overnight_yields <- overnight_data %>% 
  dplyr::mutate(value = value/100) %>% 
                #value = ((((1+value/360)^360)^(1/2))-1)*2) %>% 
  dplyr::rename(`0 Mo` = value)

api_names <- treasury_data_api %>% colnames()
scraped_names <- treasury_data_scraped %>% colnames()
ommissions <- scraped_names[!scraped_names %in% api_names]

treasury_data <- treasury_data_api %>% 
  dplyr::left_join(treasury_data_scraped %>% dplyr::select(dplyr::any_of(c("date",ommissions))), by = "date") %>% 
  dplyr::bind_rows(
    dplyr::anti_join(treasury_data_scraped, treasury_data_api, by = "date")
  ) %>% 
  left_join(overnight_yields, by = "date") %>% 
  arrange(date)
  
# Tests for bias
lm1 <- treasury_data %>% dplyr::select(`0 Mo`, `1 Mo`) %>% 
  drop_na() %>% 
  lm(formula = `0 Mo` ~ `1 Mo`)

# A simple way of removing SOFR risk premiums to bring it down to a lowest risk yield. 
# More sophisticated methods might use swaps against the 3 month yield to determine how the credit risk premium has changed over time.
# See 2008 spreads to understand that this is an imperfect solution (credit risk premium rose due to crisis and liquidity constraints, 
# where the fed can money print and remains risk free)
# Centering and unbiasing hopes to preserve some movement and structure without always leaning towards one direction or another. 
# We use the error term to inform deviations.
# That being said, interest rate risk is more weighted to the back of the curve, so if risk is overstated on the front half due to credit premium driven volatility, so be it,
# The impacts will be minimal for what a bond portfolio manager would typically use.
# This is also what orthogonality is for from our PCA!!! Hopefully the disconnect get's pushed further back in the chain of variance explained.
bias <- lm1 %>% broom::tidy() %>% .$estimate %>% .[1]
co <- lm1 %>% broom::tidy() %>% .$estimate %>% .[2]

treasury_data <- treasury_data %>% dplyr::mutate(`0 Mo` = `0 Mo`/co - bias)

over_under_sofr <- treasury_data %>% 
  dplyr::mutate(diff1 = `0 Mo` - `1 Mo`) %>% 
  dplyr::mutate(diff2 = `1 Mo` - `3 Mo`) %>% 
  dplyr::select(date, diff1, diff2) %>% 
  dplyr::rename(d_0Mo_1Mo = diff1, d_1Mo_3Mo = diff2) %>% 
  tidyr::pivot_longer(cols = -date) %>% 
  drop_na() %>% 
  ggplot() +
    geom_col(aes(x = date, y = value, color = name)) + 
  facet_grid(name~.)

# Filling in the blanks. Let's make sure PCA has everything it needs to run training back to 1990 if selected:

treasury_data <- treasury_data %>% 
  tidyr::pivot_longer(cols = -date) %>% 
  dplyr::mutate(months = 
                  case_when(stringr::str_detect(name, "Mo") ~ as.numeric(stringr::str_extract(name, "[0-9]+")),
                            stringr::str_detect(name, "Yr") ~ as.numeric(stringr::str_extract(name, "[0-9]+"))*12,))

maturities <- treasury_data %>% dplyr::select(months) %>% dplyr::arrange(months) %>% dplyr::pull(months) %>%  unique()

missing_vals <- treasury_data %>%
  dplyr::filter(is.na(value)) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(missing = list(months), .groups = "keep") %>% 
  dplyr::rowwise() %>% 
  filter(length(missing) < 7) %>%  # Ensuring we have enough rate benchmarks, for example, SOFR only is a no go
  dplyr::ungroup()
  
missing_vals <- dplyr::left_join(missing_vals, treasury_data, by = "date") %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(
    interpolation = list(fit_h_spline(
      months,
      value,
      first(missing)
    )),
    .groups = "keep"
  )



treasury_data_int <- 
  treasury_data %>% 
  dplyr::select(value, months, date) %>% 
  dplyr::arrange(months) %>% 
  dplyr::filter(is.na(value)) %>% 
  dplyr::rowwise() %>% 
  dplyr::left_join(missing_vals, by = "date") %>% 
  dplyr::filter(!is.null(interpolation)) %>% 
  dplyr::mutate(value = list(interpolation[as.character(months)]) %>% unlist()) %>% 
  dplyr::select(-interpolation)
  
# Merging in rowswhere all values provided

treasury_data_int <- dplyr::anti_join(treasury_data, treasury_data_int, by = c("date", "months")) %>% 
  dplyr::select(date, months, value) %>% 
  dplyr::bind_rows(treasury_data_int) %>% 
  dplyr::ungroup() %>% 
  drop_na() %>% 
  dplyr::distinct() %>% 
  dplyr::arrange(date, months)

# Merging Datasets

# PCA Data Prep



# From this point onward, data manipulation should be within "server". 
# This will allow for more selective time periods for model training etc.
