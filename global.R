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
  

treasury_data_scraped <- read_treasury() %>% dplyr::rename(date = Date)

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

(over_under_sofr <- treasury_data %>% 
  dplyr::mutate(diff1 = `0 Mo` - `1 Mo`) %>% 
  dplyr::mutate(diff2 = `1 Mo` - `3 Mo`) %>% 
  dplyr::select(date, diff1, diff2) %>% 
  dplyr::rename(d_0Mo_1Mo = diff1, d_1Mo_3Mo = diff2) %>% 
  tidyr::pivot_longer(cols = -date) %>% 
  drop_na() %>% 
  ggplot() +
    geom_col(aes(x = date, y = value, color = name)) + 
  facet_grid(name~.)
  )

  # Getting web scraped data (2 month etc.)

# Merging Datasets

# PCA Data Prep



# From this point onward, data manipulation should be within "server". 
# This will allow for more selective time periods for model training etc.
