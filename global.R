library(tidyverse)
library(tidyquant)

# Getting Treasury Data
tickers <- c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20","DGS30")

treasury_data <- tq_get(tickers,
                        get = "economic.data",
                        from = "1992-01-01") %>% 
  group_by(symbol) %>% 
  mutate(price = price - lag(price)) %>% 
  pivot_wider(id_cols = date, names_from = symbol, values_from = price) %>% 
  select(date, any_of(tickers)) %>% 
  drop_na()

# Getting web scraped data (2 month etc.)

# Merging Datasets

# PCA Data Prep

pca_data <- treasury_data %>% select(-date)
colnames(pca_data) <- paste0("T", 1:10)

# From this point onward, data manipulation should be within "server". 
# This will allow for more selective time periods for model training etc.
