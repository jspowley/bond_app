library(httr)
library(rvest)

fetch_yield_page <- function(y = 2025){
  
  url <- "https://home.treasury.gov/resource-center/data-chart-center/interest-rates/TextView?type=daily_treasury_yield_curve&field_tdr_date_value="
  url <- paste0(url,y)
  raw_data <- rvest::read_html(url)
  return(raw_data)
  
}

get_table <- function(html_in, class_name = NA){
  if(is.na(class_name)){
    c.s.s <- "table"
  }else{
    c.s.s <- paste0("table.",class_name)
  }
  
  html_in %>% html_elements(css = c.s.s) %>% return()
}

scrape_selection <- c("1 Mo", "2 Mo", "3 Mo", "4 Mo", "1 Yr", "2 Yr", "3 Yr", "5 Yr", "7 Yr", "10 Yr", "20 Yr", "30 Yr")

y <- 2025
temp <- fetch_yield_page(y) %>% get_table("usa-table") %>% .[[1]] %>%  rvest::html_table()

cleanup_tbl <- function(tbl, sel){
  tbl %>% 
    dplyr::select(dplyr::any_of(c("Date", sel))) %>% 
    dplyr::mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
                  dplyr::across(dplyr::any_of(scrape_selection), function(x){as.numeric(x)/100})) %>% 
    return()
}

treasury_tables_range <- function(start, end){
  
  output <- NULL
  
  for(y in start:end){
    
    out <- fetch_yield_page(y) %>% 
      get_table("usa-table") %>% 
      .[[1]] %>%  
      rvest::html_table() %>% 
      cleanup_tbl(scrape_selection)
    
    if(is.null(output)){
      output <- out
    }else{
      output <- dplyr::bind_rows(output, out)
    }
  }
  
  return(output)
  
}

update_all <- function(){
  output <- treasury_tables_range(1990, as.numeric(lubridate::year(Sys.Date())))
  last_update <- as.numeric(lubridate::year(Sys.Date()))
  saveRDS(output, "bin/treasury_cache.rds")
  saveRDS(last_update, "bin/last_treasury_update.rds")
}
