load_to_pc <- function(observed, rot_matrix){
  # Rotates data into principal components
  
  # Handling data types
  if(is.data.frame(observed)){
    observed <- as.matrix(observed)
  }
  
  if(is.data.frame(rot_matrix)){
    rot_matrix <- as.matrix(rot_matrix)
  }
  
  return(as.data.frame(observed %*% rot_matrix))
  
}


unload_pc <- function(pc, rot_matrix){
  # Rotates principal components into data
  
  # Handling data types
  if(is.data.frame(pc)){
    pc <- as.matrix(pc)
  }
  
  if(is.data.frame(rot_matrix)){
    rot_matrix <- as.matrix(rot_matrix)
  }
  
  return(as.data.frame(pc %*% t(rot_matrix)))
  
}

deltas <- function(df_in, lag_in = 1){
  # returns all lags on all columns in a dataframe
  
  c_names <- df_in %>% colnames()
  
  df_in %>% mutate(across(.cols = dplyr::any_of(c_names), .fns = function(x){x-lag(x, n = lag_in)})) %>% 
    tail(-lag_in) %>% 
    return()
  
}

pct_deltas <- function(df_in){
  # returns all lags on all columns in a dataframe
  
  c_names <- df_in %>% colnames()
  df_in %>% mutate(across(.cols = dplyr::any_of(c_names), .fns = function(x){(x-lag(x))/lag(x)})) %>% 
    tail(-1) %>% 
    return()
}

fit_h_spline <- function(x, y, missing){
  
  yield_fn <- splinefun(x, y, method = "monoH.FC")
  missing <- unlist(missing)
  out <- data.frame(missing = as.numeric(missing), pred = yield_fn(as.numeric(missing)))
  output <- out %>% dplyr::pull(pred) %>% unlist() %>% as.numeric()
  names(output) <- out %>% dplyr::pull(missing)
  return(output)
  
}


price_portfolio <- function(bond_data, today, boot_df) {
  today_parsed <- lubridate::ymd(today)
  ## Inner function
  price_bond <- function(face_value, coupon_rate, maturity_date) {
    
    maturity_date_parsed <- lubridate::ymd(maturity_date)
    
    cf_dates <- seq.Date(from = maturity_date_parsed, to = today_parsed, by = "-6 months") %>%
      tibble::as_tibble() %>%
      rename(date = value) %>%
      mutate(
        cf = dplyr::if_else(
          date == maturity_date_parsed,
          ## Final coupon and princ payment
          face_value + ((coupon_rate / 100) / 2) * face_value,
          (coupon_rate / 2)
        ),
        dtm = as.numeric(date - today_parsed)
      ) %>%
      arrange(date)
    
    ## DCF Interpolation
    strap_df <- tibble::as_tibble(
      fit_h_spline(
        x = boot_df$dtm,
        y = boot_df$dcf,
        missing = cf_dates$dtm
      )
    ) %>%
      mutate(dtm = cf_dates$dtm) %>%
      rename(dcf = value)
    
    cf_dates <- cf_dates %>%
      left_join(strap_df, by = "dtm") %>%
      mutate(pv = dcf * cf)
    
    bond_value <- sum(cf_dates$pv, na.rm = TRUE)
    return(bond_value)
  }
  ## perform inner bond pricing with rowwise function
  bond_data <- bond_data %>%
    rowwise() %>%
    mutate(bond_value = price_bond(face_value, coupon_rate, maturity_date)) %>%
    ungroup()
  
  portfolio_value <- sum(bond_data$bond_value, na.rm = TRUE)
  
  list(
    bond_data = bond_data,
    portfolio_value = portfolio_value
  )
}
