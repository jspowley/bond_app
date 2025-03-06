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


#price_portfolio <- function(bond_data, today, boot_df) {
#  today_parsed <- lubridate::ymd(today)
#  ## Inner function
##  price_bond <- function(face_value, coupon_rate, maturity_date, boot_df) {
#    
#    maturity_date_parsed <- lubridate::ymd(maturity_date)
#    
#    cf_dates <- seq.Date(from = maturity_date_parsed, to = today_parsed, by = "-6 months") %>%
#      tibble::as_tibble() %>%
#      rename(date = value) %>%
#      mutate(
#        cf = dplyr::if_else(
#          date == maturity_date_parsed,
#          ## Final coupon and princ payment
#          face_value + ((coupon_rate * face_value / 100) / 2),
##          (coupon_rate * face_value / 100) / 2
##        ),
#        dtm = as.numeric(date - today_parsed)
#      ) %>%
#      arrange(date)
#    
#    print("boot df")
#    print(head(boot_df))
#    
#    ## DCF Interpolation
#    strap_df <- tibble::as_tibble(
#      fit_h_spline(
#        x = boot_df$dtm,
#        y = boot_df$dcf,
#        missing = cf_dates$dtm
#      )
#    ) %>%
#      mutate(dtm = cf_dates$dtm) %>%
#      rename(dcf = value)
#    
#    cf_dates <- cf_dates %>%
#      left_join(strap_df, by = "dtm") %>%
#      mutate(pv = dcf * cf)
#    
#    bond_value <- sum(cf_dates$pv, na.rm = TRUE)
#    return(bond_value)
#  }
#  ## perform inner bond pricing with rowwise function
#  print("column names")
#  print(colnames(bond_data))
#  
#  print(head(bond_data))
#  
#  bond_data <- bond_data %>%
#    rowwise() %>%
#    mutate(bond_value = price_bond(face_value, coupon_rate, lubridate::as_date(maturity_date, format = "%Y-%m-%d")), boot_df) %>%
#    ungroup()
#  
#  portfolio_value <- sum(bond_data$bond_value, na.rm = TRUE)
#  
#  list(
#    bond_data = bond_data,
#    portfolio_value = portfolio_value
#  )
#}

#get_bond_schedule <- function(bond_data){
#  
#  output <- list()
#  
#  for(i in 1:nrow(bond_data)){
#    bond_data[i,] %>% .$maturity_date
#  }
#  
#}

# bond_inputs <- readRDS("bond_inputs.rds")

cf_schedule <- function(bond_in, date_in){
  
  output_cf <- c()
  output_dates <- c()
  
  for(r in 1:nrow(bond_in)){

    ref <- lubridate::as_date(bond_in$maturity_date[r], format = "%Y-%m-%d")
    ref_floating <- ref
    
    m_back <- 0
    
    out_cf <- c(bond_in$coupon_rate[r]*bond_in$face_value[r]/200 + bond_in$face_value[r])
    out_dates <- c(ref) 
    
    ceil <- ref == lubridate::ceiling_date(ref, "month") - 1
    m_offset <- 6
    
    while(ref_floating > date_in){
      
      d <- lubridate::day(ref)
      m <- lubridate::month(ref)
      y <- lubridate::year(ref)
      
      # print(paste(y,m,d))
      
      m <- m - ((m_offset - 1) %% 12 + 1)
      y_adj <- floor((m_offset - 1)/12)
      
      y <- y - y_adj
      
      if(m < 1){
        m <- m + 12
        y <- y - 1
      }
      
      # print(y)
      # print(m)
      
      if(ceil){
        
        print("Ceiling")
        
        ref_new <- as_date(paste0(
          y,
          sprintf("%02d", m),
          "01"
        ),
        format = "%Y%m%d") %>% 
          lubridate::ceiling_date("month") - 1
        
      }else{
        
        # print("Regular")
      
        if(m == 2 & d > 28){
          adj <- (y %% 4) == 0
          d <- 28 + adj
        }
        
        ref_new <- as_date(paste0(
          y,
          sprintf("%02d", m),
          sprintf("%02d", d)),
          format = "%Y%m%d")
        
      }
      
      # print(paste(y,m,d))
      
      out_cf <- append(out_cf, bond_in$coupon_rate[r]*bond_in$face_value[r]/200)
      out_dates <- append(out_dates, ref_new)
      
      m_offset <- m_offset + 6
      # print(str(ref_new))
      ref_floating <- ref_new
    }
    
    output_cf <- append(output_cf, out_cf)
    output_dates <- append(output_dates, out_dates)
    
  }
  
  output <- data.frame(date = output_dates, cf = output_cf)
  return(output)
}


interpolate_and_price <- function(boot_df_in, cf_data_in){
  fit_h_spline(x = boot_df_in$dtm, y = boot_df_in$dcf, missing = cf_data_in$dtm) %>% 
    data.frame(dtm = as.numeric(names(.)), dcf = .) %>% 
    right_join(cf_data_in, by = "dtm") %>% 
    dplyr::mutate(pv = cf * dcf) %>% 
    return()
}

#cf_data <- readRDS("cf_schedule.rds") %>% dplyr::mutate(dtm = as.numeric(date - Sys.Date()))
#boot_df <- readRDS("dcf_start.rds")

#interpolate_and_price(boot_df, cf_data)
