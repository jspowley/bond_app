

date_current <- as.Date("2020-02-01")

ai_fraction <- function(date_in, months_forward){
  
  y <- as.numeric(lubridate::year(date_in))
  c <- date_in == lubridate::ceiling_date(date_in, "month")-1
  # l <- y %% 4
  m <- as.numeric(lubridate::month(date_in))
  d <- as.numeric(lubridate::day(date_in))
  
  months_forward <- (months_forward - 1) %% 6 + 1
  
  m2 <- m + months_forward
  y2 <- y
  
  if(m2 > 12){
    m2 <- m2 - 12
    y2 <- y + 1
  }
  
  m1 <- m2 - 6
  y1 <- y2
  
  if(m1 < 1){
    m1 <- m1 + 12
    y1 <- y2 - 1
  }
  
  m1 <- sprintf("%02d", m1)
  m2 <- sprintf("%02d", m2)
  d <- sprintf("%02d", d)
  
  if(c){
    date_1 <- lubridate::ceiling_date(lubridate::as_date(paste0(y1,m1,"01"), format = "%Y%m%d"), "month")-1
    date_2 <- lubridate::ceiling_date(lubridate::as_date(paste0(y2,m2,"01"), format = "%Y%m%d"), "month")-1
  }else{
    date_1 <- lubridate::as_date(paste0(y1,m1,d), format = "%Y%m%d")
    date_2 <- lubridate::as_date(paste0(y2,m2,d), format = "%Y%m%d")
  }
  
  days_in <- as.numeric(date_2 - date_1)
  days_through <- as.numeric(date_in - date_1)
  
  return(days_through/days_in)
}

#yield_curve %>% 
#  data.frame(term = as.numeric(names(.)), yield = .) %>% 
#  dplyr::rowwise() %>% 
#  dplyr::mutate(ai = ai_fraction(date_current, term))

#yield_bench <- yield_curve %>% 
#  data.frame(term = as.numeric(names(.)), yield = .)

#for(i in 1:14){
#  yield_bench <-
#    dplyr::bind_rows(yield_bench, yield_bench)
#}

#yield_bench %>% 
#  dplyr::rowwise() %>% 
#  dplyr::mutate(ai = ai_fraction(date_current, term))

  
ai_from_df <- function(df_in, date_in){  
  
  df_in %>% 
  dplyr::mutate(
    date_in = date_in,
    y = as.numeric(lubridate::year(date_in)),
    c = date_in == lubridate::ceiling_date(date_in, "month")-1,
    # l = y %% 4,
    m = as.numeric(lubridate::month(date_in)),
    d = as.numeric(lubridate::day(date_in)),
    months_forward = (term - 1) %% 6 + 1,
    mat_ref = ((term - 1) %% 12 + 1) > 6, 
    year_mat_adj = floor((term-1)/12),
    m2 = m + months_forward,
    y2 = ifelse(m2 > 12, y + 1, y),
    m2 = (m2 - 1) %% 12 + 1,
    m1 = m2 - 6,
    y1 = ifelse(m1 < 1, y2 - 1, y2),
    m1 = (m1 - 1) %% 12 + 1,
    m1 = sprintf("%02d", m1),
    m2 = sprintf("%02d", m2),
    d = sprintf("%02d", d),
    
    maturity = ifelse(mat_ref, 
                      paste0(y1 + 1 + year_mat_adj, m1),
                      paste0(y2 + year_mat_adj, m2)
                      ),
    
    date_1 = 
      ifelse(c,
             lubridate::ceiling_date(lubridate::as_date(paste0(y1,m1,"01"), format = "%Y%m%d"), "month")-1,
             lubridate::as_date(paste0(y1,m1,d), format = "%Y%m%d")
             ),
    date_2 = ifelse(
      c,
      lubridate::ceiling_date(lubridate::as_date(paste0(y2,m2,"01"), format = "%Y%m%d"), "month")-1,
      lubridate::as_date(paste0(y2,m2,d), format = "%Y%m%d")
    ),
    maturity = ifelse(
      c,
      lubridate::ceiling_date(lubridate::as_date(paste0(maturity,"01"), format = "%Y%m%d"), "month")-1,
      lubridate::as_date(paste0(maturity,d), format = "%Y%m%d")
    ),
    days_in = as.numeric(date_2 - date_1),
    days_through = as.numeric(date_in - date_1),
    ai = days_through / days_in,
    dtm = as.numeric(as_date(maturity) - date_in)
  ) %>% return()
}

#bs_ready <- yield_curve %>% 
#  data.frame(term = as.numeric(names(.)), yield = .) %>% 
#  ai_from_df()


bootstrap_1 <- function(df_in){
  
  bs_ready <- df_in %>% 
  dplyr::mutate(bs_group = term %% 6) %>% 
  dplyr::arrange(term) %>% 
  dplyr::group_by(bs_group) %>% 
  dplyr::mutate(price = 100 + (100*yield/2) * ai,
                final_t = ceiling(term/2),
                dcf = NA)



output <- NULL
for(i in 0:5){
  
  b <- bs_ready %>% 
    dplyr::filter(term > 0) %>% 
    dplyr::filter(bs_group == i)
  
  for(r in 1:nrow(b)){
    
    #print(r)
    
    target <- b %>% dplyr::slice(r)
    #print(target)
    yie <- target$yield
    pri <- target$price
    f_t <- target$final_t
    
    #print("conditional")
    #print(pri)
    #print(yie)
    
    if(r > 1){
      
      pre <- b %>% dplyr::slice(1:(r-1))
      
      pv_rem <- pre %>% dplyr::mutate(pv_removed = dcf * (yie/2)*100) %>% dplyr::pull(pv_removed) %>% sum()
      # print(pv_rem)
      pri <- pri - pv_rem
      
    }
    
    #print("write")
    #print(pri)
    dcf_out <- pri/((target$yield/2)*100 + 100)
    #print(dcf_out)
    b[r,"dcf"] <- dcf_out
    
    #print("written")
  }
  
  if(is.null(output)){
    output <- b
  }else{
    output <- dplyr::bind_rows(output, b)
  }
}

  overnight <- bs_ready %>% dplyr::filter(term == 0)
  c <- overnight$c
  date_1 <- overnight$date_1
  y <- overnight$y2 - 1
  m <- overnight$m2
  d <- overnight$d
  date_2 <- ifelse(
    c,
    lubridate::ceiling_date(as_date(paste0(y,m,d), format = "%Y%m%d"), "month"),
    as_date(paste0(y,m,d), format = "%Y%m%d") + 1
  )
  date_1 <- date_1 + 1
  
  days_in <- as.numeric(date_1 - date_2)
  
  overnight$days_in <- days_in
  overnight$days_through <- days_in - 1
  overnight$ai <- (days_in - 1)/days_in
  overnight$dtm = 1
  overnight$maturity <- date_1
  overnight$final_t <- 1
  overnight$price <- (overnight$yield/2)*100*overnight$ai + 100
  overnight$dcf <- overnight$price / ((overnight$yield/2)*100 + 100)
  

  output <- dplyr::bind_rows(overnight, output)
return(output %>% dplyr::arrange(term))

}

#yield_curve %>% 
#  data.frame(term = as.numeric(names(.)), yield = .) %>%
#  ai_from_df() %>% 
#  dplyr::mutate(bs_group = term %% 6) %>% 
#  dplyr::arrange(term) %>% 
#  dplyr::group_by(bs_group) %>% 
#  dplyr::mutate(price = 100 + (100*yield/2) * ai,
#                final_t = ceiling(term/2),
#                dcf = NA) %>% View()

#yield_curve %>% 
#  data.frame(term = as.numeric(names(.)), yield = .) %>%
#  ai_from_df() %>% 
#  bootstrap_1() %>% View()
