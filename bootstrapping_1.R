library(explore)

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

yield_curve %>% 
  data.frame(term = as.numeric(names(.)), yield = .) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(ai = ai_fraction(date_current, term))

yield_bench <- yield_curve %>% 
  data.frame(term = as.numeric(names(.)), yield = .)

for(i in 1:14){
  yield_bench <-
    dplyr::bind_rows(yield_bench, yield_bench)
}

yield_bench %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(ai = ai_fraction(date_current, term))


yield_bench %>% 
  # dplyr::rowwise() %>% 
  dplyr::mutate(
    date_in = date_current,
    y = as.numeric(lubridate::year(date_in)),
    c = date_in == lubridate::ceiling_date(date_in, "month")-1,
    # l = y %% 4,
    m = as.numeric(lubridate::month(date_in)),
    d = as.numeric(lubridate::day(date_in)),
    months_forward = (term - 1) %% 6 + 1,
    m2 = m + months_forward,
    y2 = ifelse(m2 > 12, y + 1, y),
    m2 = (m2 - 1) %% 12 + 1,
    m1 = m2 - 6,
    y1 = ifelse(m1 < 1, y2 - 1, y2),
    m1 = (m1 - 1) %% 12 + 1,
    m1 = sprintf("%02d", m1),
    m2 = sprintf("%02d", m2),
    d = sprintf("%02d", d),
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
    days_in = as.numeric(date_2 - date_1),
    days_through = as.numeric(date_in - date_1),
    ai = days_through / days_in
  )
