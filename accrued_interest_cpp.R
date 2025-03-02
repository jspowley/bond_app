library(dplyr)

Rcpp::sourceCpp("ai_df.cpp")

df_in <- boot

df_mid <- df_in %>% 
  dplyr::ungroup() %>% 
  dplyr::select(term, date_in, yield) %>% 
  dplyr::mutate(c = date_in == lubridate::ceiling_date(date_in,"month")-1,
                y_in = lubridate::year(date_in),
                m_in = lubridate::month(date_in),
                d_in = lubridate::day(date_in))

df_mid <- ai_df(df_mid$y_in, df_mid$m_in, df_mid$d_in, df_mid$term, df_mid$c) %>% 
  dplyr::mutate(date_1 = 
                  lubridate::as_date(
                    paste0(
                      year_d1,
                      sprintf("%02d", month_d1),
                      sprintf("%02d", day_d1)
                    ),
                    format = "%Y%m%d"
                  )
  ) %>% 
  dplyr::mutate(date_2 = 
                  lubridate::as_date(
                    paste0(
                      year_d2,
                      sprintf("%02d", month_d2),
                      sprintf("%02d", day_d2)
                    ),
                    format = "%Y%m%d"
                  )
  ) %>% 
  dplyr::select(date_1, date_2)

dplyr::bind_cols(df_in, df_mid) %>% 
  dplyr::mutate(days_in = as.numeric(date_2 - date_1),
                days_through = as.numeric(date_in - date_1),
                ai = days_through/days_in)
                