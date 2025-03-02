library(dplyr)

Rcpp::sourceCpp("ai_df.cpp")

ai_df_cpp <- function(df_in){

df_mid <- df_in %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(c = date_in == lubridate::ceiling_date(date_in,"month")-1)

df_out <- ai_df(df_mid$date_in, df_mid$term, df_mid$c)

print("rcpp done")
return(cbind(df_mid, df_out))
}
     
# ai_df_cpp(test) %>% View()      
