Rcpp::sourceCpp("ai_df.cpp")

df_in <- yields_in

ai_df(df_in$y)
