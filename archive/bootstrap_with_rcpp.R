library(Rcpp)

Rcpp::sourceCpp("bootstrap_optimized.cpp")

# Pick up where VAR left off.
yields_with_ai <- readRDS("sample_yields.rds")

yields_with_ai

bs_cpp_df <- yields_with_ai %>% 
  dplyr::mutate(bs_group = term %% 6) %>% 
  dplyr::arrange(iter, bs_group, term) %>% 
  dplyr::mutate(price = 100 + 100*((1+yield/2)^ai-1),
                final_t = ceiling(term/2),
                dcf = NA)


temp <- bootstrap(
  iter = bs_cpp_df$iter,
  term = bs_cpp_df$term,
  bs_group = bs_cpp_df$bs_group,
  yield = bs_cpp_df$yield,
  price = bs_cpp_df$price,
  dcf = bs_cpp_df$dcf,
  r_count = nrow(bs_cpp_df)
)

bs_cpp_df$dcf <- temp

bs_cpp_df %>% 
  dplyr::filter(iter == 1) %>% 
  dplyr::arrange(term) %>% 
  .$dcf %>% 
  .[1:10]
