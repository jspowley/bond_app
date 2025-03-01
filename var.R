library(facmodCS)
library(moments)
library(plotly)

sample_size <- 10000

PCs <- readRDS("PCs.rds")
deltas_in <- readRDS("deltas.rds")
curve_in <- readRDS("yield_selected.rds")

curve_in <- curve_in %>% data.frame()

yield_pc <- load_to_pc(curve_in, PCs)

sample_deltas <- NULL
descriptive_stats <- NULL

for(i in 1:ncol(deltas_in)){
  
  #print(i)
  #print(yield_pc[[i]])
  c_in <- deltas_in[i]
  c_name <- colnames(c_in)
  c_in <- deltas_in[[i]]
  
  s <- moments::skewness(c_in)
  k <- moments::kurtosis(c_in)
  
  # Rough check for domain validity. 
  # (Literally eyeballing the domain validity function, with some triangles to better capture shape, and trying to keep it "bounded" inside.)
  domain_valid <- FALSE
  
  if(s >= -1 & s <= 1){
    if(k <= abs(s)*0.8 + 8 & k >= abs(s)*1.7){
      domain_valid <- TRUE
    }
  }
  
  if((s < -1 | s < 1) & (s >= -2 | s <= 2)){
    if(k <= abs(s)*2.8 + 8.8 & k >= abs(s)*4.7 + 1.7){
      domain_valid <- TRUE
    }
  }
  
  if((s < -2 | s < 2) & (s >= -2.4 | s <= 2.4)){
    if(k <= abs(s)*1.4 + 11.6 & k >= abs(s)*5.6 + 6.4){
      domain_valid <- TRUE
    }
  }
  
  if(domain_valid){ # When domain is valid, use cornish fissher to better model tail risk. Don't use when not within a valid domain.
  r_sample <- facmodCS::rCornishFisher( # We'll want to check the step size on this later, since it is a quantile transform
    n = sample_size,
    sigma = sd(c_in),
    skew = moments::skewness(c_in),
    ekurt = moments::kurtosis(c_in) - 3
  ) + mean(c_in) + yield_pc[[i]] # Adding in average and the base yield curve.
  
  }else{
    r_sample <- sample(c_in, sample_size, replace = TRUE) + yield_pc[[i]]
  }
  
  r_sample_df <- data.frame(temp = r_sample)
  r_sample_df <- purrr::set_names(r_sample_df, c_name)
  
  desc_df <- data.frame(
    group = c_name,
    mean = mean(c_in),
    sd = sd(c_in),
    skew = moments::skewness(c_in),
    kurt = moments::kurtosis(c_in),
    valid_domain = domain_valid)
  
  
  if(is.null(sample_deltas)){
    sample_deltas <- r_sample_df
  }else{
    sample_deltas <- dplyr::bind_cols(sample_deltas, r_sample_df)
  }
  
  if(is.null(descriptive_stats)){
    descriptive_stats <- desc_df
  }else{
    descriptive_stats <- dplyr::bind_rows(descriptive_stats, desc_df)
  }
}

sample_yields <- unload_pc(sample_deltas, PCs)

# Interpolation stage on yield (bootstrap ready)
inter_yields <- sample_yields %>% 
  dplyr::mutate(iter = 1, iter = cumsum(iter)) %>% 
  tidyr::pivot_longer(-iter) %>% 
  dplyr::group_by(iter) %>% 
  dplyr::summarise(fit = list(fit_h_spline(x = as.numeric(name), y = value, missing = 0:360)), .groups = "keep") %>% 
  unnest_longer(fit)
  
# Getting AI Ratios, etc. Quite slow, may need optimization, but is still "bearable"
input_date <- Sys.Date()

yields_with_ai <- inter_yields %>% 
  dplyr::group_by(iter) %>% 
  dplyr::rename(term = fit_id, yield = fit) %>% 
  dplyr::mutate(term = as.numeric(term)) %>% 
  ai_from_df(as_date(input_date)) %>% 
  dplyr::ungroup()

# Running the zero curve bootstrap. 100% needs rcpp
# Ouch, this is slow...

output <- NULL
for(i in 1:sample_size){
  print(i)
  out <- yields_with_ai %>% 
    dplyr::filter(iter == i) %>% 
    bootstrap_1()
  
  if(is.null(output)){
    output <- out
  }else{
    output <- dplyr::bind_rows(output, out)
  }
}

# Charting

pc_risk_driver_dist <- deltas_in %>% 
  dplyr::mutate(temp = 1) %>% 
  tidyr::pivot_longer(cols = -temp) %>% 
  dplyr::select(-temp) %>% 
  dplyr::mutate(name =  factor(name, levels = paste0("PC", 1:14))) %>% 
  ggplot() +
  geom_histogram(aes(x = value), bins = 100) + facet_wrap(~name)

pc_risk_driver_dist

ggplotly(pc_risk_driver_dist)

deltas_in$PC3 %>% moments::kurtosis()
