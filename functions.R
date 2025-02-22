pca_obs <- function(pc_matrix, observations){
  # Gets the principle component observed value for each observation, 
  # allowing us to assess movements within each principle component 
  # in a historical context in the broader context of our model.
  
  return(as.matrix(observations) %*% pc_matrix)
}

all_pct_deltas <- function(matrix_in){
  
  df <- matrix_in %>% as.data.frame()
  c_names <- df %>% colnames()
  df %>% mutate(across(.cols = c(c_names), .fns = function(x){(x-lag(x))/lag(x)})) %>% 
    return()

}

all_deltas <- function(matrix_in){
  
  df <- matrix_in %>% as.data.frame()
  c_names <- df %>% colnames()
  df %>% mutate(across(.cols = c(c_names), .fns = function(x){x-lag(x)})) %>% 
    return()
  
}

