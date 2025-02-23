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

deltas <- function(df_in){
  # returns all lags on all columns in a dataframe
  
  c_names <- df_in %>% colnames()
  df_in %>% mutate(across(.cols = c_names, .fns = function(x){x-lag(x)})) %>% 
    tail(-1) %>% 
    return()
}

pct_deltas <- function(df_in){
  # returns all lags on all columns in a dataframe
  
  c_names <- df_in %>% colnames()
  df_in %>% mutate(across(.cols = c_names, .fns = function(x){(x-lag(x)})/lag(x))) %>% 
    tail(-1) %>% 
    return()
}
