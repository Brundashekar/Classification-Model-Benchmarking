



accuracy_calc <- function(labels,predictions){
  
  df <- data.frame(truth = labels,pred = predictions)
  
  
  
  
  df$corr <- ifelse(df$truth != df$pred,0,1)
  
  return ((sum(df$corr))/nrow(df))
  
}