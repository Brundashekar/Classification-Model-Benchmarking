
ROC_func <- function(df, label_colnum, score_colnum, add_on = F, color = "black"){
  # Sort by score (high to low)
  df <- df[order(-df[,score_colnum]),]
  rownames(df) <- NULL  # Reset the row number to 1,2,3,...
  n <- nrow(df)
  # Total # of positive and negative cases in the data set
  P <- sum(df[,label_colnum] == 1)
  N <- sum(df[,label_colnum] == 0)
  
  # Vectors to hold the coordinates of points on the ROC curve
  TPR <- c(0,vector(mode="numeric", length=n))
  FPR <- c(0,vector(mode="numeric", length=n))
  
  # Calculate the coordinates from one point to the next
  AUC = 0
  for(k in 1:n){
    if(df[k,label_colnum] == 1){
      TPR[k+1] = TPR[k] + 1/P
      FPR[k+1] = FPR[k]
    } else{
      TPR[k+1] = TPR[k]
      FPR[k+1] = FPR[k] + 1/N
      AUC = AUC + TPR[k+1]*(1/N)
    }
  }
  
  # Plot the ROC curve
  if(add_on){
    points(FPR, TPR, main=paste0("ROC curve"," (n = ", n, ")"), type = 'l', col=color, cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.2)
  } else{
    plot(FPR, TPR, main=paste0("ROC curve"," (n = ", n, ")"), type = 'l', col=color, cex.lab = 1.2, cex.axis = 1.2, cex.main = 1.2)
  }
  return(AUC)
}

