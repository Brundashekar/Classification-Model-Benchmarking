#import necessary libraries
library('ISLR') 
library('stats')  
library('MASS') 
library('bsnsing') 
library('randomForest') 
library('e1071') 
library('class') 
library('caret') 
#X: represents the dataset/dataframe
#iteration= number/ no.of times repeatition
set.seed(3731)
source('ROC_func.R')
source('accuracymod.R')

model_func <- function(X,iterations){
  
  df <- data.frame(matrix(ncol = 8, nrow = iterations)) #holds AUC of each model each iter
  time_df <- data.frame(matrix(ncol = 8,nrow=iterations)) #hold training time
  acc_df <- data.frame(matrix(ncol = 8,nrow=iterations)) #hold accuracy
  
#naming the columns for saving the csv's
  colnames(df) <- c('glm', 'lda', 'qda','bs','forest','svm','nb')
  colnames(time_df) <- c('glm', 'lda', 'qda','bs','forest','svm','nb')
  colnames(acc_df) <- c('glm', 'lda', 'qda','bs','forest','svm','nb')
  
  
  #taking the iterations and looping
  for(i in 1:iterations){
    print(paste0('Iteration: ',i))
    
    
    #Train-Test split, most general im taking 70/30 train/test split
    trainset <- sample(1:nrow(X), 0.7*nrow(X))  
    validset <- setdiff(1:nrow(X), trainset) 
    
    
    #model fitting and training time
    start.time <- Sys.time() #starts a timer
    glm.model <- glm(label ~.,data=X[trainset,],family=binomial) #fit the model
    time_df$glm[i] <- round(Sys.time() - start.time,3) #ends the timer and saves time
    print('LogReg Trained')
    
    start.time <- Sys.time()
    lda.model <- lda(label ~.,data=X[trainset,])
    time_df$lda[i] <- round(Sys.time() - start.time,3)
    print('LDA Trained')
    
    start.time <- Sys.time()
    qda.model <- qda(label ~.,data=X[trainset,])
    time_df$qda[i] <- round(Sys.time() - start.time,3)
    print('QDA Trained')
    
    start.time <- Sys.time()
    bs.model <- bsnsing(label ~.,data=X[trainset,])
    time_df$bs[i] <- round(Sys.time() - start.time,3)
    print('Bsnsing Trained')
    
    start.time <- Sys.time()
    forest.model <- randomForest(as.factor(label)~.,data=X[trainset,])
    time_df$forest[i] <- round(Sys.time() - start.time,3)
    print('Random Forest Trained')
    
    start.time <- Sys.time()
    svm.model <- svm(label~.,data=X[trainset,],type='C-classification',probability=TRUE)
    time_df$svm[i] <- round(Sys.time() - start.time,3)
    print('SVM Trained')
    
    start.time <- Sys.time()
    nb.model<- naiveBayes(label ~.,data=X[trainset,])
    time_df$nb[i] <- round(Sys.time() - start.time,3)
    print('NB Trained')
    
    
    
    # creating dataframe to contain prediction values for AUC calculation
    AUC_df <- data.frame(label=X[validset,ncol(X)])
    
    #model prediction and AUC & accuracy calculations
    glm.predict <- predict(glm.model,newdata=X[validset,],type='response')
    AUC_df$glm <- glm.predict
    df$glm[i] <- ROC_func(AUC_df,1,2)
    glm.class <- ifelse(glm.predict >= 0.5,1,0)
    acc_df$glm[i] <- accuracy_calc(X[validset,'label'],glm.class)
    
    
    lda.predict <- predict(lda.model,newdata=X[validset,],type='response')
    AUC_df$lda <- lda.predict$posterior[,2]
    df$lda[i] <- ROC_func(AUC_df,1,3)
    acc_df$lda[i] <- accuracy_calc(X[validset,'label'],lda.predict$class)
    
    
    
    qda.predict <- predict(qda.model,newdata=X[validset,],type='response')
    AUC_df$qda <- qda.predict$posterior[,2]
    df$qda[i] <- ROC_func(AUC_df,1,4)
    acc_df$qda[i] <- accuracy_calc(X[validset,'label'],qda.predict$class)
    
    
    
    bs.predict <- predict(bs.model,newdata=X[validset,],type='prob')
    bs.predict2 <- predict(bs.model,newdata=X[validset,],type='class') #second prediction for class
    AUC_df$bsnsing <- bs.predict
    df$bs[i] <- ROC_func(AUC_df,1,5)
    acc_df$bs[i] <- accuracy_calc(X[validset,'label'],bs.predict2)
    
    
    
    forest.predict <- predict(forest.model,newdata=X[validset,],type='prob')
    forest.predict2 <- predict(forest.model,newdata=X[validset,],type='class') #second prediction for class
    AUC_df$randomforest <- forest.predict[,2]
    df$forest[i] <- ROC_func(AUC_df,1,6)
    acc_df$forest[i] <- accuracy_calc(X[validset,'label'],forest.predict2)
    
    
    
    svm.predict <- predict(svm.model,newdata=X[validset,],decision.values = TRUE,probability=TRUE)
    #second prediction for class
    svm.predict2 <- predict(svm.model,newdata=X[validset,],decision.values = TRUE,probability=FALSE)
    AUC_df$svm <- attributes(svm.predict)$probabilities[,2]
    df$svm[i] <- ROC_func(AUC_df,1,7)
    acc_df$svm[i] <- accuracy_calc(X[validset,'label'],svm.predict2)
    
    
    
    
    nb.predict <- predict(nb.model,newdata=X[validset,],type='raw')
    nb.predict2 <- predict(nb.model,newdata=X[validset,],type='class') #second prediction for class
    AUC_df$NB <- nb.predict[,2]
    df$nb[i] <- ROC_func(AUC_df,1,8)
    acc_df$nb[i] <- accuracy_calc(X[validset,'label'],nb.predict2)
    
    
    
  }
  
  
  #box plots of AUC's for each method
  boxplot(df,xlab='Method',ylab='AUC',las=2,col=seq(1,ncol(df),1),main='Validation AUC')
  
  #Accuracy for each model
  boxplot(acc_df,xlab='Method',ylab='Accuracy',las=2,col=seq(1,ncol(df),1),main='Validation Accuracy')
  
  #gives box plot for training time for each method
  boxplot(time_df,xlab='Method',ylab='Training Time',las=2,col=seq(1,ncol(df),1),main='Training Time')
  
  
  
  
  #returns AUC values for each method, for each iteration stored in dataframe
  return(list(df,time_df,acc_df))

}
