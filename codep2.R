
setwd("C:/Users/imdin/OneDrive/Desktop/project r heart")

# DSA 6000 Group 3

# The part2_loop script loops over all datasets once, and at each dataset it selects the
# predictors for each model that maximizes AUC via validation process. These predictors are
# then trained on the entire training set and tested on the final test set. Test AUC, accuracy,
# and training time are saved and output to .csv file for further use
getwd()
setwd("C:/Users/imdin/OneDrive/Desktop/project r heart/Final project")

#import necessary libraries and source functions
library('MASS')
library('ISLR')
library('bsnsing')
library('class')
library('caret')
library('randomForest') #random forest decision tree
library('e1071')
library('stats')
library('brif')

source('forwardselectionmod.R')
source('ROC_func.R')
source('accuracymod.R')

#set fixed seed so data is reproducible
set.seed()

#get list of data csv files
datadir <- 'data sets\\'
datasets = list.files(datadir,pattern='*.csv')


AUC_df <- data.frame(matrix(ncol = 8, nrow = length(datasets)))#holds AUC of each model for each dataset
colnames(AUC_df) <- c('glm', 'lda', 'qda','bs','forest','svm','nb','data')

time_df <- data.frame(matrix(ncol = 8, nrow = length(datasets)))#holds training time of each final model
colnames(time_df) <- colnames(AUC_df)

acc_df <- data.frame(matrix(ncol = 8, nrow = length(datasets)))#holds accuracy of each final model
colnames(acc_df) <- colnames(AUC_df)

formula_df <- data.frame(matrix(ncol = 8, nrow = (length(datasets))))#holds formulas of chosen
colnames(formula_df) <- colnames(AUC_df)

#instead of big names i just simply give numbers for them
codes = c('1','2','3','4','5','6','7','8','9','10','11',
          '12','13','14','15','16','17','18','19','20')

#loop over the datasets
for(n in seq(1,length(datasets))){
  
  #renaming last column 
  dataset <- read.csv(paste0(datadir,datasets[n]))
  dataset <- dataset[complete.cases(dataset),]
  colnames(dataset)[ncol(dataset)] <- 'label' 
  
  #converting strings to factors
  dataset <- as.data.frame(unclass(dataset),
                           stringsAsFactors = TRUE)
  
  #splitting train test , general split 70/30 and rest 30 % for hold out
  trainset <- sample(1:nrow(dataset), 0.7*nrow(dataset))  #70% training set
  holdoutset <- setdiff(1:nrow(dataset), trainset)  #final training dataset
  
  #further splitting the 70% set into training and validation (to find and validate formula)
  small_trainset <- sample(1:nrow(dataset[trainset,]), 0.7*nrow(dataset[trainset,]))  
  validset <- setdiff(1:nrow(dataset[trainset,]), small_trainset)
  
  print(paste0('Dataset: ',n))
  
  #getting the optimal auc formula for each model using fowardselectionmod
  glm.form <- forward_sel(dataset,small_trainset,validset,'glm')
  print('glm done')
  lda.form <- forward_sel(dataset,small_trainset,validset,'lda')
  print('lda done')
  qda.form <- forward_sel(dataset,small_trainset,validset,'qda')
  print('qda done')
  bs.form <- forward_sel(dataset,small_trainset,validset,'bsnsing')
  print('bsnsing done')
  forest.form <- forward_sel(dataset,small_trainset,validset,'forest')
  print('forest done')
  svm.form <- forward_sel(dataset,small_trainset,validset,'svm')
  print('svm done')
  nb.form <- forward_sel(dataset,small_trainset,validset,'nb')
  print('nb done')

  #appending formulas to formula_df dataframe
  formula_df$glm[n] <- toString(glm.form)
  formula_df$lda[n] <- toString(lda.form)
  formula_df$qda[n] <- toString(qda.form)
  formula_df$bs[n] <- toString(bs.form)
  formula_df$forest[n] <- toString(forest.form)
  formula_df$svm[n] <- toString(svm.form)
  formula_df$nb[n] <- toString(nb.form)
  
  #using the optimal formula so acheived  and then using for train each model and save the train time
  start.time <- Sys.time()
  glm.model <- glm(glm.form,data=dataset[trainset,],family=binomial)
  time_df$glm[n] <- round(Sys.time() - start.time,3)
  
  start.time <- Sys.time()
  lda.model <- lda(lda.form,data=dataset[trainset,])
  time_df$lda[n] <- round(Sys.time() - start.time,3)
  
  start.time <- Sys.time()
  qda.model <- qda(qda.form,data=dataset[trainset,])
  time_df$qda[n] <- round(Sys.time() - start.time,3)
  
  dataset$label <- as.factor(dataset$label) #for bsnsing and random forest to function
  
  start.time <- Sys.time()
  bs.model <- bsnsing(bs.form,data=dataset[trainset,])
  time_df$bs[n] <- round(Sys.time() - start.time,3)
  
  start.time <- Sys.time()
  forest.model <- randomForest(forest.form,data=dataset[trainset,])
  time_df$forest[n] <- round(Sys.time() - start.time,3)
  
  start.time <- Sys.time()
  svm.model <- svm(svm.form,data=dataset[trainset,],type='C-classification',probability=TRUE)
  time_df$svm[n] <- round(Sys.time() - start.time,3)
  
  start.time <- Sys.time()
  nb.model<- naiveBayes(nb.form,data=dataset[trainset,])
  time_df$nb[n] <- round(Sys.time() - start.time,3)
  
  
  
  #using the formula for prediction and saving it.
  glm.predict <- predict(glm.model,newdata=dataset[holdoutset,],type='response')
  glm.class <- ifelse(glm.predict >= 0.5,1,0)
  acc_df$glm[n] <- accuracy_calc(dataset[holdoutset,'label'],glm.class)
  
  lda.predict <- predict(lda.model,newdata=dataset[holdoutset,],type='response')
  acc_df$lda[n] <- accuracy_calc(dataset[holdoutset,'label'],lda.predict$class)
  
  qda.predict <- predict(qda.model,newdata=dataset[holdoutset,],type='response')
  acc_df$qda[n] <- accuracy_calc(dataset[holdoutset,'label'],qda.predict$class)
  
  bs.predict <- predict(bs.model,newdata=dataset[holdoutset,],type='prob')
  bs.predict2 <- predict(bs.model,newdata=dataset[holdoutset,],type='class')
  acc_df$bs[n] <- accuracy_calc(dataset[holdoutset,'label'],bs.predict2)
  
  forest.predict <- predict(forest.model,newdata=dataset[holdoutset,],type='prob')[,2]
  forest.predict2 <- predict(forest.model,newdata=dataset[holdoutset,],type='class')
  acc_df$forest[n] <- accuracy_calc(dataset[holdoutset,'label'],forest.predict2)
  
  svm.predict <- predict(svm.model,newdata=dataset[holdoutset,],decision.values = TRUE,probability=TRUE)
  svm.predict2 <- predict(svm.model,newdata=dataset[holdoutset,],decision.values = TRUE,probability=FALSE)
  acc_df$svm[n] <- accuracy_calc(dataset[holdoutset,'label'],svm.predict2)
  
  nb.predict <- predict(nb.model,newdata=dataset[holdoutset,],type='raw')[,2]
  nb.predict2 <- predict(nb.model,newdata=dataset[holdoutset,],type='class')
  acc_df$nb[n] <- accuracy_calc(dataset[holdoutset,'label'],nb.predict2)
  
  
  #creating df for AUC
  label_df <- data.frame(label = dataset[holdoutset,'label'])
  label_df$glm <- glm.predict
  label_df$lda <- lda.predict$posterior[,2]
  label_df$qda <- qda.predict$posterior[,2]
  label_df$bs <- bs.predict
  label_df$forest <- forest.predict
  label_df$svm <- attributes(svm.predict)$probabilities[,2]
  label_df$nd <- nb.predict
  
  #with Roc function we are calculating Auc and add it to df
  AUC_df$glm[n] <- ROC_func(label_df,1,2)
  AUC_df$lda[n] <- ROC_func(label_df,1,3)
  AUC_df$qda[n] <- ROC_func(label_df,1,4)
  AUC_df$bs[n] <- ROC_func(label_df,1,5)
  AUC_df$forest[n] <- ROC_func(label_df,1,6)
  AUC_df$svm[n] <- ROC_func(label_df,1,7)
  AUC_df$nb[n] <- ROC_func(label_df,1,8)
  
  #adding the codes to df
  AUC_df$data[n] <- codes[n]
  time_df$data[n] <- codes[n]
  acc_df$data[n] <- codes[n]
  formula_df$data[n] <- codes[n]
  
  
}

write.csv(AUC_df,'table_AUC.csv')
write.csv(time_df,'table_times.csv')
write.csv(acc_df,'table_acc.csv')
write.csv(formula_df,'table_formulas.csv')

