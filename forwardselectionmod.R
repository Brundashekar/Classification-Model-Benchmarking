library('ISLR') #also contains Default dataset
library('stats') #Logistic Regression (glm), part of R base 
library('MASS') #LDA and QDA
library('bsnsing') #bsnsing decision tree
library('randomForest') #random forest decision tree
library('e1071') #support vector machine, Naive Bayes method
library('class') 
library('brif') #brif

source('ROC_func.R')
forward_sel <- function(X,trainset,validset,choice){
  
  
  colnames(X)[ncol(X)] <- 'label' #ensure last column is label
  predictors = setdiff(colnames(X), c('label')) #get the predictors 
  base_formula = c('label ~ ') #set up the formula
  chosen_pred = list() #empty list to hold chosen predictors
  curr_AUC <- 0 #initialize current AUC as 0
  df <-data.frame(label = X[validset,'label']) #dataframe to hold true labels for ROC_func
  
  
  for(i in 1:length(predictors)){
    
    AUC_list <- c() #holds AUC values for all predictors in an iteration
    
    for(p in predictors){
      
      #add the next predictor to be tested to the base formula
      this_formula_string <- paste0(base_formula,p)
      this_formula <- as.formula(this_formula_string)
      
      #depending on the desired method, train and validate the model
      if(choice == 'glm') {
        model <- glm(this_formula, data = X[trainset,],family=binomial)
        pred <- predict(model,newdata=X[validset,],type='response')
      } else if (choice == 'lda'){
        model <- lda(this_formula,data=X[trainset,])
        pred <- predict(model,newdata=X[validset,],type='response')$posterior[,2]
      } else if (choice == 'qda'){ 
        model <- qda(this_formula,data=X[trainset,])
        pred = predict(model,newdata=X[validset,],type='response')$posterior[,2]
      } else if (choice == 'bsnsing'){
        X$label <- as.integer(X$label)
        model <- bsnsing(this_formula,data=X[trainset,])
        pred <- predict(model,newdata=X[validset,],type='prob')
      } else if (choice == 'forest'){
        X$label <- as.factor(X$label)
        model <- randomForest(this_formula,data=X[trainset,])
        pred <- predict(model,newdata=X[validset,],type='prob')[,2]
      } else if (choice == 'svm'){
        model <- svm(this_formula,data=X[trainset,],type='C-classification',probability=TRUE)
        svm_pred <- predict(model,newdata=X[validset,],decision.values = TRUE,probability=TRUE)
        pred <- attributes(svm_pred)$probabilities[,2]
      } else if (choice == 'nb'){
        model<- naiveBayes(this_formula,data=X[trainset,])
        pred <- predict(model,newdata=X[validset,],type='raw')[,2]
      } else if (choice == 'brif'){
        model <- brif(this_formula,data=X[trainset,])
        pred <- predict(model,newdata=X[validset,],type='score')[,2]
      }
      
      
      #adding to Roc_func df and get the Valid set Auc and add to the list
      df$pred <- pred 
      pred_AUC <- ROC_func(df, 1, 2, add_on = F, color = "black") 
      AUC_list <- append(AUC_list,pred_AUC)
      
      
    }#predictors loop end
    
    if (max(AUC_list) > curr_AUC){ #if the best predictor AUC better than current
      
      max_predictor <- predictors[which.max(AUC_list)] #get max AUC predictor using index
      base_formula <- paste0(base_formula,max_predictor) #add to formula string
      
      #moving the picked predictor to chosen list and removing from the predictor list
      predictors <- predictors[predictors != max_predictor]
      chosen_pred <- append(chosen_pred,max_predictor)
      
      #Updating the old Auc to new
      curr_AUC <- max(AUC_list)
      
      
    }else{
      #if there is no change , then con=mbine all predictors and exiting the loop
      chosen_pred <- paste(chosen_pred,collapse='+')
      break
    }
    #add + so base formula ready to test next predictor
    base_formula = paste0(base_formula,'+')
    
    
  }
  
  #return formula for best model using the chosen predictors
  best_formula <- as.formula(paste0('label ~',chosen_pred))
  
  
  return(best_formula)
  
}

