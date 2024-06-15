
## The part1_loop script loops over each dataset 20 times, training and testing all 7 models.
#Once completed,3 csv files including the AUCs, accuracies, and training times are saved. 
#as different datasets have different dimensions i mean rows, columns so it is suggested to split dataset into small group and run parallely


setwd("C:/Users/imdin/OneDrive/Desktop/project r heart/Final project")
#keeping the modele in the same directory
source('model.R')

#our datasets are saved in the named datasets,
datadir <- 'data sets//'
datasets = list.files(datadir,pattern='*.csv')

#instead of big names i just simply give numbers for them
codes = c('1','2','3','4','5','6','7','8','9','10','11',
          '12','13','14','15','16','17','18','19','20')

#so as the datasets were in the folder using this script it takes the datasets from there and loop 20 times
for(n in seq(1,(length(datasets)))){
  
#printing the datasets after completion making sure only results for the completed once were printed
  print(datasets[n])
  print(paste0('Dataset ',n))
  dataset = read.csv(paste0(datadir,datasets[n]))
  dataset <- dataset[complete.cases(dataset),]
  
#converting last column "target" into label
  colnames(dataset)[ncol(dataset)] <- 'label'
  dataset$label <- as.factor(dataset$label)
  
#coverting strings to factors if they are saved as so
  dataset <- as.data.frame(unclass(dataset),
                           stringsAsFactors = TRUE)
  
#here we are passing the dataset into our model function iterating 20 times
  tables<-model_func(dataset,20)
  
#as we considering 3 metrics so we keep them individual and seperate
  AUCs <- data.frame(tables[[1]])
  times <- data.frame(tables[[2]])
  acc <- data.frame(tables[3])
  
#putting up info about the dataset which basically is nrows, no of p.
  AUCs$data <- codes[n]
  times$data <- codes[n]
  acc$data <- codes[n]
  
  AUCs$ncases <- nrow(dataset)
  times$ncases <- nrow(dataset)
  acc$ncases <- nrow(dataset)
  
  AUCs$npreds <- ncol(dataset)-1
  times$npreds <- ncol(dataset)-1
  acc$npreds <- ncol(dataset)-1
  
#creating combined dataframe if existed joining or create them using the first
  if(exists(x='all_AUCs') == FALSE & exists(x='all_times') == FALSE){
    all_AUCs <- AUCs
    all_times <- times
    all_acc <- acc
  }else{
#adding the recent generated data to the old dataframe(if error occur after debugging the new data will be joined to past data)
    all_AUCs <- rbind(all_AUCs,AUCs)
    all_times <-rbind(all_times,times)
    all_acc <- rbind(all_acc,acc)
  }
  
  print(paste0(c('Dataset',n,'processing complete')))
  
}

#Saving the data into 3 csv's
write.csv(all_AUCs,'table_AUCs.csv',row.names=FALSE)
write.csv(all_times,'table_times.csv',row.names=FALSE)
write.csv(all_acc,'table_acc.csv',row.names=FALSE)

