
library(readxl)
#main objective removing missing value rows,unnecessary columns, non predictive/ irrevalant rows

getwd()


setwd('C:/Users/imdin/OneDrive/Desktop/project r heart/Final project/datasets2')


#output directory
dir.create('processeddata/')



#after changing the last column "label" into 0,1's when done we remove the obj from the environment.

#cellsamples
cell_samples <- read.csv('cell_samples.csv')
cell_samples$class <- ifelse(cell_samples$Class == 2, 0, 1)
write.csv(cell_samples,'processeddata/cell_samples.csv',row.names = FALSE)
rm('cell_samples')
getwd()
#Disease_symptoms_and_patient_profile_dataset
dsppd <- read.csv('dsppd.csv')
dsppd$Outcome.Variable <- ifelse(dsppd$Outcome.Variable == 'positive', 1, 0)
write.csv(dsppd,'processeddata/dsppd.csv',row.names = FALSE)
rm('dsppd')
#drug200
drug200 <- read.csv('drug200.csv')
drug200$Drug <- ifelse(drug200$Drug == 'drugY' | drug200$Drug == 'drugX', 1, 0)
write.csv(drug200,'processeddata/drug200.csv',row.names = FALSE)
rm('drug200')
#new_model
new_model <- read.csv('new_model.csv')
new_model$Target <- ifelse(new_model$Target == 'enrolled' | new_model$Target == 'graduate', 1, 0)
write.csv(new_model, 'processeddata/new_model', row.names = FALSE)
rm('new_model')
#student
student <- read_excel('student.xlsx')
student$Target <- ifelse(student$Target == 'enrolled' | student$Target == 'graduate', 1, 0)
write.csv(student, file.path("C:\\Users\\imdin\\OneDrive\\Desktop\\project r heart\\Final project\\datasets2\\processeddata", 'student.csv'), row.names = FALSE)
rm('student')

#breast cancer
breast_cancer <- read.csv('breast_cancer.csv')
#convert output to 1 (Class 3 or 4) or 0 (Class 1 or 2)
breast_cancer$class <- ifelse(breast_cancer$class >= 3,1,0)
breast_cancer <- subset(breast_cancer, select = -c(bare_nuclei))
write.csv(breast_cancer,'processeddata/breast_cancer.csv',row.names = FALSE)
rm('breast_cancer')

#Default
default <- read.csv('default.csv')
default$default <- ifelse(default$default == 'Yes',1,0)
write.csv(default,'processeddata/default.csv',row.names = FALSE)
rm('default')

#Diabetes2
diabetes2 <- read.csv('diabetes2.csv',sep=';')
write.csv(diabetes2,'processeddata/diabetes2.csv',row.names=FALSE)
rm('diabetes2')

#liver patient
liver_pt <- read.csv('liver_patient.csv')
liver_pt$disease <- ifelse(liver_pt$disease == 2,1,0)
write.csv(liver_pt,'processeddata/liver_patient.csv',row.names = FALSE)
rm('liver_pt')

#loan data
loan_data <- read.csv('loan_data.csv')
loan_data$Loan_Status <- ifelse(loan_data$Loan_Status == 'Y',1,0)
loan_data <-  loan_data[loan_data$Gender != '',]
loan_data <- loan_data[loan_data$Self_Employed != '',]
loan_data <- loan_data[loan_data$Dependents !='',]
loan_data$Dependents <- gsub('+','',loan_data$Dependents)
loan_data$Education <- gsub(' ','',loan_data$Education)
loan_data <- subset(loan_data, select = -c(Property_Area,Dependents))
write.csv(loan_data,'processeddata/loan_data.csv',row.names = FALSE)
rm('loan_data')

#stroke
stroke <- read.csv('stroke.csv')
stroke <-  stroke[stroke$smoking_status != '',]
stroke <- subset(stroke, select = -c(work_type,smoking_status,Residence_type,ever_married,gender))
write.csv(stroke,'processeddata/stroke.csv',row.names = FALSE)
rm('stroke')


