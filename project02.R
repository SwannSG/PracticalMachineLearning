#project02.R
# 20 October 2014
# We are provided two datasets, training data and testing data.
# The output we are trying to predict is ‘classe’.
# We produce a prediction model that runs against the testing data
# and classifies the output.
# The output ‘classe’ has five categories.
# A or 1 indicates exercise done correctly,
# and B,C,D,E (2,3,4,5) exercise not done correctly.
# Our task is determine the “exercise done correctly” categorisation.   
library(caret)

working_dir <- '~/Courses/Coursera/4-PracticalMachineLearning/project'
setwd(working_dir)

#--look at test data provided
#----what signals do we have for prediction
#----td = test data
td <- read.csv('pml-testing.csv', sep=',', stringsAsFactors=F, header=T)
#--review structure of test data using str(tst)
#----exclude some specific columns
colToDrop <- c('X','user_name', 'raw_timestamp_part_1', 'raw_timestamp_part_2',
               'cvtd_timestamp', 'new_window', 'num_window', 'problem_id')
td <- td[, !names(td) %in% colToDrop]
#--extract numeric columns
col_v <- numeric()
for (i in 1:ncol(td)) {
  if (is.numeric(td[,i])) {
    col_v <- c(col_v, i)
  }
}
td <- td[, col_v]
td_names <- names(td) 

#--look at training data
#----can we align training data and test data inputs
#----all test data is provided for new_window = no, so remove 'yes'
#----classe is the output
#----t = training data
t <- read.csv('pml-training.csv', sep=',', stringsAsFactors=F, header=T)
#--remove output to seperate vector
classe <- t$classe
#--keep only rows where new_window == 'no' to allign with test data
#----keep reference to these rows in classe
new_window_no <- which(t$new_window=='no')
#--only include columns that are numeric
col_v <- numeric()
for (i in 1:ncol(t)) {
  if (is.numeric(t[,i])) {
    col_v <- c(col_v, i)
  }
}
t <- t[, col_v]
#--recombine classe with t (numeric columns)
t <- cbind(t, classe)
t <- t[new_window_no, ]
t_names <- names(t)
#--we only want input names that will be available in the test data
#----that is provided by td_names, plus classe
td_names <- c(td_names, 'classe')
#--extract common column names
common_names <- intersect(t_names,td_names)
#--dumbbell curl movement of interest ony involves dumbbell
#--can ignore belt, arm, forearm (possibly forearm should be included) ????
t <- t[, common_names]
# col_indexes <- grep('dumbbell', names(t))
# col_indexes <- c(col_indexes, grep('forearm', names(t)))
# col_indexes <- c(col_indexes, grep('classe', names(t)) )
# t <- t[, col_indexes]
#--reduce columns further
#t <- t[, c('roll_dumbbell', 'pitch_dumbbell', 'yaw_dumbbell','total_accel_dumbbell', 'classe')]
#--at this point t contains numeric data columns only for dumbbell + classe
#--rows subset where new_window == 'no'
#--first randomize t
set.seed(456) 
random_indexes <- sample(c(1:nrow(t)))
#--partition data into training, validation
#----70% training, 30% validation
indexes <- createDataPartition(random_indexes, p=0.7, list=F)
train <- t[indexes,]
validate <- t[-indexes,]

#--fit model on train
#----train takes too long using caret package so use randomForst directly
#-------modelFit <- train(classe~., data=train, method='rf', prox=TRUE)

#--Uncomment below to re-run the training, but do comment out load('modelFit.RData') 
#----modelFit <- randomForest(classe ~ ., data=train, importance=TRUE, na.action=na.omit)
#----save(modelFit, file='modelFit.RData')
#--comment below line if model fit is re-run
load('modelFit.RData')

#--use validate set to check how accurate the model really is
predicted <- predict(modelFit, validate)

#--task is to predict if exercise is done correctly, classe=A (1)
#----Classification: Exercise Done Correctly
#-------------------Observed (Actual)
#--Predicted-----|-Positive-|-Negative-|
#-------Positive-|----TP----|----FP----|
#-------Negative-|----FN----|----TN----|
#
# TP, true positive, exercise is done correctly and predicted to be done correctly
# FP, false positive, exercise is not done correctly but predicted to be done correctly
# FN, false negative, exercise is done correctly but predicted not to be done correctly
# TN, true negative, exercise is noe done correctly and predicted to be not done correctly
# confusionMatrix( predicted, validate$classe)

#--we change to use just two factors 1 (A) and 2 (B, C, D, E)
#--where 1 (A) represents good exercise
#--and 2 (B,C,D,E) represents bad exercise 
vc_n <- as.numeric(validate$classe)
vc_n[which(vc_n!='1')] <- 2
vp_n <- as.numeric(predicted)
vp_n[which(vp_n!='1')] <- 2
#--generate confusion matrix
cm = confusionMatrix(vp_n, vc_n)
print (cm)

TP <- cm$table[1,1]
print (paste('True positive: ',TP))
FP <- cm$table[1,2]
print (paste('False positive: ',FP))
FN <- cm$table[2,1]
print (paste('False negative: ',FN))
TN <- cm$table[2,2]
print (paste('True negative: ',TN))

print ('')
print ('Indicator (input) names')
print (names(train))

print ('')
print (modelFit)