#install.packages('rminer')
library(rminer)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

library(caret)

working_dir <- '~/Courses/Coursera/4-PracticalMachineLearning/project'
setwd(working_dir)

td <- read.csv('pml-testing.csv', sep=',', stringsAsFactors=F, header=T)
load('modelFit.RData')
answers <- predict(modelFit, td)
pml_write_files(answers)
