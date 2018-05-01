#Load libraries
library(psych) #For cor.ci
# library(Hmisc)
library(VIM)   #For aggr()
library(e1071) #For import naiveBayes
library(class) #For import knn
library(rpart) #For Decision tree

####################################################################################
#                                                                                  #
#                          Process of data manipulation                            #
####################################################################################

#Get the work directory and set it to where the file is.
getwd()
setwd("C:/Users/xinzh/Documents/R_CODE/APSFST")

#Load the csv file as 'data_train' and 'data_test'. Skip irrelevant rows and take the header.
#Now, the dimensions for train and test data set are 60000 x 171 and 16000 x 171, respectively.
data_train <- read.csv('aps_failure_training_set.csv', header = TRUE, skip = 19)
data_test <- read.csv('aps_failure_test_set.csv', header = TRUE, skip = 19)

#Replace 'neg' and 'pos' in the column 'class' as factor 0 and 1.
data_train[,1] <- ifelse(data_train[,1]=='neg',0,1)
data_test[,1] <- ifelse(data_test[,1]=='neg',0,1)

#Replace 'na' as NA.
data_train[data_train=='na'] <- NA
data_test[data_test=='na'] <- NA

####################################################################################
#                                                                                  #
#                          Process of data cleaning                                #
####################################################################################

#Eliminate column with 10% obs are NA for train data
#Now, the dimensions for train and test data set are 60000 x 143 and 16000 x 143, respectively.
colle <- integer()
for (col_num in 1:length(data_train[1,])) {
  if (100*round(length(data_train[is.na(data_train[,col_num]),col_num])/length(data_train[,col_num]),4)>10) {
    print(col_num)
    colle <- c(colle,col_num)
  }
}
data_train <- data_train[,-colle]
data_test <- data_test[,-colle]

#Plot hist for missing data and Pattern for train data
mice_plot <- aggr(data_train, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data_train), cex.axis=0.7,
                  gap=3, ylab=c("Missing data","Pattern"))



#Transform attributes' data type to numeric for both train and test data
for (col_num in 2:length(data_train[1,])) {
  data_train[,col_num] <- as.numeric(data_train[,col_num])
  data_test[,col_num] <- as.numeric(data_test[,col_num])
  print(col_num)
}

#Transform class' data type to factor for both train and test data
data_train[,1] <- as.factor(data_train[,1])
data_test[,1] <- as.factor(data_test[,1])


#Replace NA with median for both train and test data
for (col_num in 2:length(data_train[1,])) {
  data_train[is.na(data_train[,col_num]),col_num] <- median(data_train[,col_num],na.rm = TRUE)
  data_test[is.na(data_test[,col_num]),col_num] <- median(data_train[,col_num],na.rm = TRUE)
  print(col_num)
}

####################################################################################
#                                                                                  #
#                          Process of data validation                              #
####################################################################################

#Data validation
#Detect if there are still obs with the value of NA.
for (t in 1) {
  n <- 0
  m <- 0
  for (col_num in 1:length(data_test[1,])) {
    if (length(data_test[is.na(data_test[,col_num]),col_num])!=0) {
      print(paste0('Column ',col_num, ' has NA in data_test'))
      n <- n+1
    }
    if (length(data_train[is.na(data_train[,col_num]),col_num])!=0) {
      print(paste0('Column ',col_num, ' has NA in data_train'))
      m <- m+1
    }
  }
  if ((n==0) | (m==0)) {
    print('PASS! No NA value is in data sets.')
  }
}

#Detect if there are columns with 10%+ obs are 0. 
for (t in 1) {
  i <- 0
  for (col_num in 2:length(data_train[2,])) {
    if (length(data_train[data_train[,col_num]==0,col_num])/length(data_train[,col_num])*100>10) {
      print(length(data_train[data_train[,col_num]==0,col_num])/length(data_train[,col_num])*100)
      print(col_num)
      i <- i + 1
    }
  }
  if (i == 0) {
    print('PASS! There is no column with more than 10% obs are 0.')  
  }
  
}

####################################################################################
#                                                                                  #
#                          Process of sample splitting                             #
####################################################################################

# k-fold cross validation is Usually used to split train and test data sets in machine learning. However,
# in this case, data has been splitted into two data sets. As a result, there is no need to split it by
# program. 'data_train' will be used to train model and 'data_test' will be used to test the model. 

####################################################################################
#                                                                                  #
#                          Process of model fitting                                #
####################################################################################
#Train models using algorithms Knn, Decision tree and Naive Bayes
#Knn
#For implement knn:
# 1. knn is a noise-robust algorithm. In this data set, program imputes a large number of missing values 
#    and introduces inevitable bias to the data set. In order to balance this bias and train a robust model,
#    knn is selected.
# 2. knn is more quickly when model is building.
data_test.knn <- knn(data_train[,-1],data_test[,-1],data_train[,1],k = 10)
mean(data_test.knn==data_test[,1])

#Decision tree
# 1. Decision tree is an effective algorithm. When comparing with other algorithms, DT can spend less time to
#    build model mostly. Still, although duration for fitting is short, it can always give users a model with
#    high prediction accuracy.
# 2. Decision tree can capture non-liner relationships.
data_test.ds <- rpart(class ~., method="class", data=data_train)
predict.ds <- predict(data_test.ds,data_test[,-1])
predict.ds <- ifelse(predict.ds[,1]>predict.ds[,2],0,1)
mean(predict.ds==data_test[,1])

plot(data_test.ds, uniform=TRUE, main="Classification Tree for APS Failures")
text(data_test.ds, use.n=TRUE, all=TRUE, cex=.5)

#Naive Bayes
# 1. This algorithm always perform well on real-world data, although it can not be guaranteed that all the
#    attributes are independent from each other.
data_test.naiveBayes <- naiveBayes(formula = data_train$class ~., data = data_train)
predict.nb <- predict(data_test.naiveBayes,data_test[,-1])
mean(predict.nb==data_test[,1])


