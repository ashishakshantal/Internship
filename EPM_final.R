#clearing the working environment
rm(list = ls(all=TRUE))
#setting the working directory
setwd("E:/Batch-31Internship/practise/Final_submissions")


#reading the files
final = read.csv("EPM_final.csv")
#libraries required
library(lubridate)
library(dplyr)
library(caret)
library(dummies)
library(cluster)
attach(final)

#check the structure of the data, it gives all the data types of the data
str(final)

#Date conversions
final$start_time = dmy_hms(final$start_time)
final$end_time = dmy_hms(final$end_time)

time_diff = as.numeric(final$end_time - final$start_time)

final = select(final, -c(end_time,start_time))
str(final)
final_without_activity = select(final, -c(2,4))

final_without_activity = cbind(final_without_activity,time_diff)
str(final_without_activity)

attach(final)
#converting catagorical variables to factors
final_without_activity$session = as.factor(final_without_activity$session)

#gives the counts for each record in the column
table(final_without_activity$mouse_click_right)
table(final_without_activity$mouse_click_left)
table(final_without_activity$mouse_wheel)

#subsetting numeric data
num_data = select(final_without_activity, c(3:10))
str(num_data)
#standardizing the data using Caret preProcess function
pr = preProcess(num_data, method = c('center', 'scale'))
num_data = predict(pr, num_data)
#removing the numeric columns from the original data and adding the standardized ones
final_without_activity = select(final_without_activity, -c(3:10))
final_without_activity_std = cbind(final_without_activity, num_data)
str(final_without_activity_std)

#this code below saves the boxplots of the mentioned attributes in the local machine
for(i in 6:12){
  dev.copy(jpeg, filename = paste(names(final[i]),"plot.jpeg", sep = "_"))
  par(mfrow = c(1,1))
  boxplot(final[,i], xlab=names(final[i]))
  dev.off()
}


#dummyfying categorical variables

final_without_activity_std$session =dummy(final_without_activity_std$session) 
final_without_activity_std$exercise = dummy(final_without_activity_std$exercise)

str(final_without_activity_std)
############################
# k-means clustering
############################
df1 = daisy(final_without_activity_std,metric = "gower")
df1 = daisy(final)
#this gave an error

memory.limit()
memory.limit(size = 10000)
memory.limit(size = 70000)



###########################################################
#clearing the working environment
rm(list = ls(all = TRUE))
#set working directory
setwd("E:/Batch-31Internship/practise/Final_submissions")
#read the file
epm = read.csv("EPM_final.csv")
#structure of the data and its data types
str(epm)

#libraries used
library(dplyr)
library(caret)
library(lubridate)
library(cluster)
library(Rtsne)

#Date conversions
epm$start_time = dmy_hms(epm$start_time)
epm$end_time = dmy_hms(epm$end_time)
#difference between start time and end time
time_diff  = as.numeric(epm$end_time-epm$start_time)
attach(epm)

#combining the time difference and removing start time and end time from the data
epm = cbind(epm, time_diff)
epm$start_time = NULL
epm$end_time = NULL

#combining the Session, Exercise, Activity to one column
epm$session  = paste(epm$session, "_" , epm$exercise, "_", epm$activity)

#removing the exercise and activity
epm_combined_session = select(epm, -c(exercise,activity))

#from the grades data set, adding the grades columns
A = c(1, 3, 6, 7, 11, 13, 14, 15, 16, 17, 18, 20, 24, 
      27, 28, 29, 32, 36, 39, 41, 44, 48, 53, 56, 63, 66, 
      67, 68, 70, 71, 72, 75, 76, 77, 78, 79, 81, 85, 86, 
      87, 88, 91, 93, 94, 96, 98, 100, 104, 106)
df1 = data.frame()
for (i in A) {
  df1 = rbind(df1,filter(epm_combined_session, epm$student_Id == i))
  print(i)
}
#this adds the grades column to the ddf, only grades "A" is added
df1$grade = ifelse(df1$student_Id == 0, "w","A")


B = c(2, 4, 5, 8, 9, 10, 12, 19, 22, 25, 30, 33, 34, 
      37, 38, 42, 45, 46, 47, 49, 51, 52, 54, 55, 57, 58, 
      59, 60, 61, 62, 64, 69, 73, 74, 80, 82, 83, 89, 92, 
      95, 99, 101, 102, 103)

df2 = data.frame()
for( i in B){
  df2 = rbind(df2,filter(epm_combined_session, epm$student_Id == i))
}

df2$grade = ifelse(df2$student_Id == 0, "w","B")
#combining df1 and df2
combined_df = rbind(df1,df2)
#saving the data frame to local file system
write.csv(data.frame(combined_df), 'All_Students_with_grades.csv', quote = F, 
          row.names = F)
#converted the idle time to minutes
combined_df$idle_time = (combined_df$idle_time/60000)
#type conversions
combined_df$session = as.factor(combined_df$session)
combined_df$grade = as.factor(combined_df$grade)
str(combined_df)

#dividing into train and test
tr = createDataPartition(combined_df$grade, p = 0.8, list = FALSE)
train1 = combined_df[tr,]
test = combined_df[tr,]

#dividing the train1 into train and validation
tr1 = createDataPartition(train1$grade, p = 0.9, list = FALSE)
train = train1[tr1,]
validation = train1[tr1,]

#standardizing the data using preProcess
ppp = preProcess(train, method = c('center','scale'))
std_train = predict(ppp, train)
std_valid = predict(ppp,validation)
std_test = predict(ppp,test)

#building glm model on the data
logistic = glm(grade~., data = std_train, family = 'binomial')


#predictions on validation and test
prob_val = predict(logistic, std_valid)

library(ROCR)

val_pred = prediction(prob_val, std_valid$grade)
#Extract performance measures (True Positive Rate and False Positive Rate)
#using the "performance()" function from the ROCR package
perf <- performance(val_pred, measure="tpr", x.measure="fpr")

#Plot the ROC curve using the extracted performance measures (TPR and FPR)
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))

val_pred = ifelse(val_pred > 0.1, "A", "B")

prob_test = predict(logistic, std_test)
test_pred = prediction(prob_test, std_test$grade)

test_pred = ifelse(test_pred > 0.1, "A", "B")
Accuracy(test_pred, std_test$grade)

############################################################
#this data set contains students who attended all the sessions and three class grades
#clearing the working environment
rm(list = ls(all=TRUE))
#setting the working directory
setwd("E:/Batch-31Internship/practise")

#reading the files
library(readxl)
final = read_xlsx("actual_present.xlsx")
#libraries required
library(lubridate)
library(dplyr)
library(caret)
attach(final)

str(final)

attach(final)
#converting catagorical variables to factors
final$session = as.factor(final$session)
final$exercise = as.factor(final$exercise)
final$activity = as.factor(final$activity)

# 
# for(i in 6:12){
#   dev.copy(jpeg, filename = paste(names(final[i]),"plot.jpeg", sep = "_"))
#   par(mfrow = c(1,1))
#   boxplot(final[,i], xlab=names(final[i]))
#   dev.off()
# }
# 

#dividing idle time by 10^3 to convert to seconds

final$idle_time = final$idle_time/1000
str(final)


A = c(1, 3, 6, 13, 17, 20, 29, 39, 41, 44, 56, 66, 67, 
      68, 71, 72, 75, 76, 79, 81, 85, 86, 88, 94, 100, 
      104)

B = c(7, 11, 14, 15, 16, 18, 24, 27, 28, 32, 36, 48, 
      53, 63, 70, 77, 78, 87, 91, 93, 96, 98, 106)

C = c(2, 4, 5, 8, 9, 10, 12, 19, 22, 25, 30, 33, 34, 
      37, 38, 42, 45, 46, 47, 49, 51, 52, 54, 55, 57, 58, 
      59, 60, 61, 62, 64, 69, 73, 74, 80, 82, 83, 89, 92, 
      95, 99, 101, 102, 103)

dfB=data.frame()
for (i in B) {
  dfB = rbind(dfB,filter(final, final$student_Id==i))    
}

dfA=data.frame()
for (i in A) {
  dfA = rbind(dfA,filter(final, final$student_Id==i))    
}

dfC=data.frame()
for (i in C) {
  dfC = rbind(dfC,filter(final, final$student_Id==i))    
}

dfA$grade = ifelse(dfA$student_Id == 0, "w","A")
dfB$grade = ifelse(dfB$student_Id == 0, "w","B")
dfC$grade = ifelse(dfC$student_Id == 0, "w","C")

combined_3class_df = rbind(dfA, dfB, dfC)

str(combined_3class_df)
#removing activity,student ID and session from the data set
activity = combined_3class_df$activity
combined_3class_df$activity = NULL
student_Id = combined_3class_df$student_Id
combined_3class_df$student_Id = NULL
combined_3class_df$session = NULL
length(unique(combined_3class_df$student_Id))

#converting grade to factors
combined_3class_df$grade = as.factor(combined_3class_df$grade)

library(caret)
tr = createDataPartition(y = combined_3class_df$grade,p = 0.7, list = F)
tr_val = combined_3class_df[tr,]
test = combined_3class_df[-tr,]

tr_rows = createDataPartition(y = tr_val$grade, p = 0.8, list = F)
train = tr_val[tr_rows,]
validation = tr_val[-tr_rows,]

grades_of_test = test$grade
test$grade = NULL

#standardizing the data using Caret preProcess

tr = preProcess(train, method = c('center','scale'))

std_train = predict(tr,train)
std_valid = predict(tr,validation)
std_test = predict(tr,test)


std_test = cbind(std_test, grades_of_test)



#building random forest with two class_without_activity and present students
library(randomForest)
Rf_3class = randomForest(grade~., data = std_train, importance = TRUE)

#predicting on valid
rf_val_std_pred = predict(Rf_3class,std_valid)
library(MLmetrics)
#checking for variable importance
Rf_3class$importance
varImpPlot(Rf_3class)
#accuracy for validation and test
rf_val_std_accu = Accuracy(rf_val_std_pred, std_valid$grade)
rf_val_std_accu

rf_test_std_pred = predict(Rf_3class, std_test)

rf_test_std_accu = Accuracy(rf_test_std_pred, std_test$grades_of_test)
rf_test_std_accu 


####################################################

#this data set contains students who attended all the sessions and three class grades
#clearing the working environment
rm(list = ls(all=TRUE))
#setting the working directory
setwd("E:/Batch-31Internship/practise")

#reading the files
library(readxl)
final = read.csv("actual_present.csv")
#libraries required
library(data.table)
library(lubridate)
library(dplyr)
library(caret)
attach(final)

str(final)
attach(final)
#converting catagorical variables to factors
final$session = as.factor(final$session)
final$exercise = as.factor(final$exercise)
final$activity = as.factor(final$activity)
final$student_Id = as.factor(final$student_Id)
# 
# for(i in 6:12){
#   dev.copy(jpeg, filename = paste(names(final[i]),"plot.jpeg", sep = "_"))
#   par(mfrow = c(1,1))
#   boxplot(final[,i], xlab=names(final[i]))
#   dev.off()
# }
# 

#dividing idle time by 10^3 to convert to seconds

final$idle_time = final$idle_time/1000
str(final)

#segregating the students with respective grades
A = c(1, 3, 6, 13, 17, 20, 29, 39, 41, 44, 56, 66, 67,
      68, 71, 72, 75, 76, 79, 81, 85, 86, 88, 94, 100,
      104)

B = c(7, 11, 14, 15, 16, 18, 24, 27, 28, 32, 36, 48,
      53, 63, 70, 77, 78, 87, 91, 93, 96, 98, 106)

C = c(2, 4, 5, 8, 9, 10, 12, 19, 22, 25, 30, 33, 34,
      37, 38, 42, 45, 46, 47, 49, 51, 52, 54, 55, 57, 58,
      59, 60, 61, 62, 64, 69, 73, 74, 80, 82, 83, 89, 92,
      95, 99, 101, 102, 103)
#adding the Grades Columns to a new data frame df
dfB=data.frame()
for (i in B) {
  dfB = rbind(dfB,filter(final, final$student_Id==i))
}

dfA=data.frame()
for (i in A) {
  dfA = rbind(dfA,filter(final, final$student_Id==i))
}

dfC=data.frame()
for (i in C) {
  dfC = rbind(dfC,filter(final, final$student_Id==i))
}

dfA$grade = ifelse(dfA$student_Id == 0, "w","A")
dfB$grade = ifelse(dfB$student_Id == 0, "w","B")
dfC$grade = ifelse(dfC$student_Id == 0, "w","C")

#combining the three data frames to get a data frame with Students present for all sessions
#and has three grades in the target column
combined_3class_df = rbind(dfA, dfB, dfC)

#########################################################

rm(list = ls(all = TRUE))
setwd("E:/Batch-31Internship/practise")
epm = read.csv("EPM_final.csv")
str(epm)
library(dplyr)
library(caret)
library(lubridate)
attach(epm)
library(cluster)
library(Rtsne)
#Date conversions
epm$start_time = dmy_hms(epm$start_time)
epm$end_time = dmy_hms(epm$end_time)

time_diff  = as.numeric(epm$end_time-epm$start_time)
attach(epm)

epm = cbind(epm, time_diff)
epm$start_time = NULL
epm$end_time = NULL

A = c(1, 3, 6, 7, 11, 13, 14, 15, 16, 17, 18, 20, 24, 
      27, 28, 29, 32, 36, 39, 41, 44, 48, 53, 56, 63, 66, 
      67, 68, 70, 71, 72, 75, 76, 77, 78, 79, 81, 85, 86, 
      87, 88, 91, 93, 94, 96, 98, 100, 104, 106)

df1 = data.frame()
for (i in A) {
  df1 = rbind(df1,filter(epm, epm$student_Id == i))
  print(i)
}

df1$grade = ifelse(df1$student_Id == 0, "w","A")


B = c(2, 4, 5, 8, 9, 10, 12, 19, 22, 25, 30, 33, 34, 
      37, 38, 42, 45, 46, 47, 49, 51, 52, 54, 55, 57, 58, 
      59, 60, 61, 62, 64, 69, 73, 74, 80, 82, 83, 89, 92, 
      95, 99, 101, 102, 103)

df2 = data.frame()
for( i in B){
  df2 = rbind(df2,filter(epm, epm$student_Id == i))
}

df2$grade = ifelse(df2$student_Id == 0, "w","B")

#combined data frame with grades A and B
combined_df = rbind(df1,df2)


combined_df$idle_time = (combined_df$idle_time/1000)
combined_df$session = as.factor(combined_df$session)
combined_df$grade = as.factor(combined_df$grade)
str(combined_df)


Activity = combined_df$activity
combined_df$activity = NULL
student_Id = combined_df$student_Id
combined_df$student_Id = NULL

library(caret)
tr = createDataPartition(y = combined_df$grade,p = 0.7, list = F)
tr_val = combined_df[tr,]
test = combined_df[-tr,]

tr_rows = createDataPartition(y = tr_val$grade, p = 0.8, list = F)
train = tr_val[tr_rows,]
validation = tr_val[-tr_rows,]

grades_of_test = test$grade
test$grade = NULL

#standardizing the data using Caret preProcess

tr = preProcess(train, method = c('center','scale'))

std_train = predict(tr,train)
std_valid = predict(tr,validation)
std_test = predict(tr,test)




#glm with std data, only without student id
logistic = glm(grade~.,std_train, family = binomial)
summary(logistic)

predictions_train = predict(logistic,type = 'response')
prob_std_valid = predict(logistic, std_valid, type = 'response')
prob_std_valid

library(ROCR)

pred = prediction(prob_std_valid, std_valid$grade)
perf = performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.02))



preds_std_valid = ifelse(prob_std_valid > 0.40 , "A", "B")

confusionMatrix(preds_std_valid, std_valid$grade)


library(MLmetrics)
Accuracy_val = Accuracy(preds_std_valid, std_valid$grade)
Accuracy_val
#accuracy = 58.98


prob_test = predict(logistic, std_test, type = 'response')
preds_std_test = ifelse(prob_test > 0.40 , "A", "B")

confusionMatrix(preds_std_test, std_test$grade)
Accuracy_test = Accuracy(preds_std_test, std_test$grade)
Accuracy_test
#accuracy = 58.78

#######################################
# glm without activity for the std data

logistic = glm(grade~.,std_train, family = binomial)
summary(logistic)

predictions_train = predict(logistic,type = 'response')
prob_std_valid = predict(logistic, std_valid, type = 'response')
prob_std_valid

library(ROCR)

pred = prediction(prob_std_valid, std_valid$grade)
perf = performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.02))



preds_std_valid = ifelse(prob_std_valid > 0.40 , "A", "B")

confusionMatrix(preds_std_valid, std_valid$grade)


library(MLmetrics)
Accuracy_val = Accuracy(preds_std_valid, std_valid$grade)
Accuracy_val

#stepaic model without activity for the standardize data

aic1 = stepAIC(logistic)


library(e1071)

svmmodel = svm(std_train$grade~., data = std_train)


##########################################################
rm(list = ls(all = TRUE))
setwd("E:/Batch-31Internship/practise")
epm = read.csv("EPM_final.csv")
str(epm)
library(dplyr)
library(caret)
library(lubridate)
attach(epm)
library(cluster)
library(Rtsne)
#Date conversions
epm$start_time = dmy_hms(epm$start_time)
epm$end_time = dmy_hms(epm$end_time)

time_diff  = as.numeric(epm$end_time-epm$start_time)
attach(epm)

epm = cbind(epm, time_diff)
epm$start_time = NULL
epm$end_time = NULL

epm$session  = paste(epm$session, "_" , epm$exercise, "_", epm$activity)

epm_combined_session = select(epm, -c(exercise,activity))

A = c(1, 3, 6, 7, 11, 13, 14, 15, 16, 17, 18, 20, 24, 
      27, 28, 29, 32, 36, 39, 41, 44, 48, 53, 56, 63, 66, 
      67, 68, 70, 71, 72, 75, 76, 77, 78, 79, 81, 85, 86, 
      87, 88, 91, 93, 94, 96, 98, 100, 104, 106)

df1 = data.frame()
for (i in A) {
  df1 = rbind(df1,filter(epm_combined_session, epm$student_Id == i))
  print(i)
}

df1$grade = ifelse(df1$student_Id == 0, "w","A")


B = c(2, 4, 5, 8, 9, 10, 12, 19, 22, 25, 30, 33, 34, 
      37, 38, 42, 45, 46, 47, 49, 51, 52, 54, 55, 57, 58, 
      59, 60, 61, 62, 64, 69, 73, 74, 80, 82, 83, 89, 92, 
      95, 99, 101, 102, 103)

df2 = data.frame()
for( i in B){
  df2 = rbind(df2,filter(epm_combined_session, epm$student_Id == i))
}

df2$grade = ifelse(df2$student_Id == 0, "w","B")


combined_df = rbind(df1,df2)
write.csv(data.frame(combined_df), 'All_Students_with_grades.csv', quote = F, 
          row.names = F)

combined_df$idle_time = (combined_df$idle_time/1000)
combined_df$session = as.factor(combined_df$session)
combined_df$grade = as.factor(combined_df$grade)
str(combined_df)

Session = combined_df$session
combined_df$session = NULL
student_Id = combined_df$student_Id
combined_df$student_Id = NULL

library(caret)
tr = createDataPartition(y = combined_df$grade,p = 0.7, list = F)
tr_val = combined_df[tr,]
test = combined_df[-tr,]

tr_rows = createDataPartition(y = tr_val$grade, p = 0.8, list = F)
train = tr_val[tr_rows,]
validation = tr_val[-tr_rows,]

grades_of_test = test$grade
test$grade = NULL


#GLM 
logistic = glm(grade~.,train, family = binomial)
summary(logistic)

predictions_train = predict(logistic,type = 'response')
prob_validation = predict(logistic, validation, type = 'response')
prob_validation

library(ROCR)

pred = prediction(prob_validation, validation$grade)
perf = performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.02))


preds_val = ifelse(prob_validation > 0.48 , "A", "B")

library(MLmetrics)
Accuracy_val = Accuracy(preds_val, validation$grade)
Accuracy_val

confusionMatrix(preds_val, validation$grade)

library(MASS)
################################
#stepAIC
saic = stepAIC(logistic)
prob_validation_saic = predict(saic, validation, type = 'response')
preds_val_siac = ifelse(prob_validation_saic > 0.50 , "A", "B")

pred = prediction(prob_validation_saic, validation$grade)
perf = performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.02))


library(MLmetrics)
Accuracy_val_saic = Accuracy(preds_val_saic, validation$grade)
Accuracy_val_saic

confusionMatrix(preds_val_saic, validation$grade)

















































