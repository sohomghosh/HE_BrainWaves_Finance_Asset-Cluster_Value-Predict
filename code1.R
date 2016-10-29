# Matching submitted outputs

setwd('C:/Users/HP-PC/Desktop/Knocktober/Train')
camp_1 <- read.csv("First_Health_Camp_Attended.csv")
camp_1$X = NULL
camp_2 <- read.csv("Second_Health_Camp_Attended.csv")
camp_3 <- read.csv("Third_Health_Camp_Attended.csv")
camps <- read.csv("Health_Camp_Detail.csv")
patients <- read.csv("Patient_Profile.csv")
data_init <- read.csv("Train.csv")
sub_init <- read.csv("Test.csv")

data = merge(data_init,camps,'Health_Camp_ID')
data = merge(data,patients,'Patient_ID')
data = merge(data,camp_1,c('Health_Camp_ID','Patient_ID'),all.x = T)
data = merge(data,camp_2,c('Health_Camp_ID','Patient_ID'),all.x = T)
data = merge(data,camp_3,c('Health_Camp_ID','Patient_ID'),all.x = T)
data$Registration_Date = as.numeric(as.Date(data$Registration_Date,format='%d-%B-%y'))
data$Camp_Start_Date = as.numeric(as.Date(data$Camp_Start_Date,format='%d-%B-%y'))
data$Camp_End_Date = as.numeric(as.Date(data$Camp_End_Date,format='%d-%B-%y'))
data$First_Interaction = as.numeric(as.Date(data$First_Interaction,format='%d-%B-%y'))
data$Donation = NULL
data$Last_Stall_Visited_Number = NULL

sub_data = merge(sub_init,camps,'Health_Camp_ID')
sub_data = merge(sub_data,patients,'Patient_ID')
sub_data = merge(sub_data,camp_1,c('Health_Camp_ID','Patient_ID'),all.x = T)
sub_data = merge(sub_data,camp_2,c('Health_Camp_ID','Patient_ID'),all.x = T)
sub_data = merge(sub_data,camp_3,c('Health_Camp_ID','Patient_ID'),all.x = T)
sub_data$Registration_Date = as.numeric(as.Date(sub_data$Registration_Date,format='%d-%B-%y'))
sub_data$Camp_Start_Date = as.numeric(as.Date(sub_data$Camp_Start_Date,format='%d-%B-%y'))
sub_data$Camp_End_Date = as.numeric(as.Date(sub_data$Camp_End_Date,format='%d-%B-%y'))
sub_data$First_Interaction = as.numeric(as.Date(sub_data$First_Interaction,format='%d-%B-%y'))
sub_data$Donation = NULL
sub_data$Last_Stall_Visited_Number = NULL


library(caTools)
set.seed(101) 
sample = sample.split(data, SplitRatio = .75)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)

train_score1 = as.numeric(!is.na(train$Health_Score))
train_score2 = as.numeric(!is.na(train$Health.Score))
# train_score1 = as.numeric(is.na(train$Health_Score))
# train_score2 = as.numeric(is.na(train$Health.Score))
train_score3 = train$Number_of_stall_visited
train_score3[is.na(train_score3)] = 0

train$Health_Score=NULL
train$Health.Score=NULL
train$Number_of_stall_visited=NULL



test_score1 = as.numeric(!is.na(test$Health_Score))
test_score2 = as.numeric(!is.na(test$Health.Score))
# test_score1 = as.numeric(is.na(test$Health_Score))
# test_score2 = as.numeric(is.na(test$Health.Score))
test_score3 = test$Number_of_stall_visited
test_score3[is.na(test_score3)] = 0

test$Health_Score=NULL
test$Health_Score=NULL
test$Number_of_stall_visited=NULL

# #NA values
# 
# #Registration Date:
# plot(sort(data$Registration_Date))
# plot(sort(data$Registration_Date))
# #shows no missing time
##############################################################33
library(gbm)
ntrees=1500

Model1 = gbm( 
  Y~. #dataframe of features
  , data=cbind(train,Y=train_score1)
  , distribution = "bernoulli"
  , n.trees =ntrees
  , shrinkage = 0.001 
  , interaction.depth = 5
  ,cv.folds=3
  , n.minobsinnode = 5
  , verbose = TRUE 
)  

gbm.perf(Model1)

library('AUC')
li


train_pred_1 = predict(object = Model1,newdata =train
                           , n.trees = gbm.perf(Model, plot.it = FALSE)
                           , type = "response")

pred = train_pred_1
train_auc_1= auc(roc(pred,factor(train_score1)))

test_pred_1 = predict(object = Model1,newdata =test
                          , n.trees = gbm.perf(Model, plot.it = FALSE)
                          #, n.trees = 50
                          , type = "response")

pred =   test_pred_1
test_auc_1=auc(roc(pred,factor(test_score1)))

sub_pred_1 = predict(object = Model1,newdata =sub_data
                      , n.trees = gbm.perf(Model, plot.it = FALSE)
                      #, n.trees = 50
                      , type = "response")


######################################################
library(gbm)
Model2 = gbm( 
  Y~. #dataframe of features
  , data=cbind(train,Y=train_score2)
  , distribution = "bernoulli"
  , n.trees =ntrees
  , shrinkage = 0.001 
  , interaction.depth = 5
  ,cv.folds=3
  , n.minobsinnode = 5
  , verbose = TRUE 
)  

gbm.perf(Model2)



train_pred_2 = predict(object = Model2,newdata =train
                           , n.trees = gbm.perf(Model, plot.it = FALSE)
                           , type = "response")

pred = train_pred_2
train_auc_2= auc(roc(pred,factor(train_score2)))

test_pred_2 = predict(object = Model2,newdata =test
                          , n.trees = gbm.perf(Model, plot.it = FALSE)
                          #, n.trees = 50
                          , type = "response")

pred =   test_pred_2
test_auc_2 = auc(roc(pred,factor(test_score2)))

sub_pred_2 = predict(object = Model2,newdata =sub_data
                     , n.trees = gbm.perf(Model, plot.it = FALSE)
                     #, n.trees = 50
                     , type = "response")

#####################################################3


Model3 = gbm( 
  Y~. #dataframe of features
  , data=cbind(train,Y=as.numeric(train_score3!=0))
  , distribution = "bernoulli"
  , n.trees =ntrees
  , shrinkage = 0.001 
  , interaction.depth = 5
  ,cv.folds=3
  , n.minobsinnode = 5
  , verbose = TRUE 
)  

gbm.perf(Model3)



train_pred_3 = predict(object = Model3,newdata =train
                       , n.trees = gbm.perf(Model, plot.it = FALSE)
                       , type = "response")

pred = train_pred_3
train_auc_3=auc(roc(pred,factor(train_score3!=0)))

test_pred_3 = predict(object = Model3,newdata =test
                      , n.trees = gbm.perf(Model, plot.it = FALSE)
                      #, n.trees = 50
                      , type = "response")

pred =   test_pred_3
test_auc_3=auc(roc(pred,factor(test_score3!=0)))

sub_pred_3 = predict(object = Model3,newdata =sub_data
                     , n.trees = gbm.perf(Model, plot.it = FALSE)
                     #, n.trees = 50
                     , type = "response")

pred_test_max=apply(cbind(test_pred_1,test_pred_2,test_pred_3),1,max)
final_labels = as.numeric((test_score3!=0) | !(test_score2) | !(test_score1))
final_auc_3=auc(roc(pred_test_max,final_labels))



pred_final_max=apply(cbind(sub_pred_1,sub_pred_2,sub_pred_3),1,max)
submit_max=data.frame(Patient_ID=sub_data$Patient_ID,Health_Camp_ID=sub_data$Health_Camp_ID,Outcome=pred_final_max)
write.csv(submit_max,'max_1500.csv',row.names = F)


pred_final_min=apply(cbind(sub_pred_1,sub_pred_2,sub_pred_3,0),1,min)
submit_min=data.frame(Patient_ID=sub_data$Patient_ID,Health_Camp_ID=sub_data$Health_Camp_ID,Outcome=pred_final_min)
write.csv(submit_min,'min_1500.csv',row.names = F)

pred_final_mean=apply(cbind(sub_pred_1,sub_pred_2,sub_pred_3),1,mean)
submit_mean=data.frame(Patient_ID=sub_data$Patient_ID,Health_Camp_ID=sub_data$Health_Camp_ID,Outcome=pred_final_mean)
write.csv(submit_mean,'mean_1500.csv',row.names = F)

