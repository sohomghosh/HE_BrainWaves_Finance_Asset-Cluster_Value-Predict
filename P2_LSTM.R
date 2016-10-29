setwd("E:\\MATHLogic\\DeepLearning_TensorFlow\\Piyusha_RNN_LSTM")
train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv", header=T)
library(rnn)
library(DataCombine)
DataSlid1 <- slide(train, Var = "X69", slideBy = +1)
DataSlid2 <- slide(DataSlid1,Var="X78", slideBy = +1)
DataSlid3 <- slide(DataSlid2,Var="Y",slideBy=+1)

Slidtrain<-na.omit(DataSlid3)

# create 3d array: dim 1: samples; dim 2: time; dim 3: variables
X <- array(c(Slidtrain[,2],Slidtrain[,3],Slidtrain[,5],Slidtrain[,6]), dim=c(nrow(Slidtrain),1,2) ) #1 as sliding by 1 timestamp ; 2 as columns : X69 and X78

#X<-Slidtrain[,c()]

Y<-array(c(Slidtrain[,4],Slidtrain[,7]),dim=c(nrow(Slidtrain),1,2))

# train the model
model <- trainr(Y=Y,X=X,learningrate   =  0.1, hidden_dim     = 10)


######TESTING THE MODEL###
DataSlid1_test <- slide(test, Var = "X69", slideBy = +1)
DataSlid2_test <- slide(DataSlid1_test,Var="X78", slideBy = +1)


Slidtest<-na.omit(DataSlid2_test)

# create 3d array: dim 1: samples; dim 2: time; dim 3: variables
X <- array(c(Slidtest[,2],Slidtest[,3],Slidtest[,4],Slidtest[,5]), dim=c(nrow(Slidtest),1,2) ) #1 as sliding by 1 timestamp ; 2 as columns : X69 and X78

predictr(model, X)
