#Loading the Data
setwd("C:\\Users\\SatyakiBh\\Desktop\\BrainWaves-Hackathon")
train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv", header=T)
library(forecast)

#Preprocessing with Y
train_ma<-ma(train,5)
test_ma<-ma(test,5)
train_ma_na <- na.omit(train_ma)
test_ma_na <- na.omit(test_ma)


#Preprocessing without Y
train_ma_withoutY<-ma(train[,1:101],5)
test_ma_withoutY<-ma(test[1:101],5)
