data <- read.csv("C:/Users/HP-PC/Desktop/brainwaves/train.csv")
sub_data <- read.csv("C:/Users/HP-PC/Desktop/brainwaves/test.csv")

library(caTools)
set.seed(101) 
sample = sample.split(data, SplitRatio = .75)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)


# library('TTR')
# train_x = lapply(train,function(x) ts(x))
# test_x = lapply(test,function(x) ts(x))
#tr_3 = lapply(train_x,function(x) SMA(x,n=3))


library('nnet')
library('caret')

# #Pre-processing
# train$Y = ifelse(train$Y==1,'high','low')
# test$Y = ifelse(test$Y==1,'high','low')
# train$lag.1 = (lag(train$Y,1))
# train$lag.2 = (lag(train$Y,2))
# train$lag.3 = (lag(train$Y,3))
# train$lag.4 = (lag(train$Y,4))
# train = complete.cases(train)
# test$lag.1 = (lag(test$Y,1))
# test$lag.2 = (lag(test$Y,2))
# test$lag.3 = (lag(test$Y,3))
# test$lag.4 = (lag(test$Y,4))
# test = complete.cases(test)
library('e1071')
library('nnet')
library('kernlab')
# type="C-classification" ##regression
# u= -3 ## -3,-2,-1,0,1,2,3
# gam=10^{u}; w= 0 ##1.5,-1,0.5,2,3,4
# cost=10^{w}
# ##The higher the cost produce less support vectors, increases accuracy
# ##However we may overfit
# svmFit = svm (train[,c(2:10)], train[,(ncol(train))],
#               type=type,
#               kernel= "radial",
#               gamma=gam,
#               cost=cost
# )
# summary(svmFit)
# pred_train_svm = predict(svmFit, train[,c(2:10)])
# pred_test_svm = predict(svmFit, test[,c(2:10)])
# 
# 
# pred = pred_train_svm
# acc = mean(pred==train$Y)
# #predction accuracy
# table(pred,train$Y)
# acc
# score = 6*abs(acc-0.5)
# #prediciton score
# score
# 
# pred = pred_test_svm
# acc = mean(pred==test$Y)
# #predction accuracy
# table(pred,test$Y)
# acc
# score = 6*abs(acc-0.5)
# #prediciton score
# score


##A nnet with size hidden layers +skip layer. Max iteration 10^4,

size=1

# train$Y = factor(train$Y)
nnetFit = nnet(train[,-ncol(train)], train[,ncol(train)],
               size=size,skip=T, maxit=10^4,decay=10^{-2},trace=F,linout=T)
#summary(nnetFit) ##gives description w/weights

##build NNET predictor type="raw"
pred_train_nnet_1 = predict(nnetFit,train[,-ncol(train)],type="raw")
pred_test_nnet_1 = predict(nnetFit,test[,-ncol(test)],type="raw")
pred_sub_nnet_1 = predict(nnetFit,sub_data,type="raw")


size=2

# train$Y = factor(train$Y)
nnetFit = nnet(train[,-ncol(train)], train[,ncol(train)],
               size=size,skip=T, maxit=10^4,decay=10^{-2},trace=F,linout=T)
#summary(nnetFit) ##gives description w/weights

##build NNET predictor type="raw"
pred_train_nnet_2 = predict(nnetFit,train[,-ncol(train)],type="raw")
pred_test_nnet_2 = predict(nnetFit,test[,-ncol(test)],type="raw")
pred_sub_nnet_2 = predict(nnetFit,sub_data,type="raw")


size=3

nnetFit = nnet(train[,-ncol(train)], train[,ncol(train)],
               size=size,skip=T, maxit=10^4,decay=10^{-2},trace=F,linout=T)
summary(nnetFit) ##gives description w/weights

##build NNET predictor type="raw"
pred_train_nnet_3 = predict(nnetFit,train[,-ncol(train)],type="raw")
pred_test_nnet_3 = predict(nnetFit,test[,-ncol(test)],type="raw")
pred_sub_nnet_3 = predict(nnetFit,sub_data,type="raw")

size=4

nnetFit = nnet(train[,-ncol(train)], train[,ncol(train)],
               size=size,skip=T, maxit=10^4,decay=10^{-2},trace=F,linout=T)
summary(nnetFit) ##gives description w/weights

##build NNET predictor type="raw"
pred_train_nnet_4 = predict(nnetFit,train[,-ncol(train)],type="raw")
pred_test_nnet_4 = predict(nnetFit,test[,-ncol(test)],type="raw")
pred_sub_nnet_4 = predict(nnetFit,sub_data,type="raw")


size=5

nnetFit = nnet(train[,-ncol(train)], train[,ncol(train)],
               size=size,skip=T, maxit=10^4,decay=10^{-2},trace=F,linout=T)
##gives description w/weights

##build NNET predictor type="raw"
pred_train_nnet_5 = predict(nnetFit,train[,-ncol(train)],type="raw")
pred_test_nnet_5 = predict(nnetFit,test[,-ncol(test)],type="raw")
pred_sub_nnet_5 = predict(nnetFit,sub_data,type="raw")


size=6

nnetFit = nnet(train[,-ncol(train)], train[,ncol(train)],
               size=size,skip=T, maxit=10^4,decay=10^{-2},trace=F,linout=T)
##gives description w/weights

##build NNET predictor type="raw"
pred_train_nnet_6 = predict(nnetFit,train[,-ncol(train)],type="raw")
pred_test_nnet_6 = predict(nnetFit,test[,-ncol(test)],type="raw")
pred_sub_nnet_6 = predict(nnetFit,sub_data,type="raw")


size=7

nnetFit = nnet(train[,-ncol(train)], train[,ncol(train)],
               size=size,skip=T, maxit=10^4,decay=10^{-2},trace=F,linout=T)
##gives description w/weights

##build NNET predictor type="raw"
pred_train_nnet_7 = predict(nnetFit,train[,-ncol(train)],type="raw")
pred_test_nnet_7 = predict(nnetFit,test[,-ncol(test)],type="raw")
pred_sub_nnet_7 = predict(nnetFit,sub_data,type="raw")

size=8

nnetFit = nnet(train[,-ncol(train)], train[,ncol(train)],
               size=size,skip=T, maxit=10^4,decay=10^{-2},trace=F,linout=T)
##gives description w/weights

##build NNET predictor type="raw"
pred_train_nnet_8 = predict(nnetFit,train[,-ncol(train)],type="raw")
pred_test_nnet_8 = predict(nnetFit,test[,-ncol(test)],type="raw")
pred_sub_nnet_8 = predict(nnetFit,sub_data,type="raw")

library(ts)



pred_train_nnet_mode = rowMeans(cbind(pred_train_nnet_2,pred_train_nnet_3,pred_train_nnet_4,pred_train_nnet_6))
pred_test_nnet_mode = rowMeans(cbind(pred_test_nnet_2,pred_test_nnet_3,pred_test_nnet_4,pred_test_nnet_6))
pred_sub_nnet_mode = rowMeans(cbind(pred_sub_nnet_2,pred_sub_nnet_3,pred_sub_nnet_4,pred_sub_nnet_6))

# names(sort(-table(pred_train_nnet)))[1]

pred = pred_train_nnet_4
acc = mean((pred>quantile(pred,0.5))==(train$Y==1))
#predction accuracy
table(pred>quantile(pred,0.5),train$Y==1)
acc
score = 6*abs(acc-0.5)
#prediciton score
score

pred = pred_test_nnet_8
acc = mean((pred>quantile(pred,0.5))==(test$Y==1))
#predction accuracy
table(pred>quantile(pred,0.5),test$Y==1)
acc
score = 6*abs(acc-0.5)
#prediciton score
score

pred_test_nnet_mode = rowMeans(cbind(0
                                      #,0.06684142*pred_test_nnet_2
                                     #,-0.04325033*pred_test_nnet_3
                                    ,-0.129751*pred_test_nnet_4
                                    #,0.05111402*pred_test_nnet_5
                                     ,0.1690695*pred_test_nnet_7))
pred_sub_nnet_mode = rowMeans(cbind(0
                                    #,0.06684142*pred_sub_nnet_2
                                  #  ,-0.04325033*pred_sub_nnet_3
                                   ,-0.129751*pred_sub_nnet_4
                                   # ,0.05111402*pred_sub_nnet_5
                                    ,0.1690695*pred_sub_nnet_7))
pred1 = pred_test_nnet_6
pred2 = pred_test_nnet_2
table((pred1>quantile(pred1,0.5)),(pred2>quantile(pred2,0.5)))


pred = pred_test_nnet_6
acc = mean((pred>quantile(pred,0.5))==(test$Y==1))
#predction accuracy
table(pred>quantile(pred,0.5),test$Y==1)
acc
score = 6*abs(acc-0.5)
#prediciton score
score

pred=pred_sub_nnet_6
Y=(pred>quantile(pred,0.5))
Y = ifelse(Y==TRUE,1,-1)
submit = data.frame(Time = sub_data$Time,Y=Y)
write.csv(submit,'Growth1.csv',row.names = F)

# library(rnn)
# # Create 3d array: dim 1: samples; dim 2: time; dim 3: variables.
# 
# X = array(train[,-ncol(train)],)
# model.rnn = trainr()

