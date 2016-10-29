train_test_data <- read.csv("C:/Users/HP-PC/Desktop/brainwaves/train.csv")
sub_data <- read.csv("C:/Users/HP-PC/Desktop/brainwaves/test.csv")

data = train_test_data
data$Y=NULL
data = rbind(data,sub_data)


#ADDING LAG VARIABE
library(forecast)


newdt<-as.data.frame(ma(data[,-ncol(data)],5))
for(var in names(newdt[1:100]))
{ 
  newdt <- slide(newdt, Var = var, slideBy = -2)
}
newdt[1:4,] = newdt[5,]
newdt = newdt[,101:200]


library(DataCombine)
for(var in names(data[2:101]))
{ 
  data <- slide(data, Var = var, slideBy = -1)
  data <- slide(data, Var = var, slideBy = 1)
  data <- slide(data, Var = var, slideBy = 2)
  data <- slide(data, Var = var, slideBy = -2)
}
data[1,]=data[2,]

data = cbind(data,newdt)

# newdt<-as.data.frame(ma(data[,-ncol(data)],5))
# for(var in names(newdt[1:100]))
# { 
#   newdt <- slide(newdt, Var = var, slideBy = +2)
# }
# newdt[nrow(newdt),] = newdt[nrow(newdt)-4,]
# newdt[nrow(newdt)-1,] = newdt[nrow(newdt)-4,]
# newdt[nrow(newdt)-2,] = newdt[nrow(newdt)-4,]
# newdt[nrow(newdt)-3,] = newdt[nrow(newdt)-4,]




sub_data = rbind(sub_data$Time,data[(nrow(train_test_data)+1):nrow(newdt),])
data = data[1:nrow(train_test_data),]

data$Time = NULL
data$Y = ifelse(train_test_data$Y!=1,0,1)


library(caTools)
set.seed(101)
sample = sample.split(data, SplitRatio = .75)
train = subset(data, sample == TRUE)
test = subset(data, sample == FALSE)

require(gbm)
require(dplyr)

library(FSelector)
tr_n<-data
weights <- information.gain(Y~., data=tr_n)
subset <- cutoff.k(weights, 90)
print(weights)
print(subset)



ntrees = 5000

# new_var = as.character(summary(Model)$var[1:40])
new_var = subset
train = train[,c('Y',new_var)]
test = test[,c('Y',new_var)]
sub_data = sub_data[,c('Time',new_var)]

data = train

Model = gbm( 
  Y~. #dataframe of features
  , data=data
  , distribution = "bernoulli"
  , n.trees = 500
  , shrinkage = 0.001 
  , interaction.depth = 5
  ,cv.folds=10
  , n.minobsinnode = 5
  , verbose = TRUE 
)  

cumsum(summary(Model)$rel)

library(stringr)
new_var = as.character(summary(Model)$var)[1:60]
train = train[,c('Y',new_var)]
test = test[,c('Y',new_var)]
sub_data = sub_data[,c('Time',new_var)]





TrainPredictions = predict(object = Model,newdata =train
                           , n.trees = gbm.perf(Model, plot.it = FALSE)
                           , type = "response")

pred = TrainPredictions
acc = mean((pred>quantile(pred,0.5))==(train$Y==1))
#predction accuracy
table(pred>quantile(pred,0.5),train$Y==1)
acc
score = 6*abs(acc-0.5)
#prediciton score
score



TestPredictions = predict(object = Model,newdata =test
                          , n.trees = gbm.perf(Model, plot.it = FALSE)
                          #, n.trees = 50
                          , type = "response")

pred =   TestPredictions
acc = mean((pred>quantile(pred,0.5))==(test$Y==1))
#predction accuracy
table(pred>quantile(pred,0.5),test$Y==1)
acc
score = 6*abs(acc-0.5)
#prediciton score
score

gbm_pred_save = pred


SubPredictions = predict(object = Model,newdata =sub_data
                         , n.trees = gbm.perf(Model, plot.it = FALSE)
                         #, n.trees = 50
                         , type = "response")

pred =  SubPredictions
Y=(pred>quantile(pred,0.5))
Y = ifelse(Y==TRUE,1,-1)
submit = data.frame(Time = sub_data$Time,Y=Y)
submit = submit[-1,]
submit = submit[-nrow(submit),]
write.csv(submit,'Growth1_select_gbm_n4.csv',row.names = F)


for(i in 1:length(Model$var.names)){
  plot(Model, i.var = i
       , ntrees = gbm.perf(Model, plot.it = FALSE) #optimal number of trees
       , type = "response" #to get fitted probabilities
  )
}


