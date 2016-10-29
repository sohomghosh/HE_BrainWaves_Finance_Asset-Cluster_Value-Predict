#Loading the Data
setwd("C:\\Users\\SatyakiBh\\Desktop\\BrainWaves-Hackathon")
train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv", header=T)

#Checking if TS exists



#Preprocessing
  #Checking the Information Gain
library(FSelector)
tr_n<-train[,2:102]
weights <- information.gain(Y~., data=tr_n)
subset <- cutoff.k(weights, 2)
print(weights)
print(subset)

  #Checking variable importance by Random Forest
library(randomForest)
rf<-randomForest(tr_n[,1:100],as.factor(tr_n$Y),importance=T)
rf$importance

#PCA
pca<-prcomp(tr_n[,1:100],center = T, scale = T)
summary(pca)

###PC1 to PC3 stores 99% of the variance

'''
data(iris)
weights <- information.gain(Species~., iris)
print(weights)
subset <- cutoff.k(weights, 2)
f <- as.simple.formula(subset, "Species")
print(f)
'''