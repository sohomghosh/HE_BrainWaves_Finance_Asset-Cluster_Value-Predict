
#NOTE: Not using Clustering output from part-1 as we don't know how good clusters are they

#Loading the Data
setwd("C:\\Users\\SatyakiBh\\Desktop\\BrainWaves-Hackathon")
train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv", header=T)

##Checking if TS exists
#NO TS EXIST -> Checked


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



tr<-ma(train,2)

#ADDING LAG VARIABE
library(DataCombine)
DataSlid1 <- slide(train, Var = "Y", slideBy = -2)

#MODELING CODE TO BE ADDED HERE


'''
###NO NEED #####PCA
pca<-prcomp(tr_n[,1:100],center = T, scale = T)
summary(pca)
'''