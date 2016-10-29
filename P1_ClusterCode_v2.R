setwd("C:\\Users\\SatyakiBh\\Desktop\\BrainWaves-Hackathon")
train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv", header=T)
library(ClustOfVar)
km<-kmeansvar(X.quanti = train[,2:101], init=9,matsim = TRUE)#, iter.max = 150, nstart = 1, 
km_cls<-km$cluster
#km_cls_mat<-matrix(km_cls,nrow=100,ncol=1,dimnames = list( c(names(km_cls)),c("Cluster")))
write.csv(km_cls,file = "cluster_submit_v2.csv",row.names = T)
#0.029 Eficiency