setwd("C:\\Users\\SatyakiBh\\Desktop\\BrainWaves-Hackathon")
train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv", header=T)

table(is.na(train)) #NO missing values
table(is.na(test))  #NO missing values

head(train)
View(train)
library(ClustOfVar)
tree <- hclustvar(train[,2:101])
#stability(tree,B=100,graph = T)
st<-stability(tree,B=100)

#Number of cluster 5 to 15
part_hier<-cutreevar(tree,9)
cls<-part_hier$cluster
cls_mat<-as.matrix(cls,nrow=100,ncol=2)
cls_mat<-matrix(cls,nrow=100,ncol=2,dimnames = list( c(),c("Asset","Cluster")))
write.csv(cls_mat,file = "cluster_submit_v1.csv",row.names = F)
#write.table(cls_mat,file="cluster_submit_v1.csv",col.names = c("Asset","Cluster"),sep=",")

part_hier$wss

part_hier$var$"cluster1"
part_hier$var$"cluster2"
part_hier$var$"cluster3"


km<-kmeansvar(X.quanti = train[,2:101], init=9,matsim = TRUE)#, iter.max = 150, nstart = 1, 

km$cluster
km$var
km$coef
km$sim
km$wss
km$E
km$size
km$k
km$iter

km_cls<-km$cluster
#km_cls_mat<-as.matrix(km_cls,nrow=100,ncol=2)
km_cls_mat<-matrix(km_cls,nrow=100,ncol=2,dimnames = list( c(),c("Asset","Cluster")))
write.csv(km_cls_mat,file = "cluster_submit_v2.csv",row.names = F)


library(mcclust)
arandi(cl1, cl2, adjust = TRUE)
Arguments
cl1,cl2 vectors of cluster memberships (need to have the same lengths).
adjust logical. Should index be adjusted? Defaults to TRUE


tr<-train[,2:101]
crr<-cor(tr,use="all.obs",method="pearson")
Considering correlation matrix as proximity matrix run clustering algorithm
#Highly correlated variables cluster together

'''
library(caret)
vr<-findCorrelation(crr, cutoff = .90, verbose = FALSE)

#Asset,Cluster
X1,6
X2,6
X3,7
X4,2
X5,9
'''

#library(randomForest)
#rf<-randomForest(train[,2:101],train[102],)
