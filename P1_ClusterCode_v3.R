train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv", header=T)

table(is.na(train)) #NO missing values
table(is.na(test))  #NO missing values

head(train)
View(train)
library(ClustOfVar)
tree <- hclustvar(train[,2:101])
stability(tree,B=100,graph = T)
#Optimal number of cluster is 9 from the plot obtained above

part_hier<-cutreevar(tree,9)
cls<-part_hier$cluster
write.csv(cls,file = "cluster_submit_v3.csv",row.names = T)
#.043 Efficiency