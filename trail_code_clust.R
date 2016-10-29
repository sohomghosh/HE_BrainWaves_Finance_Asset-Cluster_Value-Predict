data <- read.csv("C:/Users/HP-PC/Desktop/brainwaves/train.csv")
data<-read.csv("train.csv")
data$Y=NULL
data$Time=NULL
sub_data <- read.csv("C:/Users/HP-PC/Desktop/brainwaves/test.csv")
sub_data <- read.csv("test.csv")
sub_data$Time = NULL
data = rbind(data,sub_data)

tb = function(nu) {
  tb<-matrix(rep.int(0,length(nu)*length(nu)),nrow=length(nu),ncol=length(nu),dimnames = )
  rownames(tb)<-names(nu)
  colnames(tb)<-names(nu)
  for (i in 1:length(nu)){
    j=as.numeric(nu[i])
    tb[j,i]=1
    tb[i,j]=1
  }
  
  diag(tb)<-1
  
  tb
}

clust1 = (hclust(dist(abs(cor(na.omit(data)))), method="complete"))
clust2 = (hclust(dist(abs(cor(na.omit(data)))),method = "ward.D"))
clust3 = (hclust(dist(abs(cor(na.omit(data)))),method = "ward.D2"))
clust4 = (hclust(dist(abs(cor(na.omit(data)))),method = "single"))
clust5 = (hclust(dist(abs(cor(na.omit(data)))),method = "average"))
clust6 = (hclust(dist(abs(cor(na.omit(data)))),method = "mcquitty"))
clust7 = (hclust(dist(abs(cor(na.omit(data)))),method = "median"))
clust8 = (hclust(dist(abs(cor(na.omit(data)))),method = "centroid"))



dist_comp =  function(clust) {
  c5 = cutree(clust,k=5)
  c6 = cutree(clust,k=6)
  c7 = cutree(clust,k=7)
  c8 = cutree(clust,k=8)
  c9 = cutree(clust,k=9)
  c10 = cutree(clust,k=10)
  c11 = cutree(clust,k=11)
  c12 = cutree(clust,k=12)
  c13 = cutree(clust,k=13)
  c14 = cutree(clust,k=14)
  c15 = cutree(clust,k=15)
  x=tb(c5)*5+tb(c6)*6+tb(c7)*7+tb(c8)*8+tb(c9)*9+tb(c10)*10+tb(c11)*11+tb(c12)*12+tb(c13)*13+tb(c14)*14+tb(c15)*15
  x}

dist_final = dist_comp(clust1)+dist_comp(clust2)+dist_comp(clust3)+dist_comp(clust4)+dist_comp(clust5)
dist_final = dist_final+dist_comp(clust6)+dist_comp(clust7)+dist_comp(clust8)
# +dist_comp(clust9)+dist_comp(clust10)
# dist_final = dist_final+dist_comp(clust11)+dist_comp(clust12)+dist_comp(clust13)+dist_comp(clust14)+dist_comp(clust15)


k=9
method="single"
x=(hclust(dist(dist_final),method=method))
c = (cutree(x,k=k))
lapply(1:k,function(x) names(c)[c==x])

plot(as.dendrogram(x), cex = 0.6, main = "CPGI-based Ward(mean) Clustering showing 5 clusters")
rect.hclust(x, k = k, border = c("cyan"))

submit = data.frame(Asset=names(c),Cluster=as.numeric(c))
write.csv(submit,"Clust1.csv",row.names = F)
library(ClustOfVar)


