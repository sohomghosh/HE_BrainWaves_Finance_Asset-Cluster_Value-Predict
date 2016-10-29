setwd("C:\\Users\\SatyakiBh\\Desktop\\BrainWaves-Hackathon")
train<-read.csv("train.csv",header = T)
test<-read.csv("test.csv", header=T)

ti<-ts(train[,102])

library(fAssets)
assetsReturnPlot(ti) #Displays time series of individual assets
assetsCumulatedPlot(ti) #Displays time series of individual assets
assetsSeriesPlot(ti) #Displays time series of individual assets


assetsSelect(train,method = c("hclust","kmeans")) #Selects similar or dissimilar assets
.hclustSelect Selects due to hierarchical clustering
.kmeansSelect Selects due to k-means clustering

assetsArrange(train, method = c("pca", "hclust"))
hclustArrange(x, method = c("euclidean", "complete"), ...)

#assets modelling
assetsFit(x, method = c("st", "sn", "sc"),
          title=NULL, description=NULL, fixed.df=NA, ...)
assetsSim(n, method=c("st", "sn", "sc"),
          model=list(beta=rep(0, 2), Omega=diag(2), alpha=rep(0, 2), nu=4),
          assetNames=NULL)



#assets selection
assetsSelect(, method = c("hclust", "kmeans"), control = NULL, ...)

