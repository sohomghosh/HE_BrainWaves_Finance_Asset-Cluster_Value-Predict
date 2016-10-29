nu<-c(1,2,3,1,1,2)
names(nu)<-c("x1","x2","x3","x4","x5","x6")

tb<-matrix(rep.int(0,length(nu)*length(nu)),nrow=length(nu),ncol=length(nu),dimnames = )
rownames(tb)<-names(nu)
colnames(tb)<-names(nu)
for (i in 1:length(nu)){
  j=as.numeric(nu[i])
  tb[j,i]=1
  tb[i,j]=1
}
diag(tb)<-1
