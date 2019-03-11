setwd("~/RF_build")
load("model_noHema_weight.Rdata")
library(missForest)
library(randomForest)
library(pROC)

set.seed(1)
#week8
names(dat_allCRP)=make.names(names(dat_allCRP))
rf_allCRP = randomForest(CRP_response~.,data=dat_allCRP[,-1])

rfmodel=rf_allCRP
rf_imp = rfmodel$importance
varnames = row.names((rf_imp))
impdata=dat_allCRP[,varnames]
length_v = rep(0,length(rf_imp))  ##store length for each variable
uniqueV=list()  ##store list of values
for(i in 1:length(length_v)){
  uniqueV[[i]]=unique(impdata[,i])
  length_v[i]=length(uniqueV[[i]])
  if(length_v[i]>50){
    uniqueV[[i]]=quantile(impdata[,i],seq(0.01,0.99,0.02))
    length_v[i]=length(uniqueV[[i]])
  }
}
est = list()
for(i in 1:length(length_v)){
  print(i)
  est[[i]]=rep(0,length_v[i])
  for(j in 1:length_v[i]){
    newdata_j = impdata
    newdata_j[,varnames[i]]=uniqueV[[i]][j]
    est[[i]][j]=mean(predict(rfmodel,newdata_j,type="prob")[,2])
    gc()
  }
}
plot(uniqueV[[i]],est[[i]],xlab=varnames[i],ylab="Success Probability")
save(est,length_v,uniqueV,varnames,file="week8partialplot_noHema_weight.Rdata")

#week0
dat_week0=na.omit(dat_week0)
names(dat_week0)=make.names(names(dat_week0))
rf_week0 = randomForest(CRP_response~.,data=dat_week0[,-1])

rfmodel=rf_week0
rf_imp = rfmodel$importance
varnames = row.names((rf_imp))
impdata=dat_week0[,varnames]
length_v = rep(0,length(rf_imp))  ##store length for each variable
uniqueV=list()  ##store list of values
for(i in 1:length(length_v)){
  uniqueV[[i]]=unique(impdata[,i])
  length_v[i]=length(uniqueV[[i]])
  if(length_v[i]>50){
    uniqueV[[i]]=quantile(impdata[,i],seq(0.01,0.99,0.02))
    length_v[i]=length(uniqueV[[i]])
  }
}
est = list()
for(i in 1:length(length_v)){
  print(i)
  est[[i]]=rep(0,length_v[i])
  for(j in 1:length_v[i]){
    newdata_j = impdata
    newdata_j[,varnames[i]]=uniqueV[[i]][j]
    est[[i]][j]=mean(predict(rfmodel,newdata_j,type="prob")[,2])
    gc()
  }
}

save(est,length_v,uniqueV,varnames,file="week0partialplot_noHema_weight.Rdata")
