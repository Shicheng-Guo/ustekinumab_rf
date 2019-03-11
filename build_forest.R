
library(missForest)
library(randomForest)
library(pROC)

setwd("~/raw_tables")
qs=read.csv("qs.csv")
qs = qs[qs$QSTEST=="Weight",]
qs$DUSUBJID = sapply(qs$DUSUBJID,substr,start=17,stop=30)
setwd("~/RF_build")
load("dat_new_highCRP.Rdata")
dat_new=dat_new_highCRP
qs$QSDY=floor(qs$QSDY/7)
qs = qs[qs$QSDY==0,]

##only week 8 measurements
## extract the variables for week 8
uniquevariables=c()
for(variable_name in names(dat_new)){
  index = which(names(dat_new)==variable_name)
  truename = strsplit(variable_name,"_")[[1]][1]
  if(truename==variable_name){
    uniquevariables=c(uniquevariables,variable_name)
  }
  else{
    time = strsplit(variable_name,"_")[[1]][2]
    ##valid time
    if(!is.na(as.numeric(time))){
      uniquevariables=c(uniquevariables,truename)
    }
    else{
      uniquevariables=c(uniquevariables,variable_name)
    }
  }
}
uniquevariables=unique(uniquevariables)
variables_no_date=uniquevariables[which(uniquevariables%in%names(dat_new))]
variables_date = uniquevariables[which(!(uniquevariables%in%names(dat_new)))]
variables_date = variables_date[-1]

##create dat_week8, with longitudinal slopes
dat_week8 = data.frame(ID=dat_new$ID)
for(varnames in variables_no_date){
  dat_week8[,varnames]=dat_new[,varnames]
}

for(varnames in variables_date){
  if(paste(varnames,8,sep="_") %in% names(dat_new)){
    dat_week8[,paste(varnames,8,sep="_")]=dat_new[,paste(varnames,8,sep="_")]
  }
}
##include CRP_0
dat_week8$`C Reactive Protein_0`=dat_new$`C Reactive Protein_0`
##CNTO at week 8 has too many missing include week 6
dat_week8$`C Reactive Protein_6`=dat_new$`C Reactive Protein_6`
##Ratio variable will be changed to ratio, first record drug level
dat_week8$CNTO_6_Ratio_CRP = dat_new$`CNTO 1275 PK MSD ASSAY_6`
##delete actarmcd
ACT_col=which(names(dat_week8)=="ACTARM"|names(dat_week8)=="ACTARMCD")
dat_week8=dat_week8[,-ACT_col]
names(dat_week8)
##check correlation #first find numeric variables
uniquevalues = c()
for(i in 2:ncol(dat_week8)){
  uniquevalues[i]=length(unique(dat_week8[,i]))
}
numeric_var = which(uniquevalues>10)
#

names(dat_week8)

##Remove all /Leukocytes variables and Hemoglobin
remove_col = c(14,21,22,24,28,30,32)
dat_week8 = dat_week8[,-remove_col]
names(dat_week8)
##add in weight
qs = qs[qs$DUSUBJID%in%dat_week8$ID,]
for(i in 1:nrow(dat_week8)){
  if(length(qs$QSSTRESN[qs$DUSUBJID==dat_week8$ID[i]])>0){
    dat_week8$weight[i] = qs$QSSTRESN[qs$DUSUBJID==dat_week8$ID[i]] 
  }
}
sum(is.na(dat_week8$weight))

##include other CRPs 
##dat_allCRP use not only week 8 crp
dat_allCRP = dat_week8
dat_allCRP$`C Reactive Protein_3`=dat_new$`C Reactive Protein_3`
##Ratio variable will be changed to ratio, first record drug level
dat_allCRP$CNTO_3_Ratio_CRP=dat_new$`CNTO 1275 PK MSD ASSAY_3`
dat_allCRP$CNTO_0_Ratio_CRP=dat_new$`CNTO 1275 PK MSD ASSAY_0`


##check NA na omit dat_allCRP
NAs = c()
for(i in 1:ncol(dat_allCRP)){
  NAs=c(NAs,sum(is.na(dat_allCRP[,i])))
}
NAs

NAs = c()
for(i in 1:nrow(dat_allCRP)){
  NAs=c(NAs,sum(is.na(dat_allCRP[i,])))
}
NAs
haveNA = which(NAs>0)

##delete the samples with NA
dat_allCRP=dat_allCRP[-haveNA,]
## change the response encoding
dat_allCRP$CRP_response=(dat_allCRP$CRP_response<5)

##check NAs na.omit for dat_week8
NAs = c()
for(i in 1:ncol(dat_week8)){
  NAs=c(NAs,sum(is.na(dat_week8[,i])))
}
NAs

NAs = c()
for(i in 1:nrow(dat_week8)){
  NAs=c(NAs,sum(is.na(dat_week8[i,])))
}
NAs
haveNA = which(NAs>0)

##delete the samples with NA
dat_week8=dat_week8[-haveNA,]
## change the response encoding
dat_week8$CRP_response=(dat_week8$CRP_response<5)
##

names(dat_allCRP)

dat_allCRP$CRP_response=as.factor(dat_allCRP$CRP_response)

##raitos
##Use all CRP values, ratio to CRP at each week
dat_allCRP$CNTO_6_Ratio_CRP=dat_allCRP$CNTO_6_Ratio_CRP/dat_allCRP$`C Reactive Protein_6`
##Potential Inf in Ratio, Inf are replced with the maximum value
infinite = which(is.infinite(dat_allCRP$CNTO_6_Ratio_CRP))
if(length(infinite)>0){
  dat_allCRP[infinite,"CNTO_6_Ratio_CRP"]=max(dat_allCRP[-infinite,"CNTO_6_Ratio_CRP"])
}
dat_allCRP$CNTO_3_Ratio_CRP=dat_allCRP$CNTO_3_Ratio_CRP/dat_allCRP$`C Reactive Protein_3`
##Potential Inf in Ratio, Inf are replced with the maximum value
infinite = which(is.infinite(dat_allCRP$CNTO_3_Ratio_CRP))
if(length(infinite)>0){
  dat_allCRP[infinite,"CNTO_3_Ratio_CRP"]=max(dat_allCRP[-infinite,"CNTO_3_Ratio_CRP"])
}
dat_allCRP$CNTO_0_Ratio_CRP=dat_allCRP$CNTO_0_Ratio_CRP/dat_allCRP$`C Reactive Protein_0`
##Potential Inf in Ratio, Inf are replced with the maximum value
infinite = which(is.infinite(dat_allCRP$CNTO_0_Ratio_CRP))
if(length(infinite)>0){
  dat_allCRP[infinite,"CNTO_0_Ratio_CRP"]=max(dat_allCRP[-infinite,"CNTO_0_Ratio_CRP"])
}
## Delete slopes
dat_allCRP=dat_allCRP[,-c(7,8,9)]
n=nrow(dat_allCRP)

save(dat_allCRP,file = "dat_allCRP_noHemaWeight.Rdata")

##week 0 only baseline 
dat_week0 = data.frame(ID=dat_new$ID)

for(varnames in variables_no_date){
  dat_week0[,varnames]=dat_new[,varnames]
}
for(varnames in variables_date){
  if(paste(varnames,0,sep="_") %in% names(dat_new)){
    dat_week0[,paste(varnames,0,sep="_")]=dat_new[,paste(varnames,0,sep="_")]
  }
}
for(i in 1:nrow(dat_week0)){
  if(length(qs$QSSTRESN[qs$DUSUBJID==dat_week0$ID[i]])>0){
    dat_week0$weight[i] = qs$QSSTRESN[qs$DUSUBJID==dat_week0$ID[i]] 
  }
}
##check NA
dat_week0=dat_week0[dat_week0$ID%in%dat_week8$ID,]
sum(dat_week0$ID%in%dat_week8$ID)
## change the response encoding
dat_week0$CRP_response=as.factor(dat_week0$CRP_response<5)
## remove the ARMCODE variables
ACT_col=which(names(dat_week0)=="ACTARM"|names(dat_week0)=="ACTARMCD")
dat_week0=dat_week0[,-ACT_col]

names(dat_week0)
##remove all /Leukocytes variables and Hemoglobin
remove_col0 = c(11,14,15,16,20,22,24)
dat_week0=dat_week0[,-remove_col0]

NAs = c()
for(i in 1:nrow(dat_week0)){
  NAs=c(NAs,sum(is.na(dat_week0[i,])))
}
NAs
haveNA = which(NAs>0)
##delete the samples with NA
dat_week0=dat_week0[-haveNA,]
##delete slopes
dat_week0=dat_week0[,-c(7,8,9)]


##week8 pure without slopes
dat_week8_pure = dat_week8
##delete slope
dat_week8_pure=dat_week8_pure[,-c(7,8,9)]


names(dat_week8)
names(dat_week8_pure)

set.seed(2)
##store the tree objs
##with all CRP and ratios
treeobj_list=list()
rocobj_list=list()
##only week 8 CRP and ratios
treeobj8_list=list()
rocobj8_list=list()
##Baseline
treeobj0_list=list()
rocobj0_list=list()
##week 8 only no Ratio
treeobj8pure_list=list()
rocobj8pure_list=list()


####random splits cross validation
repl=100
n=nrow(dat_allCRP)
## proportion used as test 
testprop = 0.3
##store the estimates

roc_CRP0_list=list()
roc_CRP8_list=list()
roc_CRP0=c()
roc_CRP8=c()
roc_albumin=c()
roc_CRP6=c()
roc_CRP3=c()
roc_ratio_6=c()
for(i in 1:repl){
  test = sample(n,floor(n*testprop))
  ## first column is ID
  ## second column is response
  ##all crp
  trainset = dat_allCRP[-test,-1]
  testset = dat_allCRP[test,-(c(1,2))]
  names(trainset)=make.names(names(trainset))
  names(testset)=make.names(names(testset))
  rf_train = randomForest(CRP_response~.,data=trainset)
  treeobj_list[[i]]=rf_train
  prediction = predict(rf_train,newdata=testset,type="prob")
  rocobj_list[[i]]=roc(dat_allCRP[test,2],prediction[,2],ci=T)
  ##week 8+ slopes
  trainset = dat_week8[-test,-1]
  testset = dat_week8[test,-(c(1,2))]
  names(trainset)=make.names(names(trainset))
  names(testset)=make.names(names(testset))
  rf_train = randomForest(CRP_response~.,data=trainset)
  treeobj8_list[[i]]=rf_train
  prediction = predict(rf_train,newdata=testset,type="prob")
  rocobj8_list[[i]]=roc(dat_week8[test,2],prediction[,2],ci=T)
  ## week 0
  trainset = dat_week0[-test,-1]
  testset = dat_week0[test,-(c(1,2))]
  names(trainset)=make.names(names(trainset))
  names(testset)=make.names(names(testset))
  rf_train = randomForest(CRP_response~.,data=trainset,na.action = na.omit)
  treeobj0_list[[i]]=rf_train
  prediction = predict(rf_train,newdata=testset,type="prob")
  rocobj0_list[[i]]=roc(dat_week0[test,2],prediction[,2],ci=T)
  
  ##week 8 with no ratio no slope
  trainset = dat_week8_pure[-test,-1]
  testset = dat_week8_pure[test,-(c(1,2))]
  names(trainset)=make.names(names(trainset))
  names(testset)=make.names(names(testset))
  rf_train = randomForest(CRP_response~.,data=trainset)
  treeobj8pure_list[[i]]=rf_train
  prediction = predict(rf_train,newdata=testset,type="prob")
  rocobj8pure_list[[i]]=roc(dat_week8_pure[test,2],prediction[,2],ci=T)
  
  roc_CRP0_list[[i]]=roc(dat_allCRP[test,2],dat_allCRP$`C Reactive Protein_0`[test],ci=T)
  roc_CRP8_list[[i]]=roc(dat_allCRP[test,2],dat_allCRP$`C Reactive Protein_8`[test],ci=T)
  
  roc_CRP0[i]=roc(dat_allCRP[test,2],dat_allCRP$`C Reactive Protein_0`[test])$auc
  roc_CRP8[i]=roc(dat_allCRP[test,2],dat_allCRP$`C Reactive Protein_8`[test])$auc
  roc_albumin[i]=roc(dat_allCRP[test,2],dat_allCRP$Albumin_8[test])$auc
  roc_CRP6[i]=roc(dat_allCRP[test,2],dat_allCRP$`C Reactive Protein_6`[test])$auc
  roc_CRP3[i]=roc(dat_allCRP[test,2],dat_allCRP$`C Reactive Protein_3`[test])$auc
  roc_ratio_6[i]=roc(dat_allCRP[test,2],dat_allCRP$CNTO_6_Ratio_CRP[test])$auc
  
}
rocobj_list[[1]]$auc
##calculate average roc, var importance
roc=c()
varimp=rep(0,length(treeobj_list[[1]]$importance))
roc8=c()
varimp8=rep(0,length(treeobj8_list[[1]]$importance))
roc0=c()
varimp0=rep(0,length(treeobj0_list[[1]]$importance))
roc8pure=c()
varimp8pure=rep(0,length(treeobj8pure_list[[1]]$importance))
for(i in 1:repl){
  roc[i]=rocobj_list[[i]]$auc
  varimp = cbind(varimp,treeobj_list[[i]]$importance)
  roc8[i]=rocobj8_list[[i]]$auc
  varimp8 = cbind(varimp8,treeobj8_list[[i]]$importance)
  roc0[i]=rocobj0_list[[i]]$auc
  varimp0 = cbind(varimp0,treeobj0_list[[i]]$importance)
  roc8pure[i]=rocobj8pure_list[[i]]$auc
  varimp8pure = cbind(varimp8pure,treeobj8pure_list[[i]]$importance)
}
save.image(file="model_noHema_weight.Rdata")
##checking and plotting
mean(roc)
varimp=varimp[,-1]
sort(apply(varimp,1,mean),decreasing = T)[1:10]
barplot(sort(apply(varimp,1,mean),decreasing = T)[1:10])

mean(roc)
varimp=varimp[,-1]
sort(apply(varimp,1,mean),decreasing = T)[1:10]
barplot(sort(apply(varimp,1,mean),decreasing = T)[1:10])

mean(roc8)
varimp8=varimp8[,-1]
sort(apply(varimp8,1,mean),decreasing = T)[1:10]
barplot(sort(apply(varimp8,1,mean),decreasing = T)[1:10])

mean(roc0)
varimp0=varimp0[,-1]
sort(apply(varimp0,1,mean),decreasing = T)[1:10]

mean(roc8pure)
varimp8pure=varimp8pure[,-1]
sort(apply(varimp8pure,1,mean),decreasing = T)[1:10]

mean(roc_albumin)
mean(roc_CRP0)
mean(roc_CRP8)
mean(roc_ratio_6)
mean(roc_CRP3)
mean(roc_CRP6)
sd(roc_CRP8)
#