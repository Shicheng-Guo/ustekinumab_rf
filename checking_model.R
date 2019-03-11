setwd("~/RF_build")
load("model_noHema_weight.Rdata")
library(missForest)
library(randomForest)
library(pROC)


set.seed(1)

roc(dat_week0$CRP_response,dat_week0$`CNTO 1275 PK MSD ASSAY_0`)
mean(roc)
quantile(roc,prob=c(0.025,0.975))
varimp=varimp[,-1]
sort(apply(varimp,1,mean),decreasing = T)[1:10]
##normalize the varimp score
for(i in 1:ncol(varimp)){
  varimp[,i]=varimp[,i]/sum(varimp[,i])
}
barplot(sort(apply(100*varimp,1,mean),decreasing = T))
sort(apply(varimp*100,1,mean),decreasing = T)

length(roc)

mean(roc8)
quantile(roc8,prob=c(0.025,0.975))
varimp8=varimp8[,-1]
sort(apply(varimp8,1,mean),decreasing = T)[1:10]
barplot(sort(apply(varimp8,1,mean),decreasing = T)[1:10])

mean(roc0)
quantile(roc0,prob=c(0.025,0.975))
varimp0=varimp0[,-1]
sort(apply(varimp0,1,mean),decreasing = T)[1:10]

mean(roc8pure)
quantile(roc8pure,prob=c(0.025,0.975))
varimp8pure=varimp8pure[,-1]
sort(apply(varimp8pure,1,mean),decreasing = T)[1:10]

mean(roc_albumin)
mean(roc_CRP0)
quantile(roc_CRP0,prob=c(0.025,0.975))
mean(roc_CRP8)
quantile(roc_CRP8,prob=c(0.025,0.975))
mean(roc_ratio_6)
mean(roc_CRP3)
mean(roc_CRP6)
sd(roc_CRP8)

summary(dat_allCRP$CRP_response)
##TRUE is lower than 5

##get the best cutoff for each replication and calculate sensitivity, specificity
cutoff = rep(0,3)
cutoff_0 = rep(0,3)
cutoff_8 = rep(0,3)
cutoff_8pure = rep(0,3)
cutoff_CRP0 = rep(0,3)
cutoff_CRP8 = rep(0,3)
for (i in 1:length(rocobj_list)){
  cutoff = cbind(cutoff,coords(rocobj_list[[i]],'b',best.method = "closest.topleft"))
  cutoff_0 = cbind(cutoff_0,coords(rocobj0_list[[i]],'b',best.method = "closest.topleft"))
  cutoff_8 = cbind(cutoff_8,coords(rocobj8_list[[i]],'b',best.method = "closest.topleft"))
  cutoff_8pure = cbind(cutoff_8pure,coords(rocobj8pure_list[[i]],'b',best.method = "closest.topleft"))
  cutoff_CRP0 = cbind(cutoff_CRP0,coords(roc_CRP0_list[[i]],'b',best.method = "closest.topleft"))
  cutoff_CRP8 = cbind(cutoff_CRP8,coords(roc_CRP8_list[[i]],'b',best.method = "closest.topleft"))
}

cutoff= cutoff[,-1]
cutoff_0= cutoff_0[,-1]
cutoff_8= cutoff_8[,-1]
cutoff_8pure= cutoff_8pure[,-1]
cutoff_CRP0= cutoff_CRP0[,-1]
cutoff_CRP8= cutoff_CRP8[,-1]

## Fill in table for the specific split
closest=which.min(abs(roc-mean(roc)))
rocobj_list[[closest]]$auc
rocobj_list[[closest]]$ci
library(ModelMetrics)
brier(as.numeric(rocobj_list[[closest]]$response)-1,rocobj_list[[closest]]$predictor)

rocobj0_list[[closest]]$auc
rocobj0_list[[closest]]$ci
brier(as.numeric(rocobj0_list[[closest]]$response)-1,rocobj0_list[[closest]]$predictor)

rocobj8_list[[closest]]$auc
rocobj8_list[[closest]]$ci

rocobj8pure_list[[closest]]$auc
rocobj8pure_list[[closest]]$ci

roc_CRP0_list[[closest]]$auc
roc_CRP0_list[[closest]]$ci

roc_CRP8_list[[closest]]$auc
roc_CRP8_list[[closest]]$ci

##cutoff
coords(rocobj_list[[closest]],'b',best.method = "closest.topleft")
coords(rocobj0_list[[closest]],'b',best.method = "closest.topleft")
coords(rocobj8_list[[closest]],'b',best.method = "closest.topleft")
coords(rocobj8pure_list[[closest]],'b',best.method = "closest.topleft")
coords(roc_CRP0_list[[closest]],'b',best.method = "closest.topleft")
coords(roc_CRP8_list[[closest]],'b',best.method = "closest.topleft")
rocobj_list[[closest]]$direction
##predicted cases and true success rate
#best
temp = rocobj_list[[closest]]
a=sum(temp$cases > cutoff[1,closest])
b=sum(temp$cases <= cutoff[1,closest])
c=sum(temp$controls>cutoff[1,closest])
d=sum(temp$controls<=cutoff[1,closest])
#predicted success
a+c
a/(a+c)
#predicted failure
b+d
d/(b+d)

##week0
temp = rocobj0_list[[closest]]
a=sum(temp$cases > cutoff_0[1,closest])
b=sum(temp$cases <= cutoff_0[1,closest])
c=sum(temp$controls>cutoff_0[1,closest])
d=sum(temp$controls<=cutoff_0[1,closest])
#predicted success
a+c
a/(a+c)
#predicted failure
b+d
d/(b+d)

##week8
temp = rocobj8_list[[closest]]
a=sum(temp$cases > cutoff_8[1,closest])
b=sum(temp$cases <= cutoff_8[1,closest])
c=sum(temp$controls>cutoff_8[1,closest])
d=sum(temp$controls<=cutoff_8[1,closest])
#predicted success
a+c
a/(a+c)
#predicted failure
b+d
d/(b+d)

#week8pure
temp = rocobj8pure_list[[closest]]
a=sum(temp$cases > cutoff_8pure[1,closest])
b=sum(temp$cases <= cutoff_8pure[1,closest])
c=sum(temp$controls>cutoff_8pure[1,closest])
d=sum(temp$controls<=cutoff_8pure[1,closest])
#predicted success
a+c
a/(a+c)
#predicted failure
b+d
d/(b+d)

##CRP0
temp = roc_CRP0_list[[closest]]
a=sum(temp$cases <cutoff_CRP0[1,closest])
b=sum(temp$cases >= cutoff_CRP0[1,closest])
c=sum(temp$controls<cutoff_CRP0[1,closest])
d=sum(temp$controls>=cutoff_CRP0[1,closest])
#predicted success
a+c
a/(a+c)
#predicted failure
b+d
d/(b+d)

##CRP8
temp = roc_CRP8_list[[closest]]
a=sum(temp$cases <cutoff_CRP8[1,closest])
b=sum(temp$cases >= cutoff_CRP8[1,closest])
c=sum(temp$controls<cutoff_CRP8[1,closest])
d=sum(temp$controls>=cutoff_CRP8[1,closest])
#predicted success
a+c
a/(a+c)
#predicted failure
b+d
d/(b+d)



##change margin
setwd("~/results/noHema_Weight")

##ROC Plot, first find the one closest to the mean
mean(roc)
pdf(file="week8roc_noHema_weight.pdf")
plot(rocobj_list[[closest]],
     main="ROC Plot for week 8 model \n with CRP and CNTO/CRP Ratio",legacy.axes=T,
     cex.main=0.9)

text(0.85,0.85,sprintf("AuROC= %1.2f,\n 95%% CI= [%1.2f,%1.2f]",
                      rocobj_list[[closest]]$auc,
                      rocobj_list[[closest]]$ci[1],rocobj_list[[closest]]$ci[3]))
dev.off()
tiff(file="week8roc_noHema_weight.tiff",width=7,height=7,units="in",res=600,compression = "zip")
plot(rocobj_list[[closest]],legacy.axes=T,
     main="ROC Plot for week 8 model \n with CRP and CNTO/CRP Ratio",
     cex.main=1)
text(0.80,0.9,sprintf("AuROC= %1.2f,\n 95%% CI= [%1.2f,%1.2f]",
                       rocobj_list[[closest]]$auc,
                       rocobj_list[[closest]]$ci[1],rocobj_list[[closest]]$ci[3])
     ,cex=1.5)
dev.off()

png(file="week8roc_noHema_weight.png",width=7,height=7,units="in",res=600)
plot(rocobj_list[[closest]],legacy.axes=T,
     main="ROC Plot for week 8 model \n with CRP and CNTO/CRP Ratio",
     cex.main=1)
text(0.80,0.9,sprintf("AuROC= %1.2f,\n 95%% CI= [%1.2f,%1.2f]",
                      rocobj_list[[closest]]$auc,
                      rocobj_list[[closest]]$ci[1],rocobj_list[[closest]]$ci[3])
     ,cex=1.5)
dev.off()

##baseline roc plot

pdf(file="week0roc_noHema_weight.pdf")
plot(rocobj0_list[[closest]],
     main="ROC Plot for baseline model",legacy.axes=T,
     cex.main=0.9)
text(0.85,0.85,sprintf("AuROC= %1.2f,\n 95%% CI= [%1.2f,%1.2f]",
                       rocobj0_list[[closest]]$auc,
                       rocobj0_list[[closest]]$ci[1],rocobj0_list[[closest]]$ci[3]))
dev.off()
tiff(file="week0roc_noHema_weight.tiff",width=7,height=7,units="in",res=600,compression = "zip")
plot(rocobj0_list[[closest]],legacy.axes=T,
     main="ROC Plot for baseline model",
     cex.main=2)
text(0.80,0.85,sprintf("AuROC= %1.2f,\n 95%% CI= [%1.2f,%1.2f]",
                       rocobj0_list[[closest]]$auc,
                       rocobj0_list[[closest]]$ci[1],rocobj0_list[[closest]]$ci[3]),cex=1.5)
dev.off()

png(file="week0roc_noHema_weight.png",width=7,height=7,units="in",res=600)
plot(rocobj0_list[[closest]],legacy.axes=T,
     main="ROC Plot for baseline model",
     cex.main=2)
text(0.80,0.85,sprintf("AuROC= %1.2f,\n 95%% CI= [%1.2f,%1.2f]",
                       rocobj0_list[[closest]]$auc,
                       rocobj0_list[[closest]]$ci[1],rocobj0_list[[closest]]$ci[3]),cex=1.5)
dev.off()

#remember change the drug ratio name week 0

##week 8 varimp plot
## need to match name
##change names
## black white gray version
## retrain the models with all data
names(dat_allCRP)=make.names(names(dat_allCRP))
names(dat_allCRP)[34]="CNTO_1_Ratio_CRP"
rf_allCRP = randomForest(CRP_response~.,data=dat_allCRP[,-1])
pred =predict(rf_allCRP,type="prob")
rocall =roc(dat_allCRP$CRP_response,pred[,2])
thresall = coords(rocall,"b",best.method = "closest.topleft")[1]
sum(pred[,2]<thresall)
sum(pred[,2]>=thresall)
sum(dat_allCRP$CRP_response[pred[,2]<thresall]==T)
sum(dat_allCRP$CRP_response[pred[,2]>=thresall]==T)


row.names(rf_allCRP$importance)
##col number 2 is red 4 is blue 6 is purple
# red: decreasing blue:increasing purple: unknown
week8_color = c("purple","purple","purple","purple",
                "purple","purple","purple","purple","red",
                "blue","purple","purple","purple","purple","purple",
                "blue","purple","purple","purple","red","blue",
               "red","purple","red","purple",
                "red","blue","purple","blue","red","red","red")

week8_names = c("Ethnic","Sex","Race","Chloride","Basophils","Eosinophils",
                "Sodium","Potassium","Aspartate Aminotransferase","Hemoglobin",
                "Blood Urea Nitrogen", "Lymphocytes", "Alkaline Phosphatase",
                "Monocytes","Calcium","CNTO/CRP Ratio 1","Phosphate","Platelets",
                "Age","Leukocytes","Protein","Glucose","Neutrophils Segmented",
                "Creatinine","Weight","CRP 0", "Albumin", "CNTO/CRP Ratio 3",
                "CNTO/CRP Ratio 6", "CRP 3","CRP 6", "CRP 8")
##



pdf(file="week8varimp_noHema_weight.pdf")
par(mar=c(5,8,2,2))

barplot(sort(rf_allCRP$importance),names.arg=week8_names,space=0.5,horiz=T,cex.names=0.6,axis.lty=0.5,
        las=1,mgp=c(3,0.5,0),xlim=c(0,12),xlab="Predictor importance",
        main="Week 8 variable importance plot",col=week8_color)
legend("bottomright",
       legend=c("Higher values ~ Worse outcomes","Higher values ~ Better outcomes", "Mixed"),
        fill=c("red","blue","purple"))
dev.off()
tiff(file="week8varimp_noHema_weight.tiff",width=7,height=7,units="in",res=600,compression = "zip")
par(mar=c(5,12,2,2))
barplot(sort(rf_allCRP$importance),names.arg=week8_names,space=0.5,horiz=T,cex.names=1,axis.lty=0.5,
        las=1,mgp=c(3,0.5,0),xlim=c(0,12),xlab="Predictor importance",
        main="Week 8 variable importance plot",col=week8_color)
legend("bottomright",
       legend=c("Higher values ~ Worse outcomes","Higher values ~ Better outcomes", "Mixed"),
       fill=c("red","blue","purple"))
dev.off()

png(file="week8varimp_noHema_weight.png",width=7,height=7,units="in",res=600)
par(mar=c(5,12,2,2))
barplot(sort(rf_allCRP$importance),names.arg=week8_names,space=0.5,horiz=T,cex.names=1,axis.lty=0.5,
        las=1,mgp=c(3,0.5,0),xlim=c(0,12),xlab="Predictor importance",
        main="Week 8 variable importance plot",col=week8_color)
legend("bottomright",
       legend=c("Higher values ~ Worse outcomes","Higher values ~ Better outcomes", "Mixed"),
       fill=c("red","blue","purple"))
dev.off()


##black_white
week8_color_black = c("grey","grey","grey","grey",
                "grey","grey","grey","grey","black",
                "white","grey","grey","grey","grey","grey",
                "white","grey","grey","grey","black","white",
                "black","grey","black","grey",
                "black","white","grey","white","black","black","black")
pdf(file="week8varimp_noHema_weight_black.pdf")
par(mar=c(5,8,2,2))

barplot(sort(rf_allCRP$importance),names.arg=week8_names,space=0.5,horiz=T,cex.names=0.6,axis.lty=0.5,
        las=1,mgp=c(3,0.5,0),xlim=c(0,12),xlab="Predictor importance",
        main="Week 8 variable importance plot",col=week8_color_black)
legend("bottomright",
       legend=c("Higher values ~ Worse outcomes","Higher values ~ Better outcomes", "Mixed"),
       fill =c("black","white","grey"))
dev.off()
tiff(file="week8varimp_noHema_weight_black.tiff",width=7,height=7,units="in",res=600,compression = "zip")
par(mar=c(5,12,2,2))
barplot(sort(rf_allCRP$importance),names.arg=week8_names,space=0.5,horiz=T,cex.names=1,axis.lty=0.5,
        las=1,mgp=c(3,0.5,0),xlim=c(0,12),xlab="Predictor importance",
        main="Week 8 variable importance plot",col=week8_color_black)
legend("bottomright",
       legend=c("Higher values ~ Worse outcomes","Higher values ~ Better outcomes", "Mixed"),
       fill =c("black","white","grey"))
dev.off()

png(file="week8varimp_noHema_weight_black.png",width=7,height=7,units="in",res=600)
par(mar=c(5,12,2,2))
barplot(sort(rf_allCRP$importance),names.arg=week8_names,space=0.5,horiz=T,cex.names=1,axis.lty=0.5,
        las=1,mgp=c(3,0.5,0),xlim=c(0,12),xlab="Predictor importance",
        main="Week 8 variable importance plot",col=week8_color_black)
legend("bottomright",
       legend=c("Higher values ~ Worse outcomes","Higher values ~ Better outcomes", "Mixed"),
       fill =c("black","white","grey"))
dev.off()
##baseline variable importance
dat_week0=na.omit(dat_week0)
names(dat_week0)=make.names(names(dat_week0))
names(dat_week0)
names(dat_week0)[16]="CNTO_1"
rf_week0 = randomForest(CRP_response~.,data=dat_week0[,-1])

week0_color = c("purple","purple","purple","blue","purple","purple",
                  "blue","purple","purple","purple","purple","red","purple","purple","red")
week0_names = c("Ethnic","Sex","Race","Basophils","Leukocytes","Neutrophils Segmented",
                "Eosinophils","Lymphocytes","Age","Hemoglobin","Monocytes","CNTO 1","Platelets",
                "Weight","CRP 0")
week0_color_black = c("grey","grey","grey","white","grey","grey",
                "white","grey","grey","grey","grey","black","grey","grey","black")

pdf(file="week0varimp_noHema_weight.pdf")
par(mar=c(5,8,2,2))
barplot(sort(rf_week0$importance),names.arg=week0_names,space=0.5,horiz=T,cex.names=0.6,axis.lty=0.5,
        las=1,mgp=c(3,0.5,0),xlim=c(0,20),xlab="Predictor importance",
        main="Baseline variable importance plot",col=week0_color)
legend("bottomright",
       legend=c("Higher values ~ Worse outcomes","Higher values ~ Better outcomes", "Mixed"),
       fill=c("red","blue","purple"))
dev.off()
tiff(file="week0varimp_noHema_weight.tiff",width=7,height=7,units="in",res=600,compression = "zip")
par(mar=c(5,10,2,2))
barplot(sort(rf_week0$importance),names.arg=week0_names,space=0.5,horiz=T,cex.names=1,axis.lty=0.5,
        las=1,mgp=c(3,0.5,0),xlim=c(0,20),xlab="Predictor importance",
        main="Baseline variable importance plot",col=week0_color)
legend("bottomright",
       legend=c("Higher values ~ Worse outcomes","Higher values ~ Better outcomes", "Mixed"),
       fill=c("red","blue","purple"))
dev.off()

png(file="week0varimp_noHema_weight.png",width=7,height=7,units="in",res=600)
par(mar=c(5,10,2,2))
barplot(sort(rf_week0$importance),names.arg=week0_names,space=0.5,horiz=T,cex.names=1,axis.lty=0.5,
        las=1,mgp=c(3,0.5,0),xlim=c(0,20),xlab="Predictor importance",
        main="Baseline variable importance plot",col=week0_color)
legend("bottomright",
       legend=c("Higher values ~ Worse outcomes","Higher values ~ Better outcomes", "Mixed"),
       fill=c("red","blue","purple"))
dev.off()


pdf(file="week0varimp_noHema_weight_black.pdf")
par(mar=c(5,8,2,2))
barplot(sort(rf_week0$importance),names.arg=week0_names,space=0.5,horiz=T,cex.names=0.6,axis.lty=0.5,
        las=1,mgp=c(3,0.5,0),xlim=c(0,20),xlab="Predictor importance",
        main="Baseline variable importance plot",col=week0_color_black)
legend("bottomright",
       legend=c("Higher values ~ Worse outcomes","Higher values ~ Better outcomes", "Mixed"),
       fill=c("black","white","grey"))
dev.off()
tiff(file="week0varimp_noHema_weight_black.tiff",width=7,height=7,units="in",res=600,compression = "zip")
par(mar=c(5,10,2,2))
barplot(sort(rf_week0$importance),names.arg=week0_names,space=0.5,horiz=T,cex.names=1,axis.lty=0.5,
        las=1,mgp=c(3,0.5,0),xlim=c(0,20),xlab="Predictor importance",
        main="Baseline variable importance plot",col=week0_color_black)
legend("bottomright",
       legend=c("Higher values ~ Worse outcomes","Higher values ~ Better outcomes", "Mixed"),
       fill=c("black","white","grey"))
dev.off()

png(file="week0varimp_noHema_weight_black.png",width=7,height=7,units="in",res=600)
par(mar=c(5,10,2,2))
barplot(sort(rf_week0$importance),names.arg=week0_names,space=0.5,horiz=T,cex.names=1,axis.lty=0.5,
        las=1,mgp=c(3,0.5,0),xlim=c(0,20),xlab="Predictor importance",
        main="Baseline variable importance plot",col=week0_color_black)
legend("bottomright",
       legend=c("Higher values ~ Worse outcomes","Higher values ~ Better outcomes", "Mixed"),
       fill=c("black","white","grey"))
dev.off()


## bootstrap 
#sensitivity#specificity over replications
apply(cutoff,1,mean)
apply(cutoff_0,1,mean)
apply(cutoff_8,1,mean)
apply(cutoff_8pure,1,mean)
apply(cutoff_CRP0,1,mean)
apply(cutoff_CRP8,1,mean)

apply(cutoff,1,quantile, prob=0.025)
apply(cutoff_0,1,quantile, prob=0.025)
apply(cutoff_8,1,quantile, prob=0.025)
apply(cutoff_8pure,1,quantile, prob=0.025)
apply(cutoff_CRP0,1,quantile, prob=0.025)
apply(cutoff_CRP8,1,quantile, prob=0.025)

apply(cutoff,1,quantile, prob=0.975)
apply(cutoff_0,1,quantile, prob=0.975)
apply(cutoff_8,1,quantile, prob=0.975)
apply(cutoff_8pure,1,quantile, prob=0.975)
apply(cutoff_CRP0,1,quantile, prob=0.975)
apply(cutoff_CRP8,1,quantile, prob=0.975)
##
##True success rate
length(rocobj_list[[1]]$cases)
a=sum(rocobj_list[[1]]$cases > cutoff[1,1])
b=sum(rocobj_list[[1]]$cases <= cutoff[1,1])
c=sum(rocobj_list[[1]]$controls>cutoff[1,1])
d=sum(rocobj_list[[1]]$controls<=cutoff[1,1])

success_tsr = a/(a+c)
failure_tsr = d/(b+d)

##True success rate allCRP
success_tsr=c()
failure_tsr=c()
for(i in 1:length(rocobj_list)){
  temp = rocobj_list[[i]]
  a=sum(temp$cases > cutoff[1,i])
  b=sum(temp$cases <= cutoff[1,i])
  c=sum(temp$controls>cutoff[1,i])
  d=sum(temp$controls<=cutoff[1,i])
  success_tsr=c(success_tsr,a/(a+c))
  failure_tsr = c(failure_tsr,d/(b+d))
}
mean(success_tsr)
mean(failure_tsr)

## Baseline
success_tsr=c()
failure_tsr=c()
for(i in 1:length(rocobj_list)){
  temp = rocobj0_list[[i]]
  a=sum(temp$cases > cutoff_0[1,i])
  b=sum(temp$cases <= cutoff_0[1,i])
  c=sum(temp$controls>cutoff_0[1,i])
  d=sum(temp$controls<=cutoff_0[1,i])
  success_tsr=c(success_tsr,a/(a+c))
  failure_tsr = c(failure_tsr,d/(b+d))
}
mean(success_tsr)
mean(failure_tsr)
sum(rocobj0_list[[1]]$cases >cutoff_0[1,1])
sum(rocobj0_list[[1]]$controls>cutoff_0[1,1])
## Week 8 pure
success_tsr=c()
failure_tsr=c()
for(i in 1:length(rocobj_list)){
  temp = rocobj8pure_list[[i]]
  a=sum(temp$cases > cutoff_8pure[1,i])
  b=sum(temp$cases <= cutoff_8pure[1,i])
  c=sum(temp$controls>cutoff_8pure[1,i])
  d=sum(temp$controls<=cutoff_8pure[1,i])
  success_tsr=c(success_tsr,a/(a+c))
  failure_tsr = c(failure_tsr,d/(b+d))
}
mean(success_tsr)
mean(failure_tsr)

## Week 8
success_tsr=c()
failure_tsr=c()
for(i in 1:length(rocobj_list)){
  temp = rocobj8_list[[i]]
  a=sum(temp$cases > cutoff_8[1,i])
  b=sum(temp$cases <= cutoff_8[1,i])
  c=sum(temp$controls>cutoff_8[1,i])
  d=sum(temp$controls<=cutoff_8[1,i])
  success_tsr=c(success_tsr,a/(a+c))
  failure_tsr = c(failure_tsr,d/(b+d))
}
mean(success_tsr)
mean(failure_tsr)
## crp0
## Careful! For single predictor CRP should be less than cutoff!
success_tsr=c()
failure_tsr=c()
for(i in 1:length(rocobj_list)){
  temp = roc_CRP0_list[[i]]
  a=sum(temp$cases < cutoff_CRP0[1,i])
  b=sum(temp$cases >= cutoff_CRP0[1,i])
  c=sum(temp$controls<cutoff_CRP0[1,i])
  d=sum(temp$controls>=cutoff_CRP0[1,i])
  success_tsr=c(success_tsr,a/(a+c))
  failure_tsr = c(failure_tsr,d/(b+d))
}
mean(success_tsr)
mean(failure_tsr)

##CRP 8 
success_tsr=c()
failure_tsr=c()
for(i in 1:length(rocobj_list)){
  temp = roc_CRP8_list[[i]]
  a=sum(temp$cases < cutoff_CRP8[1,i])
  b=sum(temp$cases >= cutoff_CRP8[1,i])
  c=sum(temp$controls<cutoff_CRP8[1,i])
  d=sum(temp$controls>=cutoff_CRP8[1,i])
  success_tsr=c(success_tsr,a/(a+c))
  failure_tsr = c(failure_tsr,d/(b+d))
}
mean(success_tsr)
mean(failure_tsr)
sum(roc_CRP8_list[[1]]$cases>cutoff_CRP8[1,1])
sum(roc_CRP8_list[[1]]$controls>cutoff_CRP8[1,1])
sum(rocobj_list[[1]]$cases>cutoff[1,1])
sum(rocobj_list[[1]]$controls>cutoff[1,1])
##
plot(rocobj_list[[1]])

names(dat_allCRP)=make.names(names(dat_allCRP))
rf_allCRP = randomForest(CRP_response~.,data=dat_allCRP[,-1])
prediction = predict(rf_allCRP,type="prob")
plot(roc(dat_allCRP$CRP_response,prediction[,2]))

##Varimp plot
varimp=varimp[,-1]
varimp0=varimp0[,-1]
varimp8=varimp8[,-1]
varimp8pure=varimp8pure[,-1]

mean_varimp=apply(varimp,1,mean)
mean_varimp0=apply(varimp0,1,mean)
mean_varimp8=apply(varimp8,1,mean)
mean_varimp8pure=apply(varimp8pure,1,mean)

