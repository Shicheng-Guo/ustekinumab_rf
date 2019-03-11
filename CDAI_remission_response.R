setwd("~/RF_build")
load("model_noHema_weight.Rdata")
setwd("~/raw_tables")

##get CDAI
qs = read.csv("qs.csv")
qs = qs[qs$QSTESTCD=="CDAIX12",]
qs$DUSUBJID = sapply(qs$DUSUBJID,substr,start=17,stop=30)
qs = qs[qs$DUSUBJID%in%dat_allCRP$ID,]

ids = dat_allCRP$ID
maxdy=c()
for(i in 1:length(ids)){
  maxdy[i] = max(qs$QSDY[qs$DUSUBJID==ids[i]])
}
lastCDAI=c()
for(i in 1:length(ids)){
  temp = qs$QSSTRESN[qs$DUSUBJID==ids[i]&qs$QSDY==maxdy[i]]
  lastCDAI[i]= temp[length(temp)]
}
summary(maxdy)
summary(lastCDAI)
sum(lastCDAI<150,na.rm=T)

summary(lastCDAI)
sum(lastCDAI<150,na.rm = T)
summary(maxdy)
sum(maxdy<350)
sum(maxdy<280 & lastCDAI<150,na.rm = T)


##CDAI remission if last CDAI after week 42 < 150
##missing is treated as failure
CDAI_response = lastCDAI < 150
finish_study = maxdy/7>=42
CDAI_response = CDAI_response & finish_study
CDAI_response[is.na(CDAI_response)]=F
CDAI_response = as.factor(CDAI_response)
summary(CDAI_response)

table(CDAI_response,dat_allCRP$CRP_response)

set.seed(1)
##first column is ID, second column is CRP response
CDAI_dat = dat_allCRP[,-c(1,2)]
CDAI_dat$CDAI_response = CDAI_response
##try an oob with the week 8 model
names(CDAI_dat)= make.names(names(CDAI_dat))
rf_cdai = randomForest(CDAI_response~.,data = CDAI_dat)
pred = predict(rf_cdai,type="p")
roc(CDAI_response,pred[,2],ci=T)
varImpPlot(rf_cdai)
