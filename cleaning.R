setwd("~/raw_tables")
rawlb_3=read.csv("lb3.csv")
rawpc_3=read.csv("pc3.csv")
rawex_3=read.csv("ex3.csv")
rawxa_3 = read.csv("xa3.csv")
rawcm_3=read.csv("cm3.csv")
rawdm_3=read.csv("dm3.csv")



##delete prefix of ID
rawlb_3$DUSUBJID = sapply(rawlb_3$DUSUBJID,substr,start=17,stop=30)
rawpc_3$DUSUBJID = sapply(rawpc_3$DUSUBJID,substr,start=17,stop=30)
rawex_3$DUSUBJID = sapply(rawex_3$DUSUBJID,substr,start=17,stop=30)
rawxa_3$DUSUBJID = sapply(rawxa_3$DUSUBJID,substr,start=17,stop=30)
rawcm_3$DUSUBJID = sapply(rawcm_3$DUSUBJID,substr,start=17,stop=30)
rawdm_3$DUSUBJID = sapply(rawdm_3$DUSUBJID,substr,start=17,stop=30)

##Extract medicated
##Need to be consistently on drug not placebo
table(rawex_3$EXSTDY)
table(rawex_3$VISITNUM)
rawex_3$VISITNUM=floor(rawex_3$VISITNUM)
table(rawlb_3$VISITNUM)
rawlb_3$VISITNUM=floor(rawlb_3$VISITNUM)
table(rawpc_3$VISITNUM)
rawpc_3$VISITNUM=floor(rawpc_3$VISITNUM)
table(rawxa_3$VISITNUM)
rawxa_3$VISITNUM=floor(rawxa_3$VISITNUM)

medicated_temp=c()
medicated_temp=unique(rawex_3$DUSUBJID[rawex_3$VISITNUM==2&rawex_3$EXTRT!="Placebo"])
medicated=c()
for(id in medicated_temp){
  temp=rawex_3$EXTRT[rawex_3$DUSUBJID==id&rawex_3$VISITNUM==9]
  if("Ustekinumab"%in% unique(temp)){
    medicated=c(medicated,id)
  }
}

##clean out placebos
medications = matrix(0,length(medicated),2)
for(i in 1:length(medicated)){
  ID = medicated[i]
  medications[i,1] = sum((rawex_3$EXDOSE[rawex_3$DUSUBJID==ID]>0)&(rawex_3$EXTRT[rawex_3$DUSUBJID==ID]!="Placebo"))
  medications[i,2] = sum(rawex_3$EXTRT[rawex_3$DUSUBJID==ID]=="Placebo")
  
}
head(medications)
table(medications[,1])
table(medications[,2])


rawlb_3=rawlb_3[rawlb_3$DUSUBJID%in%medicated,]
rawpc_3=rawpc_3[rawpc_3$DUSUBJID%in%medicated,]
rawex_3=rawex_3[rawex_3$DUSUBJID%in%medicated,]
rawxa_3=rawxa_3[rawxa_3$DUSUBJID%in%medicated,]
rawcm_3=rawcm_3[rawcm_3$DUSUBJID%in%medicated,]
rawdm_3=rawdm_3[rawdm_3$DUSUBJID%in%medicated,]

#checking stuffs
table(rawlb_3$LBDY[rawlb_3$VISITNUM==5])
table(rawex_3$EXSTDY[rawex_3$VISITNUM==9])
table(rawlb_3$VISITNUM)
table(rawpc_3$VISITNUM)
summary(rawpc_3$PCSTRESN[rawpc_3$VISITNUM==2])
summary(rawpc_3$PCTM[rawpc_3$VISITNUM==2 & is.na(rawpc_3$PCSTRESN)])
length(unique(rawpc_3$DUSUBJID[rawpc_3$VISITNUM==2]))


table(rawex_3$VISITNUM)

##Extract first day of medication (First time EXDOSE>0)
startday=c()
startvis=c()
for(i in medicated){
  startday=c(startday,min((rawex_3[rawex_3$EXDOSE>0 & rawex_3$DUSUBJID==i,])$EXSTDY))
  startvis=c(startvis,min((rawex_3[rawex_3$EXDOSE>0 & rawex_3$DUSUBJID==i,])$VISITNUM))
}
table(startvis)
startday
startvis
##Not placebo on the firstday of medication
placeboID=c()
for(i in medicated){
  index= which(medicated==i)
  temp=rawex_3[rawex_3$DUSUBJID==i,]
  temp=temp[temp$EXDOSE>0,]
  if( temp$EXTRT[temp$EXSTDY==startday[index]]=="Placebo"){
    placeboID=c(placeboID,i)
  }
}
##All these patients are on medication not placebo
## Use startday and medicated as index to clean the data

##Standardize days into weeks by   round[(day-startday)/7]
for(i in medicated){
  index= which(medicated==i)
  refday= startday[index]
  rawlb_3$LBDY[rawlb_3$DUSUBJID==i]=round((rawlb_3$LBDY[rawlb_3$DUSUBJID==i]-refday)/7)
  rawex_3$EXSTDY[rawex_3$DUSUBJID==i]=round((rawex_3$EXSTDY[rawex_3$DUSUBJID==i]-refday)/7)
  rawpc_3$PCDY[rawpc_3$DUSUBJID==i]=round((rawpc_3$PCDY[rawpc_3$DUSUBJID==i]-refday)/7)
  rawxa_3$XADY[rawxa_3$DUSUBJID==i]=round((rawxa_3$XADY[rawxa_3$DUSUBJID==i]-refday)/7)
  rawdm_3$DMDY[rawdm_3$DUSUBJID==i]=round((rawdm_3$DMDY[rawdm_3$DUSUBJID==i]-refday)/7)
  rawcm_3$CMSTDY[rawcm_3$DUSUBJID==i]=round((rawcm_3$CMSTDY[rawcm_3$DUSUBJID==i]-refday)/7)
}


##Merging data
table(rawlb_3$LBDY)

## merge LB



## variable name LBTEST 
## value LBSTRESN
## time  LBDY
## Reference range lower LBSTNRLO
## Reference range higher LBSTNRHI

unique(rawlb_3$LBTEST)
dat = data.frame(ID=medicated)
rawlb_3=rawlb_3[rawlb_3$DUSUBJID%in%medicated,]
##first grab response, last CRP/Calprotectin measurement after week 42
rawlb_response=rawlb_3[rawlb_3$LBDY>=42,]
for(currentID in medicated){
  index = which(medicated==currentID)
  templb=rawlb_response[rawlb_response$DUSUBJID==currentID,]
  if(nrow(templb)==0){
    next
  }
  CRP=NA
  CRPtime=NA
  Calpro=NA
  Calprotime=NA
  CRPref_low=NA
  CRPref_high=NA
  Calproref_low=NA
  Calproref_high=NA
  for(j in 1:nrow(templb)){
    variable_name = as.character(templb$LBTEST[j])
    time = templb$LBDY[j]
    value = templb$LBSTRESN[j]
    if(is.na(value)){
      next
    }
    if(variable_name=="Calprotectin"){
      if(is.na(Calpro)||Calprotime<time){
        Calpro=value
        Calprotime=time
        ## Reference range lower LBSTNRLO
        ## Reference range higher LBSTNRHI
        Calproref_low=templb$LBSTNRLO[j]
        Calproref_high=templb$LBSTNRHI[j]
        
      }
    }
    else if(variable_name=="C Reactive Protein"){
      if(is.na(CRP)||CRPtime<time){
        CRP=value
        CRPtime=time
        ## Reference range lower LBSTNRLO
        ## Reference range higher LBSTNRHI
        CRPref_low=templb$LBSTNRLO[j]
        CRPref_high=templb$LBSTNRHI[j]
      }
    }
  }
  dat[index,"CRP_response"]=CRP
  dat[index,"CRPref_high"]=CRPref_high
  dat[index,"CRPref_low"]=CRPref_low
  dat[index,"Calpro_response"]=Calpro
  dat[index,"Calproref_high"]=Calproref_high
  dat[index,"Calproref_low"]=Calproref_low
}
##Response done

##merging labs by VISITNUM
lblist=unique(rawlb_3$LBTEST)
notuseful = c(13,18,20,38:42,44,47,48,49,51:55)
lblist=lblist[-notuseful]
##useful predictors are in lblist
##VISITNUM  1  SCREENING
# VISITNUM 2 WEEK 0
# VISITNUM 3 WEEK 3
# VISITNUM 4 WEEK 6
# VISITNUM 5 WEKK 8
weeks=c(0,3,6,8,16,24)
##create week variables for each predictors
for(variable_name in lblist){
  for(i in weeks){
    dat[1,paste(variable_name,i,sep="_")]=NA
  }
}

#Fill in entries
rawlb_week8=rawlb_3[rawlb_3$VISITNUM<6,]
for(currentID in medicated){
    index = which(medicated==currentID)
    templb=rawlb_week8[rawlb_week8$DUSUBJID==currentID,]
    if(nrow(templb)==0){
      next
    }
    for(j in 1:nrow(templb)){
      variable_name = as.character(templb$LBTEST[j])
      if(!(variable_name%in%lblist)){
        next
      }
      time = weeks[templb$VISITNUM[j]-1]
      if(length(time)==0){
        next
      }
      if(!time%in% weeks){
        next
      }
      value = templb$LBSTRESN[j]
      if(is.na(value)){
        next
      }
      dat[index,paste(variable_name,time,sep="_")]=value
    }
}
###Week 16 labs
summary(rawlb_3$VISITNUM[rawlb_3$LBDY<18 & rawlb_3$LBDY>14])
summary(rawlb_3$LBDY[rawlb_3$VISITNUM==11])
sum(rawlb_3$LBDY==16,na.rm = T)
##should be visitnum 11 at week 16, but some without visitnum
rawlb_week16=rawlb_3[rawlb_3$LBDY==16,]
for(currentID in medicated){
  index = which(medicated==currentID)
  templb=rawlb_week16[rawlb_week16$DUSUBJID==currentID,]
  if(nrow(templb)==0){
    next
  }
  time = 16
  for(j in 1:nrow(templb)){
    variable_name = as.character(templb$LBTEST[j])
    if(!(variable_name%in%lblist)){
      next
    }
    value = templb$LBSTRESN[j]
    if(is.na(value)){
      next
    }
    dat[index,paste(variable_name,time,sep="_")]=value
  }
}
###Week 24 labs
summary(rawlb_3$VISITNUM[rawlb_3$LBDY<26 & rawlb_3$LBDY>22])
summary(rawlb_3$LBDY[rawlb_3$VISITNUM==13])
sum(rawlb_3$LBDY==24,na.rm = T)
##should be visitnum 13 at week 24, but some without vistinum
rawlb_week24=rawlb_3[rawlb_3$LBDY==24,]
for(currentID in medicated){
  index = which(medicated==currentID)
  templb=rawlb_week24[rawlb_week24$DUSUBJID==currentID,]
  if(nrow(templb)==0){
    next
  }
  time = 24
  for(j in 1:nrow(templb)){
    variable_name = as.character(templb$LBTEST[j])
    if(!(variable_name%in%lblist)){
      next
    }
    value = templb$LBSTRESN[j]
    if(is.na(value)){
      next
    }
    dat[index,paste(variable_name,time,sep="_")]=value
  }
}
##LBdone Save object
save(dat,file="dat.Rdata")


##Merging PC
names(rawpc_3)
sum(table(rawpc_3$PCDY[rawpc_3$VISITNUM==5]))

##value PCSTRESN
##time PCDY
rawpc_week8=rawpc_3[rawpc_3$VISITNUM<6,]
for(currentID in medicated){
  index = which(medicated==currentID)
  temppc=rawpc_week8[rawpc_week8$DUSUBJID==currentID,]
  if(nrow(temppc)==0){
    next
  }
  for(j in 1:nrow(temppc)){
    variable_name = as.character(temppc$PCTEST[j])
    time = weeks[temppc$VISITNUM[j]-1]
    if(length(time)==0){
      next
    }
    if(!time%in% weeks){
      next
    }
    value = temppc$PCSTRESN[j]
    dat[index,paste(variable_name,time,sep="_")]=value
  }
}
##PC week 16 week 24
rawpc_week16=rawpc_3[rawpc_3$PCDY==16,]
for(currentID in medicated){
  index = which(medicated==currentID)
  temppc=rawpc_week16[rawpc_week16$DUSUBJID==currentID,]
  if(nrow(temppc)==0){
    next
  }
  time = 16
  for(j in 1:nrow(temppc)){
    variable_name = as.character(temppc$PCTEST[j])
    value = temppc$PCSTRESN[j]
    if(is.na(value)){
      next
    }
    dat[index,paste(variable_name,time,sep="_")]=value
  }
}

##should be visitnum 13 at week 24, but some without vistinum
rawpc_week24=rawpc_3[rawpc_3$PCDY==24,]
for(currentID in medicated){
  index = which(medicated==currentID)
  temppc=rawpc_week24[rawpc_week24$DUSUBJID==currentID,]
  if(nrow(temppc)==0){
    next
  }
  time = 24
  for(j in 1:nrow(temppc)){
    variable_name = as.character(temppc$PCTEST[j])
    value = temppc$PCSTRESN[j]
    if(is.na(value)){
      next
    }
    dat[index,paste(variable_name,time,sep="_")]=value
  }
}









summary(dat)
##Merging DM
##DM AGE SEX RACE ETHNIC ACTARM ACTARMCD
sum(medicated==rawdm_3$DUSUBJID)
sum(dat$ID==rawdm_3$DUSUBJID)
##can directly attach
names(rawdm_3)
dat[,"age"]=rawdm_3$AGE
dat[,"sex"]=rawdm_3$SEX
dat[,"race"]=rawdm_3$RACE
dat[,"ethnic"]=rawdm_3$ETHNIC
dat[,"ACTARM"]=rawdm_3$ACTARM
dat[,"ACTARMCD"]=rawdm_3$ACTARMCD





##Merging XA
##create variables
names(rawxa_3)
XAvar=unique(rawxa_3$XATEST)
for(varname in XAvar){
  dat[,paste(varname,-1:num_weeks,sep="_")]=NA
}
rawxa_week8 = rawxa_3[rawxa_3$XADY<(num_weeks+1) & rawxa_3$XADY>=-1,]
for(currentID in medicated){
  index = which(medicated==currentID)
  tempxa=rawxa_week8[rawxa_week8$DUSUBJID==currentID,]
  if(nrow(tempxa)==0){
    next
  }
  for(j in 1:nrow(tempxa)){
    variable_name = as.character(tempxa$XATEST[j])
    time = weeks[tempxa$VISITNUM[j]-1]
    if(length(time)==0){
      next
    }
    if(!time%in% weeks){
      next
    }
    value = tempxa$XASTRESN[j]
    dat[index,paste(variable_name,time,sep="_")]=value
  }
}

##Merging CM
CMcodes=unique(rawcm_3$CMDECOD)
## Look for Azathioprine, 6-mercaptopurine, methotrexate, mycophenolate
"AZATHIOPRINE" %in% CMcodes
##TRUE
"MERCAPTOPURINE" %in% CMcodes
##TRUE
"METHOTREXATE" %in% CMcodes
##TRUE
"MYCOPHENOLATE MOFETIL" %in% CMcodes
##TRUE
target=c("AZATHIOPRINE","MERCAPTOPURINE","METHOTREXATE","MYCOPHENOLATE MOFETIL")
## name CMDECOD
## time CMSTDY
## value CMOCCUR  Yes or No or  "", "" means continuously using
table(rawcm_3$CMOCCUR)
## Only record the ones before week 44
for(currentID in medicated){
  index = which(medicated==currentID)
  tempcm=rawcm_3[rawcm_3$DUSUBJID==currentID,]
  if(nrow(tempcm)==0){
    next
  }
  for(j in 1:nrow(tempcm)){
    variable_name = as.character(tempcm$CMDECOD[j])
    time = tempcm$CMSTDY[j]
    if(is.na(time)){
      next
    }
    value = tempcm$CMOCCUR[j]
    if(variable_name%in%target && time<44){
      ##yes existing
      if(is.null(dat[index,variable_name])||is.na(dat[index,variable_name])){
        if(value==""||value=="Y"){
          dat[index,variable_name]="Y"
        }
        else{
          dat[index,variable_name]="N"
        }
      }
      else if(dat[index,variable_name]=="Y"){
        next
      }
      else if(value==""||value=="Y"){
        dat[index,variable_name]="Y"
      }
    }
  }
}

save(dat,file="dat.Rdata")


##Further cleaning
NAs= c()
for(i in 1:ncol(dat)){
  NAs=c(NAs,sum(is.na(dat[,i])))
}
NAs
names(dat)[NAs==741]


table(rawpc_3$VISITNUM)
rawpc_3=rawpc_3[rawpc_3$VISITNUM==5,]
cnto8 = cbind(rawpc_3$DUSUBJID, rawpc_3$PCSTRESN)
save(cnto8,file="cnto8.Rdata")
