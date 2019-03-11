setwd("~/raw_tables")
load("dat.Rdata")

# ##combining measurement between weeks

dat_new = dat
save(dat_new,file="dat_new.Rdata")
##Calculate slope in old way, provide max-slope and avg-slope
#C Reactive Protein_0/3/6/8
dat_new[,"min_slope"]=NA
dat_new[,"max_slope"]=NA
dat_new[,"mean_slope"]=NA
getvarname = paste("C Reactive Protein",c(0,3,6,8),sep="_")
week_sep = c(3,3,2)
for(i in 1:nrow(dat_new)){
  ##get the CRP
  temp_CRP=dat_new[i,getvarname]
  ##calculate slopes
  slope_temp = (temp_CRP[-1]-temp_CRP[-length(temp_CRP)])/week_sep
  max_slope = max(slope_temp,na.rm=T)
  min_slope = min(slope_temp,na.rm=T)
  mean_slope=mean(unlist(slope_temp),na.rm=T)
  if(is.finite(min_slope)){
    dat_new[i,"min_slope"]=min_slope
  }
  if(is.finite(max_slope)){
    dat_new[i,"max_slope"]=max_slope
  }
  if(is.finite(mean_slope)){
    dat_new[i,"mean_slope"]=mean_slope
  }
}
save(dat_new,file="dat_new.Rdata")



##clean out variables with too many NAs 
load("dat_new.Rdata")
NAs= c()
for(i in 1:ncol(dat_new)){
  NAs=c(NAs,sum(is.na(dat_new[,i])))
}
NAs
names(dat_new)[which(NAs>730)]
dat_new = dat_new[,which(NAs<730)]
## New NAs
NAs= c()
for(i in 1:ncol(dat_new)){
  NAs=c(NAs,sum(is.na(dat_new[,i])))
}
NAs
names(dat_new)[which(NAs>600)]
names(dat_new)[NAs>100]
##

##Change response NA to 999
dat_new[is.na(dat_new[,"CRP_response"]),"CRP_response"]=999
save(dat_new,file="dat_new.Rdata")

##change variables with number of levels less than 10 to factor
uniquelvs = c()
for(i in 1:ncol(dat_new)){
  uniquelvs[i]=length(unique(dat_new[,i]))
}
cat= which(uniquelvs<10)
for(i in cat){
  dat_new[,i]=as.factor(dat_new[,i])
}
##Separate for CDs
dat_new_CD = dat_new[!is.na(dat_new[,"CD3_0"]),]
save(dat_new_CD,file="dat_new_CD.Rdata")
dat_new_noCD = dat_new[is.na(dat_new[,"CD3_0"]),]
save(dat_new_noCD,file="dat_new_noCD.Rdata")


##Delete the CD variables
CDcolumns=which(substr(names(dat_new),1,2)=="CD")
dat_new_backup=dat_new
save(dat_new_backup,file="dat_new_CDundeleted.Rdata")
dat_new = dat_new[,-CDcolumns]
dat_new_CD=dat_new_CD[,-CDcolumns]
dat_new_noCD=dat_new_noCD[,-CDcolumns]



NAs= c()
for(i in 1:ncol(dat_new)){
  NAs=c(NAs,sum(is.na(dat_new[,i])))
}
NAs
names(dat_new)[which(NAs>300)]

##

##With CRP_0>5  CRP ref high =10
sum(dat_new[,"C Reactive Protein_0"]>5,na.rm=T)
sum(dat_new[,"CRP_response"]>5,na.rm=T)

##filter out initial elevated CRP
dat_new_highCRP = dat_new[dat_new$`C Reactive Protein_0`>=5 & !is.na(dat_new$`C Reactive Protein_0`),]
dat_new_week1624=dat_new_highCRP
save(dat_new_week1624,file="dat_week1624.Rdata")

##delete varaibles with too many missing values
NAs= c()
for(i in 1:ncol(dat_new_highCRP)){
  NAs=c(NAs,sum(is.na(dat_new_highCRP[,i])))
}
NAs
names(dat_new_highCRP)[(NAs>200)]
dat_new_highCRP=dat_new_highCRP[,NAs<50]
NAs=c()
for(i in 1:ncol(dat_new_highCRP)){
  NAs=c(NAs,sum(is.na(dat_new_highCRP[,i])))
}
NAs
save(dat_new_highCRP,file="dat_new_highCRP.Rdata")
names(dat_new_highCRP)

