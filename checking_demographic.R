
setwd("~/raw_tables")
load("dat.Rdata")

load("dat_allCRP.Rdata")
##medicated cohort demographic

mean(dat$age)
sd(dat$age)
summary(dat$sex)
summary(dat$race)


median(dat$`C Reactive Protein_0`,na.rm = T)
summary(dat$`C Reactive Protein_0`)

mean(dat_allCRP$age)
sd(dat_allCRP$age)
summary(dat_allCRP$sex)
summary(dat_allCRP$race)
median(dat_allCRP$`C Reactive Protein_0`)
summary(dat_allCRP$`C Reactive Protein_0`)


##CM
table(dat$MERCAPTOPURINE)
table(dat$METHOTREXATE)
table(dat$`MYCOPHENOLATE MOFETIL`)
table(dat$AZATHIOPRINE)
cm=read.csv("cm3.csv")
cm$DUSUBJID = sapply(cm$DUSUBJID,substr,start=17,stop=30)
summary(cm$CMCLAS)
all(dat_allCRP$ID %in% dat$ID)
table(cm$CMOCCUR[cm$CMCLAS=="GLUCOCORTICOIDS"])
table(cm$CMOCCUR[cm$CMCLAS=="CORTICOSTEROIDS FOR SYSTEMIC USE"])
table(cm$CMOCCUR[cm$CMCLAS=="TUMOR NECROSIS FACTOR ALPHA (TNF-) INHIBITORS"])
table(cm$CMOCCUR[cm$CMCLAS=="ANTINEOPLASTIC AND IMMUNOMODULATING AGENTS"])


gluco =cm[cm$CMCLAS=="GLUCOCORTICOIDS",]
glucoID = unique(gluco$DUSUBJID)
sum(glucoID%in%dat_allCRP$ID)
sum(glucoID%in%dat$ID)

cortico = cm[cm$CMCLAS=="CORTICOSTEROIDS FOR SYSTEMIC USE",]
length(unique(cortico$DUSUBJID))
table(cortico$CMOCCUR)


length(unique(gluco$DUSUBJID))

cm_dat = cm[cm$DUSUBJID%in%dat$ID,]
cm_allCRP = cm[cm$DUSUBJID%in%dat_allCRP$ID,]
cmlist = sort(unique(cm$CMCLAS))
cmprop_dat = data.frame("CM"=cmlist)
cmprop_dat$dat = 0
cmprop_dat$allcrp = 0
##create csv for all CM
for(i in 1:length(cmlist)){
  cmname = cmlist[i]
  cmprop_dat$dat[i]=sum(cm$CMOCCUR[cm$CMCLAS==cmname & cm$DUSUBJID%in%dat$ID]=="Y")/741
  if(cmprop_dat$dat[i]==0){
    cmprop_dat$dat[i]=length(cm$CMOCCUR[cm$CMCLAS==cmname & cm$DUSUBJID%in%dat$ID])/741
  }
  cmprop_dat$allcrp[i]=sum(cm$CMOCCUR[cm$CMCLAS==cmname & cm$DUSUBJID%in%dat_allCRP$ID]=="Y")/401
  if(cmprop_dat$allcrp[i]==0){
    cmprop_dat$allcrp[i]=length(cm$CMOCCUR[cm$CMCLAS==cmname & cm$DUSUBJID%in%dat_allCRP$ID])/401
  }
}
table(cm$CMOCCUR[cm$CMCLAS=="AMINO ACIDS"])
write.csv(cmprop_dat,file="CMprop.csv")
##"GLUCOCORTICOIDS" "IMMUNOSUPPRESSANTS"

##weight in QS
qs=read.csv("qs.csv")
qs$DUSUBJID = sapply(qs$DUSUBJID,substr,start=17,stop=30)
qs = qs[qs$DUSUBJID%in% dat$ID,]
qs = qs[qs$QSTEST=="Weight"|qs$QSTEST=="Standard Weight",]
qs = qs[qs$QSDY<=1,]
summary(qs$QSSTRESN[qs$QSTEST=="Weight"])
sd((qs$QSSTRESN[qs$QSTEST=="Weight"]))
hist(qs$QSSTRESN[qs$QSTEST=="Weight"])
summary(qs$QSSTRESN[qs$QSTEST=="Standard Weight"])
sd((qs$QSSTRESN[qs$QSTEST=="Standard Weight"]))
hist(qs$QSSTRESN[qs$QSTEST=="Standard Weight"])

summary(qs$QSSTRESN[qs$QSTEST=="Weight"&qs$DUSUBJID%in%dat_allCRP$ID])
sd((qs$QSSTRESN[qs$QSTEST=="Weight"&qs$DUSUBJID%in%dat_allCRP$ID]))
hist(qs$QSSTRESN[qs$QSTEST=="Weight"&qs$DUSUBJID%in%dat_allCRP$ID])
summary(qs$QSSTRESN[qs$QSTEST=="Standard Weight"&qs$DUSUBJID%in%dat_allCRP$ID])
sd((qs$QSSTRESN[qs$QSTEST=="Standard Weight"&qs$DUSUBJID%in%dat_allCRP$ID]))
hist(qs$QSSTRESN[qs$QSTEST=="Standard Weight"&qs$DUSUBJID%in%dat_allCRP$ID])



### Disease location can
suppmh = read.csv("suppmh.csv")
suppmh$DUSUBJID = sapply(suppmh$DUSUBJID,substr,start=17,stop=30) 
suppmh = suppmh[suppmh$DUSUBJID%in%dat$ID,]
table(suppmh$QNAM,suppmh$QLABEL)
##QNAM  MHPCOLON--Colon   MHHIST--complications indicating penetrating CD
##MHEXINT -- extra intestinal manifestations
##MHILEUM--Ileum     MHPERANL--Perianal
##MHPRXMLS--- Prxml small intestine/stomach/esophagus
summary(suppmh$QVAL[suppmh$QNAM=='MHPCOLON'])
summary(suppmh$QVAL[suppmh$QNAM=='MHILEUM'])
summary(suppmh$QVAL[suppmh$QNAM=='MHEXINT'])
summary(suppmh$QVAL[suppmh$QNAM=='MHPRXMLS'])
summary(suppmh$QVAL[suppmh$QNAM=='MHPERANL'])

summary(suppmh$QVAL[suppmh$QNAM=='MHPCOLON'&suppmh$DUSUBJID%in%dat_allCRP$ID])
summary(suppmh$QVAL[suppmh$QNAM=='MHILEUM'&suppmh$DUSUBJID%in%dat_allCRP$ID])
summary(suppmh$QVAL[suppmh$QNAM=='MHEXINT'&suppmh$DUSUBJID%in%dat_allCRP$ID])
summary(suppmh$QVAL[suppmh$QNAM=='MHPRXMLS'&suppmh$DUSUBJID%in%dat_allCRP$ID])
summary(suppmh$QVAL[suppmh$QNAM=='MHPERANL'&suppmh$DUSUBJID%in%dat_allCRP$ID])

##colon only , ileum only, both 
table(suppmh$QVAL[suppmh$QNAM=='MHPCOLON'],suppmh$QVAL[suppmh$QNAM=='MHILEUM'])


##all crp
table(suppmh$QVAL[suppmh$QNAM=='MHPCOLON'&suppmh$DUSUBJID%in%dat_allCRP$ID],suppmh$QVAL[suppmh$QNAM=='MHILEUM'&suppmh$DUSUBJID%in%dat_allCRP$ID])


###Disease Duration
ae = read.csv("ae.csv")
ae$DUSUBJID = sapply(ae$DUSUBJID,substr,start=17,stop=30)
temp = unique(ae$AEDECOD)
ae = ae[ae$AEDECOD=="Crohn's disease",]
length(unique(ae$DUSUBJID))
ae = ae[ae$DUSUBJID%in%dat$ID,]
summary(ae$AEENDY - ae$AESTDY)
hist(ae$AEENDY - ae$AESTDY)
sd(ae$AEENDY - ae$AESTDY,na.rm=T)

ae_all = ae[ae$DUSUBJID%in%dat_allCRP$ID,]
summary(ae_all$AEENDY - ae_all$AESTDY)
sd(ae_all$AEENDY - ae_all$AESTDY,na.rm=T)
sum(unique(ae$DUSUBJID)%in%dat_allCRP$ID)
sum(unique(ae$DUSUBJID)%in%dat$ID)

summary(dat_allCRP$CRP_response)
summary(dat_new_highCRP$CRP_response)
dat_new_highCRP$CRP_response[dat_new_highCRP$CRP_response==999]=NA
summary(dat_new_highCRP$CRP_response<5)

