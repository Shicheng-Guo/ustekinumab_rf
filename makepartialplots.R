setwd("~/RF_build")
week= 0
load(file=paste("week",week,"partialplot_noHema_weight.Rdata",sep=""))
if(week==8){
  varnames[32]="CNTO_1_Ratio_CRP"
}
if(week==0){
  varnames[14]="CNTO_1"
}
setwd(paste("~/results/partialplots_noHema_weight/week",week,sep=""))
for (i in 1:length(length_v)){
  tiff(height=7,width=7,units="in",res=600,
       filename=paste(varnames[i],".tiff",sep=""),compression = "zip")
  plot(uniqueV[[i]],est[[i]],xlab=varnames[i],ylab="Success Probability",
       main=varnames[i])
  dev.off()
  gc()
}

