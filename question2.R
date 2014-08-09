#question2.R
#files needed: tab-delim files in 'e8DataPackage_080314_clean' folder
#question2: How does reference nutrient availability correlate with invaded nutrient availability across sites?

library(ggplot2); theme_set(theme_bw())
library(dplyr)
library(reshape2)
library(doBy)
#########################################################
#load data

#soil data (sd)
sd<-read.table("e8DataPackage_080314_clean/e8_plothalfSoilData_clean.txt",header=TRUE,sep="\t")
sd_dic<-read.table("e8DataPackage_080314_clean/e8_plothalfSoilData_dictionary_clean.txt",header=TRUE,sep="\t")

#veg data (vd)
vd<-read.table("e8DataPackage_080314_clean/e8_plothalfVegData_clean.txt",header=TRUE,sep="\t")
vd_dic<-read.table("e8DataPackage_080314_clean/e8_plothalfVegData_dictionary_clean.txt",header=TRUE,sep="\t")

#########################################################
#Make dataframe to plot: invader biomass vs native subplot resource availability

#pull out the top depth
sdt<-filter(sd,depth == "T") 

#pick out the columns of interest
colnames(sdt)
sdf<-sdt[,c('year','inv','nhi','noi','toti')]

#reshape
colnames(sdf)
sdf1<-melt(sdf, measure.var=c('nhi','noi','toti'))
sdf1$year<-as.factor(sdf1$year)
sdt1N<-filter(sdf1,inv == "N") 
sdt1I<-filter(sdf1,inv == "I") 
sdf1<-data.frame(sdt1N[,c('year','variable')], nat.vals=sdt1N[,'value'], inv.vals=sdt1I[,'value'])
head(sdf)

#rename variables
sdf1s <- sdf1
levels(sdf1s$variable)[levels(sdf1s$variable)=="nhi"] <- "Ammonium (ug/G)"
levels(sdf1s$variable)[levels(sdf1s$variable)=="noi"] <- "Nitrate (ug/G)"
levels(sdf1s$variable)[levels(sdf1s$variable)=="toti"] <- "Total Inorganic N (ug/G)"
levels(sdf1s$variable)[levels(sdf1s$variable)=="nitrifd"] <- "Nitrification (ug/G*d)"
levels(sdf1s$variable)[levels(sdf1s$variable)=="minzd"] <- "Mineralization (ug/G*d)"
levels(sdf1s$variable)[levels(sdf1s$variable)=="soilmoi"] <- "Soil Moisture (%)"

s<-ggplot(sdf1s,aes(sdf1s, x=nat.vals, y = inv.vals)) + 
  geom_point(aes(color=year)) +
  facet_wrap(~variable+year, scales='free_x', ncol=2) +
  xlab("Reference plot value") + ylab("Invaded plot value") +
  geom_smooth(method='lm',se=T) +
  geom_abline(intercept=0, slope=1, lty=2, color=1)
s

s<-ggplot(sdf1s,aes(sdf1s, x=nat.vals, y = inv.vals)) + 
  geom_point(aes(color=year)) +
  facet_wrap(~variable+year, scales='free_x', ncol=2) +
  xlab("Reference plot value") + ylab("Invaded plot value") +
  geom_smooth(method='lm',se=T)
s

#make scales 1:1
#outliers

##########################
#linear model
s12<-sdf[year==2012,]
s13<-sdf[year==2013,]

stcol<-3
endcol<-5

#Fxn to calc pvals and coefs
AnotherFxn2<-function(data, stcol, endcol){
  xvars<-colnames(data)[stcol:endcol]
  store<-numeric(0)
  i<-0
  for (i in 1:length(xvars)){
    dataI<-data[data$inv=='I',]
    dataN<-data[data$inv=='N',]
    x<-dataN[,i+stcol-1]
    y<-dataI[,i+stcol-1]
    df<-data.frame(x,y)
    
    fit<-lm(y ~ x, df) #slope = 0
    pvals<-GetPvals(fit)
    coefs<-GetCoefs(fit)
    
    fit2<-lm(y~x+offset(1*x), df) #slope = 1
    pvals2<-GetPvals(fit2)
    coefs2<-GetCoefs(fit2)
    
    row<-c(pvals,coefs,pvals2,coefs2)
    store<-rbind(store,row)
  }
  return(store)
}

#Do it!
result12<-AnotherFxn2(s12, stcol,endcol)
tmp<-c('pvalx','r2','int', 'coefx')
tmp2<-paste(tmp,"slope1",sep="_")
colnames(result12)<-c(tmp,tmp2) # does slope differ from 0,1
result12

result13<-AnotherFxn2(s13, stcol, endcol)
colnames(result13)<-c(tmp,tmp2) #does slope differ from 0,1
result13

#Make a nice table
varnum<-endcol-stcol+1
year<-c(rep('2012',varnum),rep('2013',varnum))
tmp<-c("Ammonium (ug/G)","Nitrate (ug/G)","Total Inorganic N (ug/G)")
xvar<-rep(tmp,2)
tmp1<-rbind(result12,result13)
tmp2<-data.frame(xvar,year,tmp1)
tmp2
tmp3<-orderBy(~xvar,tmp2)
dim(tmp3)
tmp3[,c(3:6,8:10)]<-round(tmp3[,c(3:6,8:10)], digits=2)
tmp3[,7]<-round(tmp3[,7], digits=4)
tab<-tmp3
tab




