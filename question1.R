#question1.R
#files needed: tab-delim files in 'e8DataPackage_080314_clean' folder
#question1: How does invader biomass vary across resource availability?

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
library(dplyr)

#pull out just the invaded rows of data
vdt1<-filter(vd,inv == "I")
#make a df with just 2012 and 2013 data
vdt2<-filter(vdt1,year != "2011")
dim(vdt2)

#pull out just the native rows of data
sdt1<-filter(sd,inv == "N") 
#make a df for the top depth
sdt2<-filter(sdt1,depth == "T") 
dim(sdt2)

#merge the veg and soil dfs by plotid
test<-cbind(sdt2[,'plotid'],vdt2[,'plotid'])
test #are plotid rows aligned? yes.
colnames(sdt2); colnames(vdt2)

sv<-cbind(sdt2[,c(1,3:16)], yrplotname=sdt2[,18], vdt2[,7:11], total=vdt2[,16])
View(sv)

#########################################################
#Invader biomass vs native subplot resource availability... color points by year

library(ggplot2); theme_set(theme_bw())

#reshape
colnames(sv)
sv1<-sv[,c(1,8,9,11,12,13,17)]
library(reshape2)
sv2<-melt(sv1, measure.var=c('noi','toti','nitrifd','minzd','soilmoi'))
sv2$year<-as.factor(sv2$year)

#rename variables
sv2s <- sv2
levels(sv2s$variable)[levels(sv2s$variable)=="noi"] <- "Nitrate (ug/G)"
levels(sv2s$variable)[levels(sv2s$variable)=="toti"] <- "Total Inorganic N (ug/G)"
levels(sv2s$variable)[levels(sv2s$variable)=="nitrifd"] <- "Nitrification (ug/G*d)"
levels(sv2s$variable)[levels(sv2s$variable)=="minzd"] <- "Mineralization (ug/G*d)"
levels(sv2s$variable)[levels(sv2s$variable)=="soilmoi"] <- "Soil Moisture (%)"

s<-ggplot(sv2s,aes(sv2s, x=value, y = mv)) + 
  geom_point(aes(color=year)) +
  facet_wrap(~variable+year, scales='free_x', ncol=2) +
  xlab("Reference plot value") + ylab("Microstegium biomass (g)") +
  geom_smooth(method='lm',se=F)
s


##########################
#linear model
colnames(sv1)
sv12<-sv1[year==2012,]
sv13<-sv1[year==2013,]

#Fxns to use within loop
GetPvals<-function(fit){
  store<-numeric(0)
  
  pvalx<-summary(fit)$coefficients[2,4]
  r2<-summary(fit)$r.squared
  
  row<-c(pvalx, r2)
  store<-rbind(store,row)
  colnames(store)<-c('pvalx','r2')
  
  return(store)
}
GetCoefs<-function(fit){
  store<-numeric(0)
  
  int<-summary(fit)$coefficients[1,1]
  coefx<-summary(fit)$coefficients[2,1]
  
  row<-c(int, coefx)
  store<-rbind(store,row)
  colnames(store)<-c('int', 'coefx')
  
  return(store)
}

#Fxn to calc pvals and coefs
AnotherFxn<-function(data){
  xvars<-colnames(data)[2:6]
  store<-numeric(0)
  i<-0
  for (i in 1:length(xvars)){
    x<-sv1[,i+1]
    y<-sv1[,'mv']
    df<-data.frame(x,y)
    fit<-lm(y ~ x, df)
    pvals<-GetPvals(fit)
    coefs<-GetCoefs(fit)
    row<-c(pvals,coefs)
    store<-rbind(store,row)
  }
  return(store)
}

#Do it!
result12<-AnotherFxn(sv12)
colnames(result12)<-c('pvalx','r2','int', 'coefx')
result12

result13<-AnotherFxn(sv13)
colnames(result13)<-c('pvalx','r2','int', 'coefx')
result13
