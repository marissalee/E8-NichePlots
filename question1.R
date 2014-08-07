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
#Plot: invader biomass vs native subplot resource availability... color points by year
library(ggplot2); theme_set(theme_bw())
str(sv)
sv$year<-as.factor(sv$year)

soil_shortname<-c('noi','toti','nitrifd','minzd','soilmoi')
soil_longname<-c('Nitrate (ug/G) in Reference Plot','Total Inorganic N (ug/G) in Reference Plot','Nitrification (ug/G*d) in Reference Plot','Mineralization (ug/G*d) in Reference Plot','Soil Moisture (%) in Reference Plot')

i<-0
for (i in 1:length(soil_shortname)){
  p<-ggplot(sv,aes(sv[,soil_shortname[i]], y = mv)) + geom_point(aes(color=year)) +
    geom_smooth(aes())+
    xlab(soil_longname[i]) + ylab("Microstegium biomass (g)")
  print(p)
}

i<-0
store<-numeric(0)
for (i in 1:length(soil_shortname)){
  x<-sv[,soil_shortname[i]]
  y<-sv[,'mv']
  year<-sv[,'year']
  fit<-lm(y~x*year)
  pvalx<-summary(fit)$coefficients[2,4]
  pvalyear<-summary(fit)$coefficients[3,4]
  pvalint<-summary(fit)$coefficients[4,4]
  r2<-summary(fit)$r.squared
  row<-c(pvalx, pvalyear, pvalint,r2)
  store<-rbind(store,row)
}
colnames(store)<-c('pvalx','pvalyear','pvalint','r2')
rownames(store)<-soil_shortname
store










