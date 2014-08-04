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
library(ggplot2)

str(sv)
sv$year<-as.factor(sv$year)

soil_shortname<-c('noi','toti','nitrifd','minzd','soilmoi')
soil_longname<-c('Nitrate (ug/G) in Reference Plot','Total Inorganic N (ug/G) in Reference Plot','Nitrification (ug/G*d) in Reference Plot','Mineralization (ug/G*d) in Reference Plot','Soil Moisture (%) in Reference Plot')

for (i in 1:length(soil_shortname)){
  plot(x = sv[,soil_shortname[i]], y = sv$mv,
       xlab='Microstegium biomass (g)', ylab=soil_longname[i],
       type='n')
  points # add for each year
  line # add if lm has a signif slope
}

soil_longname[i]


i<-1
sv[,soil_shortname[i]]





