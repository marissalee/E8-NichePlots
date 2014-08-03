#clean.R
#files needed: tab-delim files in 'e8DataPackage_080314' folder

#########################################################
#load functions
source('leeFunctions.R')

#########################################################
#load data

#soil data (sd)
sd<-read.table("e8DataPackage_080314/e8_plothalfSoilData.txt",header=TRUE,sep="\t")
sd_dic<-read.table("e8DataPackage_080314/e8_plothalfSoilData_dictionary.txt",header=TRUE,sep="\t")

#veg data (vd)
vd<-read.table("e8DataPackage_080314/e8_plothalfVegData.txt",header=TRUE,sep="\t")
vd_dic<-read.table("e8DataPackage_080314/e8_plothalfVegData_dictionary.txt",header=TRUE,sep="\t")

#plot location data (pd)
pd<-read.table("e8DataPackage_080314/e8_plotLoc.txt",header=TRUE,sep="\t")
pd_dic<-read.table("e8DataPackage_080314/e8_plotLoc_dictionary.txt",header=TRUE,sep="\t")

#plot tree data (td)
pd<-read.table("e8DataPackage_080314/e8_plotTrees.txt",header=TRUE,sep="\t")
pd_dic<-read.table("e8DataPackage_080314/e8_plotTrees_dictionary.txt",header=TRUE,sep="\t")

#timeline data
td<-read.table("e8DataPackage_080314/e8_timeline.txt",header=TRUE,sep="\t")

#########################################################
#clean and reshape data
data$h<-as.factor(data$h) #year needs to be a factor
data$plothalfid<-as.factor(data$plothalfid) #plothalfid needs to be a factor
data$plotid<-as.factor(data$plotid) #plotid needs to be a factor
data$plotname<-paste(data$site,data$rep,sep=".")
data$total<-data$mv+data$nat

