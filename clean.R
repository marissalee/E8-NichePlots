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
td<-read.table("e8DataPackage_080314/e8_plotTrees.txt",header=TRUE,sep="\t")
td_dic<-read.table("e8DataPackage_080314/e8_plotTrees_dictionary.txt",header=TRUE,sep="\t")

#timeline data (hd)
hd<-read.table("e8DataPackage_080314/e8_timeline.txt",header=TRUE,sep="\t")

#########################################################
#chop extra rows, verify structure

#clean soil data
#View(sd) #there are a number of empty columns to the right that need to be chopped off.
s<-sd[,-grep('X',colnames(sd))]
#View(s) #looks good
str(s)

#View(sd_dic) #there is one empty column to the right that needs to be chopped off.
s_dic<-sd_dic[,-grep('X',colnames(sd_dic))]
#View(s_dic) #looks good

#clean veg data
#View(vd) #there are 3 empty rows at the bottom that need to get chopped off.
v<-vd[!is.na(vd[,1]),]
#View(v) #looks good
View(vd_dic) #there are 5 empty columns to the right that need to be chopped off.
v_dic<-vd_dic[,-grep('X',colnames(vd_dic))]
#View(v_dic) #looks good

#clean plot location data
#View(pd) #looks good
#View(pd_dic) #looks good

#clean plot tree data
#View(td) #looks good
#View(td_dic) #looks good

#clean timeline data
#View(hd) #there is one empty column to the right that needs to be chopped off.
h<-hd[,-grep('X',colnames(hd))]
#View(h)

#########################################################


data$h<-as.factor(data$h) #year needs to be a factor
data$plothalfid<-as.factor(data$plothalfid) #plothalfid needs to be a factor
data$plotid<-as.factor(data$plotid) #plotid needs to be a factor
data$plotname<-paste(data$site,data$rep,sep=".")
data$total<-data$mv+data$nat

