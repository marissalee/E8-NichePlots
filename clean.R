#clean.R
#files needed: tab-delim files in 'e8DataPackage_080314' folder

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
#chop extra rows

#clean soil data
#View(sd) #there are a number of empty columns to the right that need to be chopped off.
s<-sd[,-grep('X',colnames(sd))]
#View(s) #looks good
library(doBy) #for orderBy
s1<-orderBy(~year+plothalfid+inv+depth, s)
s<-s1
#View(sd_dic) #there is one empty column to the right that needs to be chopped off.
s_dic<-sd_dic[,-grep('X',colnames(sd_dic))]
#View(s_dic) #looks good

#clean veg data
#View(vd) #there are 3 empty rows at the bottom that need to get chopped off.
v<-vd[!is.na(vd[,1]),]
#View(v) #looks good
v1<-orderBy(~year+plothalfid+inv, v)
v<-v1
#View(vd_dic) #there are 5 empty columns to the right that need to be chopped off.
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
#verify structure
#make 'year','plothalfid', and 'plotid' factors
#combine 'site' and 'rep' into a character string for plotname
#combine 'site' and 'rep' and 'inv' into a character string for plothalfname
#add new variables to the data dictionary file

#function to make 'year','plothalfid', and 'plotid' factors
MakeFactors<-function(data,vars){
  i<-0
  for (i in 1:length(vars)){
    data[,vars[i]]<-as.factor(data[,vars[i]])
  }
  return(data)
}
vars<-c('year','plothalfid','plotid')

#function to make 'plotname' and 'plothalfname'
MakeNames<-function(data){
  data$plotname<-as.factor(paste(data$site,data$rep,sep="_")) #make 'plotname'
  data$plothalfname<-as.factor(paste(data$site,data$rep,data$inv,sep="_")) #make 'plothalfname'  
  data$yrplotname<-as.factor(paste(data$year,data$site,data$rep,sep="_")) #make 'yrplotname'
  data$yrplothalfname<-as.factor(paste(data$year,data$site,data$rep,data$inv,sep="_")) #make 'yrplothalfname'
  return(data)
}

#function to add 'plotname' and 'plothalfname' to data directories
AddNames<-function(data_dic){
  numvars<-dim(data_dic)[2]-1 #number of variables
  n2<-numvars+1
  addvar1<-c('Plot Name','plotname','A3_A1',rep(NA,4),'Pasted character string to identify unique plots',NA)
  addvar2<-c('Plot Half Name','plothalfname','A3_A1_A1',rep(NA,4),'Pasted character string to identify unique plot halves',NA)
  addvar3<-c('Year Plot Name','yrplotname','I4_A3_A1',rep(NA,4),'Pasted character string to identify unique plots by year',NA)
  addvar4<-c('Year Plot Half Name','yrplothalfname','I4_A3_A1_A1',rep(NA,4),'Pasted character string to identify unique plot halves by year',NA)
  data_dic<-cbind(data_dic,addvar1,addvar2,addvar3,addvar4)
  colnames(data_dic)<-c(colnames(data_dic)[1:n2],paste('v',numvars+1, sep=''),paste('v',numvars+2, sep=''),paste('v',numvars+3, sep=''),paste('v',numvars+4, sep=''))
  return(data_dic)
}

#soil data
data<-s
data_dic<-s_dic
vars<-c('year','plothalfid','plotid')
data<-MakeFactors(data, vars)
data<-MakeNames(data)
data_dic<-AddNames(data_dic)
str(data) #looks good
str(data_dic) #looks good
s<-data
s_dic<-data_dic

#veg data
data<-v
data_dic<-v_dic
vars<-c('year','plothalfid','plotid')
data<-MakeFactors(data, vars)
data<-MakeNames(data)
data_dic<-AddNames(data_dic)
str(data) #looks good
str(data_dic) #looks good
v<-data
v_dic<-data_dic

#plot location data
data<-pd
data_dic<-pd_dic
vars<-c('plothalfid','plotid')
data<-MakeFactors(data, vars)
data$plotname<-as.factor(paste(data$site,data$rep,sep="_")) #make 'plotname'
numvars<-dim(data_dic)[2]-1 #number of variables
n2<-numvars+1
addvar1<-c('Plot Name','plotname','A3_A1',rep(NA,4),'Pasted character string to identify unique plots',NA)
data_dic<-cbind(data_dic,addvar1)
colnames(data_dic)<-c(colnames(data_dic)[1:n2],paste('v',numvars+1, sep=''))

#add a column to identify which plots are not in the forest understory
notreeplots<-c('G2_A','G48_D','G13_C')
data$understory<-rep('Yes',length(data$plotname))
data[data$plotname %in% notreeplots,'understory']<-'No'
numvars<-dim(data_dic)[2]-1 #number of variables
n2<-numvars+1
addvar1<-c('Understory Plot','understory','A3',rep(NA,4),'The plot is in the forest understory (Yes) or not (No)',NA)
data_dic<-cbind(data_dic,addvar1)
colnames(data_dic)<-c(colnames(data_dic)[1:n2],paste('v',numvars+1, sep=''))
data$understory<-as.factor(data$understory)
str(data) #looks good
str(data_dic) #looks good
p<-data
p_dic<-data_dic

#plot tree data
data<-td
data_dic<-td_dic
vars<-c('plotid')
data<-MakeFactors(data, vars)
data$plotname<-as.factor(paste(data$site,data$rep,sep="_")) #make 'plotname'
numvars<-dim(data_dic)[2]-1 #number of variables
n2<-numvars+1
addvar1<-c('Plot Name','plotname','A3_A1',rep(NA,4),'Pasted character string to identify unique plots',NA)
data_dic<-cbind(data_dic,addvar1)
colnames(data_dic)<-c(colnames(data_dic)[1:n2],paste('v',numvars+1, sep=''))
str(data)
str(data_dic)
t<-data
t_dic<-data_dic


#########################################################
#make a total understory biomass variable in v (veg) and add it to the data directory
v$total<-v$mv+v$nat
newmin<-min(v$total)
newmax<-max(v$total)
newavg<-mean(v$total)
data_dic<-v_dic
numvars<-dim(data_dic)[2]-1;n2<-numvars+1 #number of variables

addvar1<-c('Total understory plant biomass (g)','total','F2.2',
           'g/.1875m2', round(newmin,2), round(newmax,2), round(newavg,2),
           'Dry weight (g) of total understory plant biomass that was harvested from 3-0.25mx0.25m quadrats',
           'Addition of directly measured variables: mv+nat=total')
data_dic<-cbind(data_dic,addvar1)
colnames(data_dic)<-c(colnames(data_dic)[1:n2],paste('v',numvars+1, sep=''))
v_dic<-data_dic


#########################################################
#export clean files

#soil data (sm)
write.table(s, "e8DataPackage_080314_clean/e8_plothalfSoilData_clean.txt",sep="\t", row.names=F)
write.table(s_dic, "e8DataPackage_080314_clean/e8_plothalfSoilData_dictionary_clean.txt",sep="\t", row.names=F)

#veg data (vm)
write.table(v, "e8DataPackage_080314_clean/e8_plothalfVegData_clean.txt",sep="\t", row.names=F)
write.table(v_dic, "e8DataPackage_080314_clean/e8_plothalfVegData_dictionary_clean.txt",sep="\t", row.names=F)

#plot location data (p)
write.table(p, "e8DataPackage_080314_clean/e8_plotLoc_clean.txt",sep="\t", row.names=F)
write.table(p_dic, "e8DataPackage_080314_clean/e8_plotLoc_dictionary.txt",sep="\t", row.names=F)

#plot tree data (t)
write.table(t, "e8DataPackage_080314_clean/e8_plotTrees_clean.txt",sep="\t", row.names=F)
write.table(t_dic, "e8DataPackage_080314_clean/e8_plotTrees_dictionary_clean.txt",sep="\t", row.names=F)

#timeline data (hd)
write.table(h, "e8DataPackage_080314_clean/e8_timeline_clean.txt",sep="\t", row.names=F)










