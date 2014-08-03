#clean.R
#files needed: tab-delim files in 'e8DataPackage_080314' folder

#########################################################
#load functions
#source('leeFunctions.R')

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
#View(sd_dic) #there is one empty column to the right that needs to be chopped off.
s_dic<-sd_dic[,-grep('X',colnames(sd_dic))]
#View(s_dic) #looks good

#clean veg data
#View(vd) #there are 3 empty rows at the bottom that need to get chopped off.
v<-vd[!is.na(vd[,1]),]
#View(v) #looks good
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
  data$plotname<-paste(data$site,data$rep,sep="_") #make 'plotname'
  data$plothalfname<-paste(data$site,data$rep,data$inv,sep="_") #make 'plothalfname'  
  return(data)
}

#function to add 'plotname' and 'plothalfname' to data directories
AddNames<-function(data_dic){
  numvars<-dim(data_dic)[2]-1 #number of variables
  n2<-numvars+1
  addvar1<-c('Plot Name','plotname','A3_A1',rep(NA,4),'Pasted character string to identify unique plots',NA)
  addvar2<-c('Plot Half Name','plothalfname','A3_A1_A1',rep(NA,4),'Pasted character string to identify unique plot halves',NA)
  data_dic<-cbind(data_dic,addvar1,addvar2)
  colnames(data_dic)<-c(colnames(data_dic)[1:n2],paste('v',numvars+1, sep=''),paste('v',numvars+2, sep=''))
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
data$plotname<-paste(data$site,data$rep,sep="_") #make 'plotname'
numvars<-dim(data_dic)[2]-1 #number of variables
n2<-numvars+1
addvar1<-c('Plot Name','plotname','A3_A1',rep(NA,4),'Pasted character string to identify unique plots',NA)
data_dic<-cbind(data_dic,addvar1)
colnames(data_dic)<-c(colnames(data_dic)[1:n2],paste('v',numvars+1, sep=''))
str(data) #looks good
str(data_dic) #looks good
p<-data
p_dic<-data_dic

#plot tree data





#timeline data





data$total<-data$mv+data$nat

