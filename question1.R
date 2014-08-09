#question1.R
#files needed: tab-delim files in 'e8DataPackage_080314_clean' folder
#question1: How does invader biomass vary across resource availability?

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

#########################################################
#Invader biomass vs native subplot resource availability... color points by year

#Make dataframes to plot 1) nhi, noi and 2) toti
colnames(sv)
sv1<-sv[,c(15,1,7,8,9,17)]
sv.n<-melt(sv1, measure.var=c('nhi','noi'))
sv.n$year<-as.factor(sv.n$year)
head(sv.n) 
sv.t<-melt(sv1, measure.var=c('toti'))
sv.t$year<-as.factor(sv.t$year)
head(sv.t)

#Fxn to rename variables for ggplot
PrettyVaribNames<-function(df, prettynames, vars){
  prettydf <- df
  for (i in 1:length(vars)){
    levels(prettydf$variable)[levels(prettydf$variable)==vars[i]] <- prettynames[i]
  }
  return (prettydf)
}

#Provide pretty varibale names using fxn
prettynames<-c("Ammonium (ug/G)","Nitrate (ug/G)","Total Inorganic N (ug/G)","Nitrification (ug/G*d)","Mineralization (ug/G*d)","Soil Moisture (%)") #for all
vars<-c('nhi','noi','toti','nitrifd','minzd','soilmoi') #for all
sv.n.pretty<-PrettyVaribNames(sv.n, prettynames, vars) #for plot with nhi and noi
sv.t.pretty<-PrettyVaribNames(sv.t, prettynames, vars) #for plot with toti

#Ammonium and Nitrate
p1.n<-ggplot(sv.n.pretty,aes(sv.n.pretty, x=value, y = mv)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Microstegium biomass (g)") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=0, lty=2, color=1)
#p1.n + geom_text(aes(label=plotname),hjust=1.1, size=3)
p1.n + geom_text(aes(label=ifelse((value>4*IQR(value)|mv>4*IQR(mv)),as.character(plotname)," "), hjust=1.1, size=3)) #label outliers

#Total inorganic N
p1.t<-ggplot(sv.t.pretty,aes(sv.t.pretty, x=value, y = mv)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Microstegium biomass (g)") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=0, lty=2, color=1)
#p1 + geom_text(aes(label=plotname),hjust=1.1, size=3)
p1.t + geom_text(aes(label=ifelse((value>4*IQR(value)|mv>4*IQR(mv)),as.character(plotname)," "), hjust=1.1, size=3)) #label outliers


##########################
#linear model
sv12<-sv1[sv1$year=='2012',]
sv13<-sv1[sv1$year=='2013',]

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

colnames(sv12)
stcol<-3
endcol<-5
#Fxn to calc pvals and coefs
AnotherFxn<-function(data, stcol, endcol){
  xvars<-colnames(data)[stcol:endcol]
  store<-numeric(0)
  i<-0
  for (i in 1:length(xvars)){
    x<-data[,i+stcol-1]
    y<-data[,'mv']
    df<-data.frame(x,y)
    fit<-lm(y ~ x, df)
    pvals<-GetPvals(fit)
    coefs<-GetCoefs(fit)
    row<-c(pvals,coefs)
    store<-rbind(store,row)
  }
  return(store)
} #need to check this.

#Do it!
result12<-AnotherFxn(sv12, stcol,endcol)
colnames(result12)<-c('pvalx','r2','int', 'coefx')
result12

result13<-AnotherFxn(sv13, stcol, endcol)
colnames(result13)<-c('pvalx','r2','int', 'coefx')
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
tmp3[,3:5]<-round(tmp3[,3:5], digits=2)
tab<-tmp3
tab
