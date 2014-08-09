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
sdf<-sdt[,c('plotname','year','inv','nhi','noi','toti')]

#Make dataframes to plot 1) nhi, noi and 2) toti
colnames(sdf)
##for nhi and noi
sdf.n<-melt(sdf, measure.var=c('nhi','noi'))
sdf.n$year<-as.factor(sdf.n$year)
sdf.nN<-filter(sdf.n,inv == "N") 
sdf.nI<-filter(sdf.n,inv == "I") 
s.n<-data.frame(sdf.nN[,c('plotname','year','variable')], nat.vals=sdf.nN[,'value'], inv.vals=sdf.nI[,'value'])
head(s.n)
##for toti
sdf.t<-melt(sdf, measure.var=c('toti'))
sdf.t$year<-as.factor(sdf.t$year)
sdf.tN<-filter(sdf.t,inv == "N") 
sdf.tI<-filter(sdf.t,inv == "I") 
s.t<-data.frame(sdf.tN[,c('plotname','year','variable')], nat.vals=sdf.tN[,'value'], inv.vals=sdf.tI[,'value'])
head(s.t)

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
s.n.pretty<-PrettyVaribNames(s.n, prettynames, vars) #for plot with nhi and noi
s.t.pretty<-PrettyVaribNames(s.t, prettynames, vars) #for plot with toti

#Ammonium and Nitrate
##slope = 0
p2.n<-ggplot(s.n.pretty,aes(s.n.pretty, x=nat.vals, y = inv.vals)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Invaded plot value") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=1, lty=2, color=1)
p2.n
#p2.n + geom_text(aes(label=plotname),hjust=1.1, size=3)
p2.n + geom_text(aes(label=ifelse((nat.vals>4*IQR(nat.vals)|inv.vals>4*IQR(inv.vals)),as.character(plotname)," "), hjust=1.1, size=3)) #label outliers
##slope = 1
s.n.pretty1<-transform(s.n.pretty,inv.vals=inv.vals-s.n.pretty$nat.vals)
p3.n<-ggplot(s.n.pretty1,aes(s.n.pretty1, x=nat.vals, y = inv.vals)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='free_x', ncol=2) + xlab("Reference plot value") + ylab("Invaded plot value") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=0, lty=2, color=1)
p3.n
#p3 + geom_text(aes(label=plotname), hjust=1.1, size=3)
p3.n + geom_text(aes(label=ifelse((nat.vals>4*IQR(nat.vals)|inv.vals>4*IQR(inv.vals)),as.character(plotname)," "), hjust=1.1, size=3)) #label outliers

#Total Inorganic N
##slope = 0
p2.t<-ggplot(s.t.pretty,aes(s.t.pretty, x=nat.vals, y = inv.vals)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Invaded plot value") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=1, lty=2, color=1)
p2.t
#p2.t + geom_text(aes(label=plotname),hjust=1.1, size=3)
p2.t + geom_text(aes(label=ifelse((nat.vals>4*IQR(nat.vals)|inv.vals>4*IQR(inv.vals)),as.character(plotname)," "), hjust=1.1, size=3)) #label outliers
##slope = 1
s.t.pretty1<-transform(s.t.pretty,inv.vals=inv.vals-s.t.pretty$nat.vals)
p3.t<-ggplot(s.t.pretty1,aes(s.t.pretty1, x=nat.vals, y = inv.vals)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='free_x', ncol=2) + xlab("Reference plot value") + ylab("Invaded plot value") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=0, lty=2, color=1)
p3.t
#p3 + geom_text(aes(label=plotname), hjust=1.1, size=3)
p3.t + geom_text(aes(label=ifelse((nat.vals>4*IQR(nat.vals)|inv.vals>4*IQR(inv.vals)),as.character(plotname)," "), hjust=1.1, size=3)) #label outliers

#REMOVE OUTLIER 2012_K7_A

#Ammonium and Nitrate
##remove outlier
s.n.prettyOL<-s.n.pretty
s.n.prettyOL[s.n.prettyOL$plotname=='K7_A' & s.n.prettyOL$year=='2012',c('nat.vals','inv.vals')]<-NA #exclude 2012_K7_A
##slope = 0
p2.nOL<-ggplot(s.n.prettyOL,aes(s.n.prettyOL, x=nat.vals, y = inv.vals)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Invaded plot value") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=1, lty=2, color=1)
p2.nOL
#p2.n + geom_text(aes(label=plotname),hjust=1.1, size=3)
p2.nOL + geom_text(aes(label=ifelse((nat.vals>4*IQR(nat.vals, na.rm=T)|inv.vals>4*IQR(inv.vals, na.rm=T)),as.character(plotname)," "), hjust=1.1, size=3)) +
  scale_x_continuous(limits=c(0,7)) +
  scale_y_continuous(limits=c(0,7)) #label outliers
##slope = 1
s.n.pretty1OL<-transform(s.n.prettyOL,inv.vals=inv.vals-s.n.prettyOL$nat.vals)
p3.nOL<-ggplot(s.n.pretty1OL,aes(s.n.pretty1OL, x=nat.vals, y = inv.vals)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Invaded plot value") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=0, lty=2, color=1)
p3.nOL
#p3 + geom_text(aes(label=plotname), hjust=1.1, size=3)
p3.nOL + geom_text(aes(label=ifelse((nat.vals>4*IQR(nat.vals, na.rm=T)|inv.vals>4*IQR(inv.vals, na.rm=T)),as.character(plotname)," "), hjust=1.1, size=3)) +
  scale_x_continuous(limits=c(0,7)) #label outliers

#Total Inorganic N
##remove outlier
s.t.prettyOL<-s.t.pretty
s.t.prettyOL[s.t.prettyOL$plotname=='K7_A' & s.t.prettyOL$year=='2012',c('nat.vals','inv.vals')]<-NA #exclude 2012_K7_A
##slope = 0
p2.tOL<-ggplot(s.t.prettyOL,aes(s.t.prettyOL, x=nat.vals, y = inv.vals)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Invaded plot value") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=1, lty=2, color=1)
p2.tOL
#p2.t + geom_text(aes(label=plotname),hjust=1.1, size=3)
p2.tOL + geom_text(aes(label=ifelse((nat.vals>4*IQR(nat.vals, na.rm=T)|inv.vals>4*IQR(inv.vals, na.rm=T)),as.character(plotname)," "), hjust=1.1, size=3)) + 
  scale_x_continuous(limits=c(0,11)) +
  scale_y_continuous(limits=c(0,11)) #label outliers
##slope = 1
s.t.pretty1OL<-transform(s.t.prettyOL,inv.vals=inv.vals-s.t.prettyOL$nat.vals)
p3.tOL<-ggplot(s.t.pretty1OL,aes(s.t.pretty1OL, x=nat.vals, y = inv.vals)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Invaded plot value") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=0, lty=2, color=1)
p3.tOL
#p3 + geom_text(aes(label=plotname), hjust=1.1, size=3)
p3.tOL + geom_text(aes(label=ifelse((nat.vals>4*IQR(nat.vals, na.rm=T)|inv.vals>4*IQR(inv.vals, na.rm=T)),as.character(plotname)," "), hjust=1.1, size=3)) +
  scale_x_continuous(limits=c(0,11)) #label outliers


##########################
#linear model

#Fxn to calc pvals and coefs
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
s12<-sdf[sdf$year=='2012',]
s13<-sdf[sdf$year=='2013',]
colnames(s12)
stcol<-4 #for all
endcol<-6 #for all
##for 2012
result12<-AnotherFxn2(s12, stcol,endcol)
colna1<-c('pvalx','r2','int', 'coefx')
colna2<-paste(colna1,"slope1",sep="_")
colnames(result12)<-c(colna1,colna2) # does slope differ from 0,1
##for 2013
result13<-AnotherFxn2(s13, stcol, endcol)
colnames(result13)<-c(colna1,colna2) #does slope differ from 0,1

#Make a nice table
varnum<-endcol-stcol+1
year<-c(rep('2012',varnum),rep('2013',varnum))
varna<-c("Ammonium (ug/G)","Nitrate (ug/G)","Total Inorganic N (ug/G)")
xvar<-rep(varna,2)
tmp1<-rbind(result12,result13)
tmp2<-data.frame(xvar,year,tmp1)
tmp3<-orderBy(~xvar,tmp2)
tmp3[,c(3:6,8:10)]<-round(tmp3[,c(3:6,8:10)], digits=2)
tmp3[,7]<-round(tmp3[,7], digits=4)
tab<-tmp3
tab

#REMOVE OUTLIER 2012_K7_A

#Do it!
sdfOL<-sdf
sdfOL[sdfOL$plotname=='K7_A' & sdfOL$year==2012,c('nhi','noi','toti')]<-NA #exclude 2012_K7_A
sOL12<-sdfOL[sdfOL$year=='2012',]
sOL13<-sdfOL[sdfOL$year=='2013',]
##for 2012
resultOL12<-AnotherFxn2(sOL12, stcol,endcol)
colnames(resultOL12)<-c(colna1,colna2) # does slope differ from 0,1
##for 2013
resultOL13<-AnotherFxn2(s13, stcol, endcol)
colnames(resultOL13)<-c(colna1,colna2) #does slope differ from 0,1

#Make a nice table
tmp1.OL<-rbind(resultOL12,resultOL13)
tmp2.OL<-data.frame(xvar,year,tmp1.OL)
tmp3.OL<-orderBy(~xvar,tmp2.OL)
tmp3.OL[,c(3:6,8:10)]<-round(tmp3.OL[,c(3:6,8:10)], digits=2)
tmp3.OL[,7]<-round(tmp3.OL[,7], digits=4)
tab.OL<-tmp3.OL
tab.OL




