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
sv1<-sv[,c(15,1,7,8,9,10,11,12,17)]
#for nhi, noi, toti
sv.n<-melt(sv1, measure.var=c('nhi','noi','toti'))
sv.n$year<-as.factor(sv.n$year)
head(sv.n) 
#for ammonif, nitrif, minze
sv.m<-melt(sv1, measure.var=c('ammonifd','nitrifd','minzd'))
sv.m$year<-as.factor(sv.m$year)
head(sv.m)

#Fxn to rename variables for ggplot
PrettyVaribNames<-function(df, prettynames, vars){
  prettydf <- df
  for (i in 1:length(vars)){
    levels(prettydf$variable)[levels(prettydf$variable)==vars[i]] <- prettynames[i]
  }
  return (prettydf)
}

#Provide pretty varibale names using fxn
prettynames<-c("Ammonium (ug/G)","Nitrate (ug/G)","Total Inorganic N (ug/G)","Ammonification (ug/G*d)","Nitrification (ug/G*d)","Mineralization (ug/G*d)","Soil Moisture (%)") #for all
vars<-c('nhi','noi','toti','ammonifd','nitrifd','minzd','soilmoi') #for all
sv.n.pretty<-PrettyVaribNames(sv.n, prettynames, vars) #for plot with nhi and noi
sv.m.pretty<-PrettyVaribNames(sv.m, prettynames, vars) #for plot with toti

#Ammonium, Nitrate, Total inorganic N
p1.n<-ggplot(sv.n.pretty,aes(sv.n.pretty, x=value, y = mv)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Microstegium biomass (g)") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=0, lty=2, color=1)
p1.n
#ggsave(file = "p1_n.png",scale=1,width = 6, height = 6)
#p1.n + geom_text(aes(label=plotname),hjust=1.1, size=3)
p1.n + geom_text(aes(label=ifelse((value>4*IQR(value)|mv>4*IQR(mv)),as.character(plotname)," "), hjust=1.1, size=3)) #label outliers
#ggsave(file = "p1_n_labels.png",scale=1,width = 6, height = 6)

#Total inorganic N
p1.m<-ggplot(sv.m.pretty,aes(sv.m.pretty, x=value, y = mv)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Microstegium biomass (g)") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=0, lty=2, color=1)
p1.m
#ggsave(file = "p1_m.png",scale=1,width = 6, height = 6)
#p1 + geom_text(aes(label=plotname),hjust=1.1, size=3)
p1.m + geom_text(aes(label=ifelse((value>4*IQR(value)|mv>4*IQR(mv)),as.character(plotname)," "), hjust=1.1, size=3)) #label outliers
#ggsave(file = "p1_m_labels.png",scale=1,width = 6, height = 6)

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
endcol<-8
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
varna<-c("Ammonium (ug/G)","Nitrate (ug/G)","Total Inorganic N (ug/G)",
         "Ammonification (ug/G*d)","Nitrification (ug/G*d)","Mineralization (ug/G*d)")
xvar<-rep(varna,2)
tmp1<-rbind(result12,result13)
tmp2<-data.frame(xvar,year,tmp1)
tmp3<-orderBy(~xvar,tmp2)
colnames(tmp3)
tmp3[,3:6]<-round(tmp3[,3:6], digits=2)
tab<-tmp3
tab
write.table(tab, file='q1tab.txt', sep="\t", row.names=F)


# plot just the significant correlations
## indicate whether signif as a column in the dataset

## do this for s.n.pretty
head(sv.n.pretty)
sv.n.pretty$signif0<-rep(NA,dim(sv.n.pretty)[1])
### fill in signif0 for s.n.pretty
filter1<-sv.n.pretty$year == 2013 & s.n.pretty$variable == "Ammonium (ug/G)"
filter2<-sv.n.pretty$year == 2013 & s.n.pretty$variable == "Total Inorganic N (ug/G)"
lgth<-length(sv.n.pretty[filter1,'signif0'])
sv.n.pretty[filter1,'signif0']<-rep(1,lgth)
sv.n.pretty[filter2,'signif0']<-rep(1,lgth)
View(sv.n.pretty)

## do this for s.m.pretty
head(sv.m.pretty)
sv.m.pretty$signif0<-rep(NA,dim(sv.m.pretty)[1])
### fill in signif0 for s.m.pretty
#none are signif

# plot
p1.n1<-ggplot(sv.n.pretty,aes(sv.n.pretty, x=value, y = mv)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Microstegium biomass (g)") +  geom_abline(intercept=0, slope=0, lty=2, color=1)
p1.n1 + geom_smooth(data=subset(sv.n.pretty, signif0 == 1), method=lm,se=T)
ggsave(file = "p1_n_signif.png",scale=1,width = 6, height = 6)

p1.m<-ggplot(sv.m.pretty,aes(sv.m.pretty, x=value, y = mv)) + geom_point(aes(color=year)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Microstegium biomass (g)") + geom_abline(intercept=0, slope=0, lty=2, color=1)
p1.m #none are signif
ggsave(file = "p1_m_signif.png",scale=1,width = 6, height = 6)

