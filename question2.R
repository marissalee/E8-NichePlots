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

#plot data(pd)
pd<-read.table("e8DataPackage_080314_clean/e8_plotLoc_clean.txt",header=TRUE,sep="\t")
pd_dic<-read.table("e8DataPackage_080314_clean/e8_plotLoc_dictionary_clean.txt",header=TRUE,sep="\t")


#########################################################
#Make dataframe to plot: invader biomass vs native subplot resource availability

#pull out the top depth
sdt<-filter(sd,depth == "T") 

#add understory col
sdt$understory<-rep(pd$understory,2)
View(sdt)

#pick out the columns of interest
colnames(sdt)
sdf<-sdt[,c('plotname','year','inv','nhi','noi','toti','ammonifd','nitrifd','minzd','understory')]

#Make dataframes to plot 1) nhi, noi and 2) toti and 3) ammonif nitrif minzd
colnames(sdf)

##for nhi and noi and toti
sdf.n<-melt(sdf, measure.var=c('nhi','noi','toti'))
sdf.n$year<-as.factor(sdf.n$year)
sdf.nN<-filter(sdf.n,inv == "N") 
sdf.nI<-filter(sdf.n,inv == "I") 
s.n<-data.frame(sdf.nN[,c('plotname','year','variable')], nat.vals=sdf.nN[,'value'], inv.vals=sdf.nI[,'value'],understory=sdf.nN[,'understory'])
head(s.n)
##for ammonif, nitrifd, minzd
sdf.m<-melt(sdf, measure.var=c('ammonifd','nitrifd','minzd'))
colnames(sdf.m)
sdf.m$year<-as.factor(sdf.m$year)
sdf.mN<-filter(sdf.m,inv == "N") 
sdf.mI<-filter(sdf.m,inv == "I") 
s.m<-data.frame(sdf.mN[,c('plotname','year','variable')], nat.vals=sdf.mN[,'value'], inv.vals=sdf.mI[,'value'], understory = sdf.mN[,'understory'])
head(s.m)

#Fxn to rename variables for ggplot
PrettyVaribNames<-function(df, prettynames, vars){
  prettydf <- df
  for (i in 1:length(vars)){
    levels(prettydf$variable)[levels(prettydf$variable)==vars[i]] <- prettynames[i]
  }
  return (prettydf)
}

#Provide pretty varibale names using fxn
prettynames<-c("Ammonium (ug/G)","Nitrate (ug/G)","Total Inorganic N (ug/G)","Ammonification","Nitrification (ug/G*d)","Mineralization (ug/G*d)","Soil Moisture (%)") #for all
vars<-c('nhi','noi','toti','ammonifd','nitrifd','minzd','soilmoi') #for all
s.n.pretty<-PrettyVaribNames(s.n, prettynames, vars) #for plot with nhi and noi and toti
s.m.pretty<-PrettyVaribNames(s.m, prettynames, vars) #for plot with ammonif, nitrif, minz

#Ammonium, Nitrate, Total inorganic N
##slope = 0
p2.n<-ggplot(s.n.pretty,aes(s.n.pretty, x=nat.vals, y = inv.vals)) + geom_point(aes(color=understory)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Invaded plot value") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=1, lty=2, color=1)
p2.n
#ggsave(file = "p2_n.png",scale=1,width = 6, height = 6)
#p2.n + geom_text(aes(label=plotname),hjust=1.1, size=3)
#p2.n + geom_text(aes(label=ifelse((nat.vals>4*IQR(nat.vals, na.rm=T)|inv.vals>4*IQR(inv.vals, na.rm=T)),as.character(plotname)," "), hjust=1.1, size=3)) #label outliers
#ggsave(file = "p2_n_labels.png",scale=1,width = 6, height = 6)
##slope = 1
s.n.pretty1<-transform(s.n.pretty,inv.vals=inv.vals-s.n.pretty$nat.vals)
p3.n<-ggplot(s.n.pretty1,aes(s.n.pretty1, x=nat.vals, y = inv.vals)) + geom_point(aes(color=understory)) + facet_wrap(~variable+year, scales='free_x', ncol=2) + xlab("Reference plot value") + ylab("Invaded - Reference plot value") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=0, lty=2, color=1)
p3.n
#ggsave(file = "p3_n.png",scale=1,width = 6, height = 6)
#p3 + geom_text(aes(label=plotname), hjust=1.1, size=3)
#p3.n + geom_text(aes(label=ifelse((nat.vals>4*IQR(nat.vals, na.rm=T)|inv.vals>4*IQR(inv.vals, na.rm=T)),as.character(plotname)," "), hjust=1.1, size=3)) #label outliers
#ggsave(file = "p3_n_labels.png",scale=1,width = 6, height = 6)


#Ammonif, Nitrif, Minzd
##slope = 0
p2.m<-ggplot(s.m.pretty,aes(s.m.pretty, x=nat.vals, y = inv.vals)) + geom_point(aes(color=understory)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Invaded plot value") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=1, lty=2, color=1)
p2.m
#ggsave(file = "p2_m.png",scale=1,width = 6, height = 6)
#p2.m + geom_text(aes(label=plotname),hjust=1.1, size=3)
#p2.m + geom_text(aes(label=ifelse((nat.vals>4*IQR(nat.vals, na.rm=T)|inv.vals>4*IQR(inv.vals, na.rm=T)),as.character(plotname)," "), hjust=1.1, size=3)) #label outliers
#ggsave(file = "p2_m_labels.png",scale=1,width = 6, height = 6)
##slope = 1
s.m.pretty1<-transform(s.m.pretty,inv.vals=inv.vals-s.m.pretty$nat.vals)
p3.m<-ggplot(s.m.pretty1,aes(s.m.pretty1, x=nat.vals, y = inv.vals)) + geom_point(aes(color=understory)) + facet_wrap(~variable+year, scales='free_x', ncol=2) + xlab("Reference plot value") + ylab("Invaded - Reference plot value") + geom_smooth(method='lm',se=T) + geom_abline(intercept=0, slope=0, lty=2, color=1)
p3.m
#ggsave(file = "p3_m.png",scale=1,width = 6, height = 6)
#p3 + geom_text(aes(label=plotname), hjust=1.1, size=3)
#p3.m + geom_text(aes(label=ifelse((nat.vals>4*IQR(nat.vals, na.rm=T)|inv.vals>4*IQR(inv.vals, na.rm=T)),as.character(plotname)," "), hjust=1.1, size=3)) #label outliers
#ggsave(file = "p3_m_labels.png",scale=1,width = 6, height = 6)


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
endcol<-9 #for all
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
varna<-c("Ammonium (ug/G)","Nitrate (ug/G)","Total Inorganic N (ug/G)",
         "Ammonification (ug/G*d)","Nitrification (ug/G*d)","Mineralization (ug/G*d)")
xvar<-rep(varna,2)
tmp1<-rbind(result12,result13)
tmp2<-data.frame(xvar,year,tmp1)
tmp3<-orderBy(~xvar,tmp2)
tmp3
tmp3[,c(3:6,8:10)]<-round(tmp3[,c(3:6,8:10)], digits=2)
tmp3[,7]<-round(tmp3[,7], digits=4)
tab<-tmp3
write.table(tab, file='q2n3tab.txt', sep="\t", row.names=F)


# plot just the significant correlations
## indicate whether signif as a column in the dataset

## do this for s.n.pretty
head(s.n.pretty)
s.n.pretty$signif0<-rep(NA,dim(s.n.pretty)[1])
s.n.pretty1$signif1<-rep(NA,dim(s.n.pretty1)[1])
### fill in signif0 for s.n.pretty
filter1<-s.n.pretty$year == 2013 & s.n.pretty$variable == "Ammonium (ug/G)"
filter2<-s.n.pretty$year == 2013 & s.n.pretty$variable == "Nitrate (ug/G)"
filter3<-s.n.pretty$year == 2013 & s.n.pretty$variable == "Total Inorganic N (ug/G)"
lgth<-length(s.n.pretty[filter1,'signif0'])
s.n.pretty[filter1,'signif0']<-rep(1,lgth)
s.n.pretty[filter2,'signif0']<-rep(1,lgth)
s.n.pretty[filter3,'signif0']<-rep(1,lgth)
#View(s.n.pretty)
### fill in signif1 for s.n.pretty
filter1<-s.n.pretty1$year == 2012 & s.n.pretty1$variable == "Ammonium (ug/G)"
filter2<-s.n.pretty1$year == 2013 & s.n.pretty1$variable == "Ammonium (ug/G)"
filter3<-s.n.pretty1$year == 2012 & s.n.pretty1$variable == "Nitrate (ug/G)"
lgth<-length(s.n.pretty1[filter1,'signif1'])
s.n.pretty1[filter1,'signif1']<-rep(1,lgth)
s.n.pretty1[filter2,'signif1']<-rep(1,lgth)
s.n.pretty1[filter3,'signif1']<-rep(1,lgth)
#View(s.n.pretty1)

## do this for s.m.pretty
head(s.m.pretty)
s.m.pretty$signif0<-rep(NA,dim(s.m.pretty)[1])
s.m.pretty1$signif1<-rep(NA,dim(s.m.pretty)[1])
### fill in signif0 for s.m.pretty
filter1<-s.m.pretty$year == 2012 & s.m.pretty$variable == "Nitrification (ug/G*d)"
filter2<-s.m.pretty$year == 2012 & s.m.pretty$variable == "Mineralization (ug/G*d)"
filter3<-s.m.pretty$year == 2013 & s.m.pretty$variable == "Mineralization (ug/G*d)"
lgth<-length(s.m.pretty[filter1,'signif0'])
s.m.pretty[filter1,'signif0']<-rep(1,lgth)
s.m.pretty[filter2,'signif0']<-rep(1,lgth)
s.m.pretty[filter3,'signif0']<-rep(1,lgth)
#View(s.m.pretty)
### fill in signif1 for s.m.pretty
#all signif
lgth<-length(s.m.pretty1[,'signif1'])
s.m.pretty1[,'signif1']<-rep(1,lgth)
#View(s.m.pretty1)

# plot
p2.n1<-ggplot(s.n.pretty,aes(s.n.pretty, x=nat.vals, y = inv.vals)) + geom_point(aes(color=understory)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Invaded plot value") + geom_abline(intercept=0, slope=1, lty=2, color=1)
p2.n1 + geom_smooth(data=subset(s.n.pretty, signif0 == 1), method=lm,se=T)
ggsave(file = "p2_n_signif.png",scale=1,width = 6, height = 6)

p3.n1<-ggplot(s.n.pretty1,aes(s.n.pretty1, x=nat.vals, y = inv.vals)) + geom_point(aes(color=understory)) + facet_wrap(~variable+year, scales='free_x', ncol=2) + xlab("Reference plot value") + ylab("Invaded - Reference plot value")  + geom_abline(intercept=0, slope=0, lty=2, color=1)
p3.n1 + geom_smooth(data=subset(s.n.pretty1, signif1 == 1), method=lm,se=T)
ggsave(file = "p3_n_signif.png",scale=1,width = 6, height = 6)

p2.m1<-ggplot(s.m.pretty,aes(s.m.pretty, x=nat.vals, y = inv.vals)) + geom_point(aes(color=understory)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Reference plot value") + ylab("Invaded plot value") + geom_abline(intercept=0, slope=1, lty=2, color=1)
p2.m1 + geom_smooth(data=subset(s.m.pretty, signif0 == 1), method=lm,se=T)
ggsave(file = "p2_m_signif.png",scale=1,width = 6, height = 6)

p3.m1<-ggplot(s.m.pretty1,aes(s.m.pretty1, x=nat.vals, y = inv.vals)) + geom_point(aes(color=understory)) + facet_wrap(~variable+year, scales='free_x', ncol=2) + xlab("Reference plot value") + ylab("Invaded - Reference plot value")  + geom_abline(intercept=0, slope=0, lty=2, color=1)
p3.m1 + geom_smooth(method=lm,se=T) # all signif
ggsave(file = "p3_m_signif.png",scale=1,width = 6, height = 6)





