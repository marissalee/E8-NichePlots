#question2_xbiomass.R
#files needed: tab-delim files in 'e8DataPackage_080314_clean' folder
#question2_xbiomass: How does the invasion impact (on N pools and fluxes) correlate with M.v. biomass?

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
#Make dataframe to plot: invader biomass vs soil inv - ref

#add understory col to vd
dim(vd)
vd$understory<-rep(pd$understory,3)
#View(vd)

#pull out just the invaded rows of data
vdt1<-filter(vd,inv == "I")
#make a df with just 2012 and 2013 data
vdt2<-filter(vdt1,year != "2011")

#pull out just the native and invaded rows of data
sdtN1<-filter(sd,inv == "N") 
sdtI1<-filter(sd,inv == "I") 
#make a df for the top depth
sdtN2<-filter(sdtN1,depth == "T") 
sdtI2<-filter(sdtI1,depth == "T") 
dim(sdtN2); dim(sdtI2)
colnames(sdtN2)

#calculate rr of soil N measures (inv - ref)
colnames(sdtN2)
Nmeas<-sdtN2[,c(8:13)]
Imeas<-sdtI2[,c(8:13)]

store<-numeric(0)
i<-0
for (i in 1:dim(Nmeas)[2]){
  diff<- Imeas[,i] - Nmeas[,i]
  store<-cbind(store,diff)
}
colnames(store)<-paste(colnames(Nmeas),'diff',sep="_")
diffs<-store

#make a dataframe with all soil diff measures and mv biomass
#see if I can merge the veg and soil dfs by plotid
colnames(vdt2)
df<-data.frame(vdt2[,c('plotname','year','mv')],diffs,understory=vdt2[,'understory'])

#Make dataframes to plot diff (y) 1) nhi, noi and 2) toti and 3) ammonif nitrif minzd vs mv
colnames(df)
##for nhi and noi and toti
df.n<-melt(df, measure.var=c('nhi_diff','noi_diff','toti_diff'))
df.n$year<-as.factor(df.n$year)
mv.vals<-rep(df$mv,3)
colnames(df.n)
s.n<-data.frame(df.n[,c('plotname','year','variable')], mv.vals=mv.vals, diff.vals=df.n[,'value'], understory=df.n[,'understory'])
head(s.n)
##for ammonif, nitrifd, minzd
df.m<-melt(df, measure.var=c('ammonifd_diff','nitrifd_diff','minzd_diff'))
df.m$year<-as.factor(df.m$year)
s.m<-data.frame(df.m[,c('plotname','year','variable')], mv.vals=mv.vals, diff.vals=df.m[,'value'], understory=df.n[,'understory'])
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
vars<-c('nhi_diff','noi_diff','toti_diff','ammonifd_diff','nitrifd_diff','minzd_diff') #for all
s.n.pretty<-PrettyVaribNames(s.n, prettynames, vars) #for plot with nhi and noi and toti
s.m.pretty<-PrettyVaribNames(s.m, prettynames, vars) #for plot with ammonif, nitrif, minz

#N pools
p2biom.n<-ggplot(s.n.pretty,aes(s.n.pretty, x=mv.vals, y = diff.vals)) + geom_point(aes(color=understory)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Microstegium biomass (g)") + ylab("Invaded - Ref.") + geom_smooth(method='lm',se=T)
p2biom.n

#N fluxes
p2biom.m<-ggplot(s.m.pretty,aes(s.m.pretty, x=mv.vals, y = diff.vals)) + geom_point(aes(color=understory)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Microstegium biomass (g)") + ylab("Invaded - Ref.") + geom_smooth(method='lm',se=T)
p2biom.m

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
AnotherFxn3<-function(data, stcol, endcol){
  yvars<-colnames(data)[stcol:endcol]
  store<-numeric(0)
  i<-0
  for (i in 1:length(yvars)){
    y<-data[,i+stcol-1]
    x<-data[,'mv']
    df<-data.frame(x,y)
    
    fit<-lm(y ~ x, df) #slope = 0
    pvals<-GetPvals(fit)
    coefs<-GetCoefs(fit)

    row<-c(pvals,coefs)
    store<-rbind(store,row)
  }
  return(store)
}

#Do it!

dff<-data.frame(sdtN2[,c('year','plotname')],diffs,mv=vdt2[,'mv'])
colnames(dff)
dff12<-dff[dff$year=='2012',]
dff13<-dff[dff$year=='2013',]
colnames(dff12)
stcol<-3 #for all
endcol<-8 #for all
##for 2012
result12<-AnotherFxn3(dff12, stcol,endcol)
result12
colna1<-c('pvalx','r2','int', 'coefx')
colnames(result12)<-colna1 # does slope differ from 0,1
result12

##for 2013
result13<-AnotherFxn3(dff13, stcol, endcol)
colnames(result13)<-colna1 #does slope differ from 0,1
#Make a nice table
varnum<-endcol-stcol+1
year<-c(rep('2012',varnum),rep('2013',varnum))
varna<-c("Diff Ammonium (ug/G)","Diff Nitrate (ug/G)","Diff Total Inorganic N (ug/G)",
         "Diff Ammonification (ug/G*d)","Diff Nitrification (ug/G*d)","Diff Mineralization (ug/G*d)")
yvar<-rep(varna,2)
tmp1<-rbind(result12,result13)
tmp2<-data.frame(yvar,year,tmp1)
tmp3<-orderBy(~xvar,tmp2)
tmp3
tmp3[,c(3:6)]<-round(tmp3[,c(3:6)], digits=2)
tab<-tmp3
tab
write.table(tab, file='q3_xbiomtab.txt', sep="\t", row.names=F)


## do this for s.n.pretty
head(s.n.pretty)
unique(s.n.pretty$variable)
s.n.pretty$signif0<-rep(NA,dim(s.n.pretty)[1])
### fill in signif0 for s.n.pretty
filter1<-s.n.pretty$year == 2013 & s.n.pretty$variable == "Ammonium (ug/G)"
filter2<-s.n.pretty$year == 2012 & s.n.pretty$variable == "Nitrate (ug/G)"
filter3<-s.n.pretty$year == 2013 & s.n.pretty$variable == "Nitrate (ug/G)"
filter4<-s.n.pretty$year == 2013 & s.n.pretty$variable == "Total Inorganic N (ug/G)"
lgth<-length(s.n.pretty[filter1,'signif0'])
lgth
s.n.pretty[filter1,'signif0']<-rep(1,lgth)
s.n.pretty[filter2,'signif0']<-rep(1,lgth)
s.n.pretty[filter3,'signif0']<-rep(1,lgth)
s.n.pretty[filter4,'signif0']<-rep(1,lgth)
#View(s.n.pretty)

## do this for s.m.pretty
head(s.m.pretty)
s.m.pretty$signif0<-rep(NA,dim(s.m.pretty)[1])
### fill in signif0 for s.m.pretty
# no diffs
#View(s.m.pretty)


# plot
#N pools
p2biom.n1<-ggplot(s.n.pretty,aes(s.n.pretty, x=mv.vals, y = diff.vals)) + geom_point(aes(color=understory)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Microstegium biomass (g)") + ylab("Invaded - Ref.") + geom_abline(intercept=0, slope=0, lty=2, color=1)
p2biom.n1 +  geom_smooth(data=subset(s.n.pretty, signif0 == 1), method=lm,se=T)
ggsave(file = "p3_xbiom_n_signif.png",scale=1,width = 6, height = 6)


#N fluxes
p2biom.m1<-ggplot(s.m.pretty,aes(s.m.pretty, x=mv.vals, y = diff.vals)) + geom_point(aes(color=understory)) + facet_wrap(~variable+year, scales='fixed', ncol=2) + xlab("Microstegium biomass (g)") + ylab("Invaded - Ref.") + geom_abline(intercept=0, slope=0, lty=2, color=1)
p2biom.m1
ggsave(file = "p3_xbiom_m_signif.png",scale=1,width = 6, height = 6)







