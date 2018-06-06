#script_q2_prep.R


#subset data
allVars.levels<- allVars_levels()
data.vars<-data.choice[data.choice$variable %in% allVars.levels$allVars, c('plotid','plothalfid1','inv','year','variable','value')]
data.vars$variable<-factor(data.vars$variable, levels=allVars.levels$allVars)


##############
#A. Calculate impact magnitude for each target variable (nitrate, nitrification, ammonification)
##############
#reshape dataframe
data.vars.wideIN <- dcast(data.vars, plotid + year + variable ~ inv, value.var="value")
#calculate diff
data.vars.wideIN$Diff<-data.vars.wideIN$I - data.vars.wideIN$N
#subset select diff variables and reshape
data.diffVars<-data.vars.wideIN[data.vars.wideIN$variable %in% diffVars, c('plotid','year','variable','Diff')]
data.diffVars.wide<-dcast(data.diffVars, plotid + year ~ variable, value.var='Diff')
colnames(data.diffVars.wide)[-c(1:2)]<-paste('Diff', colnames(data.diffVars.wide)[-c(1:2)], sep="_")

##############
#B. Test the role of reference plot conditions and Microstegium biomass on impacts individually using mixed effects models with year as a random effect
##############

#i) Identify reference plot condition variables
data.refVars<-data.vars
data.refVars<-subset(data.refVars, inv == 'N')
data.refVars.wide<-dcast(data.refVars, plotid + year ~ variable, value.var='value')

#ii) Identify microstegium biomass
data.mvVar<-data.choice[data.choice$variable %in% 'mv_g.m2', c('plotid','plothalfid1','inv','year','variable','value')]
data.mvVar<-subset(data.mvVar, inv == 'I')
data.mvVar.wide<-dcast(data.mvVar, plotid + year ~ variable, value.var='value')
pHist.mv<-ggplot(data.mvVar.wide, aes(x=log(mv_g.m2)))+geom_histogram() #log-transform mv biomass to improve normality
#pHist.mv
data.mvVar.wide$mv_g.m2_logt<-log(data.mvVar.wide$mv_g.m2)

#iii) Merge the reference plot condition columns and the microstegium biomass columns
data.q2.indp<-merge(data.refVars.wide, data.mvVar.wide)

#iv) Merge the impact variables and the independant variables
data.q2<-merge(data.diffVars.wide, data.q2.indp)
data.q2$plotYear<-paste(data.q2$plotid,data.q2$year, sep="_")


#v) If PercBA_AM should be included separately from reference conditions...Identify PercBA_AM and merge it into dataset
#externalPercBA_AM
externalPercBA_AM
if(externalPercBA_AM == 'y'){
  
  #extract data and reformat
  data.AM<-data.choice[data.choice$variable %in% 'PercBA_AM', c('plotid','plothalfid1','inv','year','variable','value')]
  data.AM<-subset(data.AM, inv == 'N')
  data.AM.wide<-dcast(data.AM, plotid + year ~ variable, value.var='value')
  
  #merge
  data.q2<-merge(data.q2, data.AM.wide)
  
  #make a bin for %AM
  data.q2$MycBin<-NA
  data.q2[data.q2$PercBA_AM >=0 & data.q2$PercBA_AM <50,'MycBin']<-'AM'
  data.q2[data.q2$PercBA_AM >=50 & data.q2$PercBA_AM <=100,'MycBin']<-'ECM'
}











