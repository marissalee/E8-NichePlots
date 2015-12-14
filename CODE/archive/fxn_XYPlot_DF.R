#XYPlot_DF.R


#
XYPlot_DF<-function(n, xVar, constantVar, df, mod){
  
  #1) Set up parameters to loop through for each new dataset
  select.year.vec<-c(2012, 2013)
  constantVar.mean<-mean(df.mod[,constantVar])
  
  #2) Create new dataframes that represent the different scenarios to plot
  newDf.list<-list()
  i<-0
  for(i in 1:length(select.year.vec)){
    
    #mean of the constant var (from full dataset)
    V_constantVar<-rep(constantVar.mean, n)
    
    #range of xVar
    xVarRange<-range(df[,xVar])
    V_xVar<-seq(from=xVarRange[1], to=xVarRange[2], length.out=n)
    
    #current year
    V_year<-rep(select.year.vec[i], n)
    
    #make df
    df.new<-data.frame(V_year, V_xVar, V_constantVar)
    colnames(df.new)<-c('year', xVar, constantVar)
    df.new
    newDf.list[[i]]<-df.new
    
  }
  names(newDf.list)<-paste('group', select.year.vec, sep="_")
  
  #3) Predict y values for each scenario
  predData.list.fit<-llply(newDf.list, predict, object=mod)
  predData.list<-llply(newDf.list, predictInterval, merMod=mod, n.sims = 999)
  
  #4) Add ypred columns back to the original data frames
  df.tmp.list<-list()
  i<-0
  for(i in 1:length(newDf.list)){
    df.tmp<-data.frame(newDf.list[[i]], ypred=predData.list.fit[[i]], CI=predData.list[[i]])
    df.tmp.list[[i]]<-df.tmp
  }
  names(df.tmp.list)<-names(newDf.list)
  df.pred<-ldply(df.tmp.list)
  
  #remove the year intercept for another set of ypred columns
  mod.coefs<-coef(mod)$year
  int2012<-mod.coefs[1,1]
  int2013<-mod.coefs[2,1]
  int.Diff<-int2012 - int2013
  df.pred$ypred.minusYr<-df.pred$ypred
  df.pred[df.pred$year==2012,'ypred.minusYr']<-df.pred[df.pred$year==2012,'ypred'] - int.Diff
  df.pred$CI.lwr.minusYr<-df.pred$CI.lwr
  df.pred[df.pred$year==2012,'CI.lwr.minusYr']<-df.pred[df.pred$year==2012,'CI.lwr'] - int.Diff
  df.pred$CI.upr.minusYr<-df.pred$CI.upr
  df.pred[df.pred$year==2012,'CI.upr.minusYr']<-df.pred[df.pred$year==2012,'CI.upr'] - int.Diff
  
  return(df.pred)
  
}

