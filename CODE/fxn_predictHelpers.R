#fxn_predictHelpers

#Set up factor levels by break a continuous variable (catVar) into low and high, split by the median value
SetupFactors<-function(df, catVar, labelNames){
  
  #add a new column to the dataframe
  df$factorLevel<-NA
  
  #identify labels
  lowLabel<-labelNames[1]
  highLabel<-labelNames[2]
  
  #split by median
  LowVals <- df[,catVar] < median(df[,catVar])
  df[LowVals,'factorLevel']<-lowLabel
  HighVals <- df[,catVar] >= median(df[,catVar])
  df[HighVals,'factorLevel']<-highLabel
  
  #set up order
  df$factorLevel<-factor(df$factorLevel, levels=c(lowLabel, highLabel))
  
  return(df)
}




#InteractionPlot_DF: creates dataframes to predict for specific groups when there is an interaction in the model
InteractionPlot_DF<-function(n, xVar, factorVar, factorBin, constantVar, df, mod, re.pred){
  
  #1) Set up parameters to loop through for each new dataset
  select.year.vec<-c(2012, 2012, 2013, 2013)
  select.factorBin.vec<-c(1,2,1,2)
  constantVar.mean<-mean(df.mod[,constantVar])
  
  #2) Create new dataframes that represent the different scenarios to plot
  newDf.list<-list()
  i<-0
  for(i in 1:length(select.year.vec)){
    
    #identify the current bin for factorVar and subset the data
    current.Bin<-levels(df[,factorBin])[select.factorBin.vec[i]]
    factorLevel<-rep(current.Bin, n)
    current.df<-df.mod[df.mod[,factorBin] == current.Bin,]
    
    #mean of the constant var (from full dataset)
    V_constantVar<-rep(constantVar.mean, n)
    
    #mean of the current bin for factorVar within the current bin
    V_factorVar<-rep(mean(current.df[,factorVar]), n)
    
    #range of Mv biomass within the current bin
    xVarRange<-range(current.df[,xVar])
    V_xVar<-seq(from=xVarRange[1], to=xVarRange[2], length.out=n)
    
    #current year
    V_year<-rep(select.year.vec[i], n)
    
    #make df
    df.new<-data.frame(V_year, V_xVar, V_factorVar, factorLevel, V_constantVar)
    colnames(df.new)<-c('year', xVar, factorVar, 'factorLevel', constantVar)
    
    newDf.list[[i]]<-df.new
    
  }
  names(newDf.list)<-paste('group',select.factorBin.vec,select.year.vec, sep="_")
  
  #3) Predict y values for each scenario
  if(re.pred==FALSE){
    predData.list.fit<-llply(newDf.list, predict, object=mod, re.form=NA) #unconditional
  }else{
    predData.list.fit<-llply(newDf.list, predict, object=mod) #conditional on random effect levels
  }
  predData.list<-llply(newDf.list, predictInterval, merMod=mod, n.sims = 999, include.resid.var=TRUE)

  #4) Add ypred columns back to the original data frames
  df.tmp.list<-list()
  i<-0
  for(i in 1:length(newDf.list)){
    df.tmp<-data.frame(newDf.list[[i]], ypred=predData.list.fit[[i]], CI=predData.list[[i]])
    df.tmp.list[[i]]<-df.tmp
  }
  names(df.tmp.list)<-names(newDf.list)
  df.pred<-ldply(df.tmp.list)
  
  return(df.pred)
  
}

#XYPlot_DF.R: creates dataframes to predict for specific groups, no interaction
XYPlot_DF<-function(n, xVar, constantVar, df, mod, re.pred){
  
  #1) Set up parameters to loop through for each new dataset
  select.year.vec<-c(2012, 2013)
  
  #2) Create new dataframes that represent the different scenarios to plot
  newDf.list<-list()
  i<-0
  for(i in 1:length(select.year.vec)){
    
    #mean of the constant var (from full dataset)
    V_constantVars<-matrix(nr=n, nc=length(constantVar))
    k<-0
    for(k in 1:length(constantVar)){
      constantVar.mean<-mean(df.mod[,constantVar[k]])
      V_constantVars[,k]<-rep(x=constantVar.mean, times=n)
    }
    colnames(V_constantVars)<-paste('consVar',seq(1,length(constantVar)), sep="_")
    
    #range of xVar
    xVarRange<-range(df[,xVar])
    V_xVar<-seq(from=xVarRange[1], to=xVarRange[2], length.out=n)
    
    #current year
    V_year<-rep(select.year.vec[i], n)
    
    #make df
    df.new<-data.frame(V_year, V_xVar, V_constantVars)
    colnames(df.new)<-c('year', xVar, constantVar)
    newDf.list[[i]]<-df.new
    
  }
  names(newDf.list)<-paste('group', select.year.vec, sep="_")
  
  #3) Predict y values for each scenario
  if(re.pred==FALSE){
    predData.list.fit<-llply(newDf.list, predict, object=mod, re.form=NA) #unconditional
  }else{
    predData.list.fit<-llply(newDf.list, predict, object=mod) #conditional on random effect levels
  }
  predData.list<-llply(newDf.list, predictInterval, merMod=mod, n.sims = 999, include.resid.var=TRUE)
  
  #4) Add ypred columns back to the original data frames
  df.tmp.list<-list()
  i<-0
  for(i in 1:length(newDf.list)){
    df.tmp<-data.frame(newDf.list[[i]], ypred=predData.list.fit[[i]], CI=predData.list[[i]])
    df.tmp.list[[i]]<-df.tmp
  }
  names(df.tmp.list)<-names(newDf.list)
  df.pred<-ldply(df.tmp.list)
  
  return(df.pred)
  
}




