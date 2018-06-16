
XYPlot_DF <- function(n, xVar, constantVar, df, mod){
  
  # create new dataframes
  select.year.vec <- c(2012, 2013)
  newDf.list<-list()
  for(i in 1:length(select.year.vec)){
    
    #mean of the constant var (from full dataset)
    V_constantVars<-matrix(nr=n, nc=length(constantVar))
    for(k in 1:length(constantVar)){
      df[,constantVar]
      constantVar.mean<-mean(df[,constantVar[k]])
      V_constantVars[,k]<-rep(x=constantVar.mean, times=n)
    }
    colnames(V_constantVars)<-paste('consVar',seq(1,length(constantVar)), sep="_")
    
    #range of xVar
    xVarRange<-range(df[,xVar])
    V_xVar<-seq(from=xVarRange[1], to=xVarRange[2], length.out=n)
    
    #current year
    V_year<-rep(select.year.vec[i], n)
    
    #make df
    df.new <- data.frame(V_year, V_xVar, V_constantVars)
    colnames(df.new) <- c('year', xVar, constantVar)
    newDf.list[[i]] <- df.new
  }
  names(newDf.list) <- paste('group', select.year.vec, sep="_")
  
  # predict y values, conditional on random effect levels
  predData.list.mean <- list()
  predData.list.mean[["group_2012"]] <- predict(object = mod, newdata = newDf.list[["group_2012"]])
  predData.list.mean[["group_2013"]] <- predict(object = mod, newdata = newDf.list[["group_2013"]])
  predData.list.err <- list()
  require(merTools)
  predData.list.err[["group_2012"]] <- predictInterval(merMod = mod, # require(merTools)
                                                       newdata = newDf.list[["group_2012"]], 
                                                       n.sims = 999, include.resid.var=T)
  predData.list.err[["group_2013"]] <- predictInterval(merMod = mod, # require(merTools)
                                                       newdata = newDf.list[["group_2013"]], 
                                                       n.sims = 999, include.resid.var=T)
  # add ypred columns back to the original data frame
  df.tmp.list<-list()
  for(i in 1:length(newDf.list)){
    df.tmp<-data.frame(newDf.list[[i]], 
                       ypred=predData.list.mean[[i]], 
                       CI=predData.list.err[[i]])
    df.tmp.list[[i]]<-df.tmp
  }
  names(df.tmp.list) <- names(newDf.list)
  df.pred <- list_to_df(df.tmp.list)
  
  return(df.pred)
  
}

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

InteractionPlot_DF <- function(n, xVar, factorVar, factorBin, constantVar, df, mod){
  
  #1) Set up parameters to loop through for each new dataset
  select.year.vec<-c(2012, 2012, 2013, 2013)
  select.factorBin.vec<-c(1,2,1,2)
  if(length(constantVar) == 2){
    constantVar.mean1<-mean(df[,constantVar[1]])
    constantVar.mean2<-mean(df[,constantVar[2]])
  }else{
    constantVar.mean<-mean(df[,constantVar])  
  }
  
  
  #2) Create new dataframes that represent the different scenarios to plot
  newDf.list<-list()
  for(i in 1:length(select.year.vec)){
    
    #identify the current bin for factorVar and subset the data
    current.Bin<-levels(df[,factorBin])[select.factorBin.vec[i]]
    factorLevel<-rep(current.Bin, n)
    current.df<-df[df[,factorBin] == current.Bin,]
    
    #mean of the constant var (from full dataset)
    if(length(constantVar) == 2){
      V_constantVar1<-rep(constantVar.mean1, n)
      V_constantVar2<-rep(constantVar.mean2, n)
    }else{
      V_constantVar<-rep(constantVar.mean, n)  
    }
    
    #mean of the current bin for factorVar within the current bin
    V_factorVar<-rep(mean(current.df[,factorVar]), n)
    
    #range of Mv biomass within the current bin
    xVarRange<-range(current.df[,xVar])
    V_xVar<-seq(from=xVarRange[1], to=xVarRange[2], length.out=n)
    
    #current year
    V_year<-rep(select.year.vec[i], n)
    
    #make df
    if(length(constantVar) == 2){
      df.new<-data.frame(V_year, V_xVar, V_factorVar, factorLevel, V_constantVar1, V_constantVar2)
      colnames(df.new)<-c('year', xVar, factorVar, 'factorLevel', constantVar)
    }else{
      df.new<-data.frame(V_year, V_xVar, V_factorVar, factorLevel, V_constantVar)
      colnames(df.new)<-c('year', xVar, factorVar, 'factorLevel', constantVar)
    }
    newDf.list[[i]]<-df.new
    
  }
  names(newDf.list)<-paste('group',select.factorBin.vec,select.year.vec, sep="_")
  
  #3) Predict y values for each scenario
  predData.list.fit<- lapply(newDf.list, function(x){
    result <- predict(object = mod, newdata = x) #conditional on random effect levels
    return(result)
  })
  predData.list <- lapply(newDf.list, function(x){
    result <- predictInterval(newdata = x, merMod=mod, n.sims = 999, include.resid.var=TRUE)
    return(result)
  })
  
  #4) Add ypred columns back to the original data frames
  df.tmp.list<-list()
  for(i in 1:length(newDf.list)){
    df.tmp<-data.frame(newDf.list[[i]], ypred=predData.list.fit[[i]], CI=predData.list[[i]])
    df.tmp.list[[i]]<-df.tmp
  }
  names(df.tmp.list)<-names(newDf.list)
  df.pred <- list_to_df(df.tmp.list)
  
  return(df.pred)
  
}


