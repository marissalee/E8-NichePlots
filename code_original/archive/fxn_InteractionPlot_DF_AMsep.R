#fxn_InteractionPlot_DF.R

# n=100
# xVar='mv_g.m2_logt' 
# factorVar='PC2'
# factorBin='factorLevel'
# constantVar='PC1'
# df=df.mod
# mod=result.mod[['modT']]

InteractionPlot_DF<-function(n, xVar, factorVar, factorBin, constantVar, df, mod){
  
  #1) Set up parameters to loop through for each new dataset
  select.year.vec<-c(2012, 2012, 2013, 2013)
  select.factorBin.vec<-c(1,2,1,2)
  constantVar.mean<-mean(df.mod[,constantVar])
  
  #2) Create new dataframes that represent the different scenarios to plot
  newDf.list<-list()
  i<-0
  for(i in 1:length(select.year.vec)){
    
    #identify the current bin for factorVar and subset the data
    df[,factorBin]
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

  return(df.pred)
  
}









