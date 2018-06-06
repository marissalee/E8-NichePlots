#fxn_XYPlot_DF2.R


XYPlot_DF2<-function(yVar, xVar, df, mod, otherVars){
  
  #1) Set up data frame and model
  df.plot<-df
  df.plot$year<-factor(df.plot$year)
  select.year.vec<-c(2012, 2013)
  
  #2) Create new dataframes that represent the different scenarios to plot
  df.group.list<-list()
  i<-0
  for(i in 1:length(select.year.vec)){
    
    select.yearLevel<-select.year.vec[i]  
    
    #year and yObs
    year<-df.plot[df.plot$year==select.yearLevel,c('year')]
    yObs<-df.plot[df.plot$year==select.yearLevel,c(yVar)]
    
    #xVar
    x<-df.plot[df.plot$year==select.yearLevel,c(xVar)]
    
    #otherVars
    if(length(otherVars)==1){
      other<-rep(mean(df.plot[,otherVars]), times=length(year))
    }else{
      otherVars_means<-apply(df.plot[,otherVars],2, mean)
      other<-data.frame(alply(.data=otherVars_means, 1, rep, times=length(year)))
      colnames(other)<-otherVars
    }
    
    #put everything together
    df.group<-data.frame(year, yObs, x, other)
    colnames(df.group)<-c('year','yObs',xVar, otherVars)
    
    df.group.list[[i]]<-df.group
  }
  names(df.group.list)<-paste('year',select.year.vec, sep="_")
  
  #3) Predict y values for each scenario
  predData.list.fit<-llply(df.group.list, predict, object=mod)
  #predData.list<-llply(df.group.list, predictInterval, merMod=mod, n.sims = 999)
  
  #4) Add ypred columns back to the original data frames
  df.tmp.list<-list()
  i<-0
  for(i in 1:length(df.group.list)){
    df.tmp<-data.frame(df.group.list[[i]], ypred=predData.list.fit[[i]])
    df.tmp.list[[i]]<-df.tmp
  }
  names(df.tmp.list)<-names(df.group.list)
  df.pred<-ldply(df.tmp.list)
  
  #rename scenarios so they are pretty
  colnames(df.pred)[colnames(df.pred) =='.id']<-'FactorYear'
  df.pred
  return(df.pred)
  
}

