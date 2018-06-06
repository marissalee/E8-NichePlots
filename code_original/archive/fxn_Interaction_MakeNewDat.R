#fxn_Interaction_MakeNewDat.R

#Fxn to predict ys of different groups within the model so that I can plot interaction effects
Interaction_MakeNewDat<-function(df, factorVar, xVar, select.yearLevel, select.factorLevel){
  
  #year and yObs
  year<-df[df$year==select.yearLevel,c('year')]
  yObs<-df[df$year==select.yearLevel,c('select.diffVar')]
  
  #xVar
  x<-df[df$year==select.yearLevel,c(xVar)]
  
  #factorVar
  if(select.factorLevel == 'high'){
    factorVal<-max(df[df$year==select.yearLevel,c(factorVar)])
  }
  if(select.factorLevel == 'low'){
    factorVal<-min(df[df$year==select.yearLevel,c(factorVar)])
  }
  factor<-rep(factorVal, length(year))
  factorLevel<-rep(select.factorLevel, length(year))
  
  #otherVars
  otherVars<-colnames(df)[!(colnames(df) %in% c('plotYear','year','select.diffVar', xVar, factorVar))]
  other<-rep(mean(df[df$year==select.yearLevel,c(otherVars)]), length(year))
  
  #put everything together
  df.group<-data.frame(year, yObs, x, factor, factorLevel, other)
  colnames(df.group)<-c('year','yObs',xVar,factorVar,'factorLevel',otherVars)
  
  return(df.group)
}
