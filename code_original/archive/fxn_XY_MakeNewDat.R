#fxn_XY_MakeNewDat.R

#Fxn to predict ys of different groups within the model so that I can plot interaction effects
XY_MakeNewDat<-function(df, xVar, select.yearLevel){
  
  #year and yObs
  year<-df[df$year==select.yearLevel,c('year')]
  yObs<-df[df$year==select.yearLevel,c('select.diffVar')]
  
  #xVar
  x<-df[df$year==select.yearLevel,c(xVar)]
  
  #otherVars
  otherVars<-colnames(df)[!(colnames(df) %in% c('plotYear','year','select.diffVar', xVar))]
  mean(df[,otherVars])
  other<-data.frame(alply(.data=otherVars_means, 1, rep, times=length(year)))
  colnames(other)<-otherVars
  
  #put everything together
  df.group<-data.frame(year, yObs, x, other)
  colnames(df.group)<-c('year','yObs',xVar, otherVars)
  
  return(df.group)
}
