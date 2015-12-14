#fxn_reshapeHelpers.R

QuickReshape1<-function(variables, variableNames, refOnly='n'){
  data.vars<-data.choice[data.choice$variable %in% variables, c('plotid','plothalfid1','inv','year','variable','value')]
  data.vars$variable<-factor(data.vars$variable, levels=variables)
  data.vars.wide<-dcast(data.vars, plothalfid1 + plotid + inv+ year ~ variable, value.var='value')
  data.vars.wide$plotInvYear<-paste(data.vars.wide$plothalfid1, data.vars.wide$year, sep="_") #use this to identify rows
  data.vars.wide$plotYear<-paste(data.vars.wide$plotid, data.vars.wide$year, sep="_") #use this to block by plotid*year
  row.names(data.vars.wide)<-data.vars.wide$plotInvYear
  NonValCols<-c('plothalfid1','plotid','plotInvYear', 'plotYear','inv','year')
  ValCols<-colnames(data.vars.wide)[!(colnames(data.vars.wide) %in% NonValCols)]
  
  if(refOnly=='y'){
    data.vars.wide<-data.vars.wide[data.vars.wide$inv=='N',]
  }

  data.vars.wide.rm<-data.vars.wide[!rowSums(is.na(data.vars.wide))>0,] #remove rows with missing data
  df.ord<-data.vars.wide.rm[,ValCols]
  colnames(df.ord)<-variableNames
  
  corel<-cor(data.vars.wide.rm[,ValCols]) #look at correlation matrix
  
  results<-list(df.ord=df.ord, corel=corel, 
                NonValCols=NonValCols, ValCols=ValCols, 
                data.vars.wide.rm=data.vars.wide.rm)
  
  return(results)
}