#fxn_reshapeHelpers.R

QuickReshape1<-function(variables, variableNames, select.inv){
  
  #subset 'data.choice' by 'variables'
  data.vars<-data.choice[data.choice$variable %in% variables, c('plotid','plothalfid1','inv','year','variable','value')]
  data.vars$variable<-factor(data.vars$variable, levels=variables)
  
  #make variables into columns and observations into rows, add row labels
  data.vars.wide<-dcast(data.vars, plothalfid1 + plotid + inv+ year ~ variable, value.var='value')
  data.vars.wide$plotInvYear<-paste(data.vars.wide$plothalfid1, data.vars.wide$year, sep="_") #use this to identify rows
  data.vars.wide$plotYear<-paste(data.vars.wide$plotid, data.vars.wide$year, sep="_") #use this to block by plotid*year
  row.names(data.vars.wide)<-data.vars.wide$plotInvYear
  
  #select whether to include 'invaded', 'reference', or 'both' plot information
  if(select.inv=='invaded'){
    data.vars.wide<-data.vars.wide[data.vars.wide$inv=='I',]
  }
  if(select.inv=='reference'){
    data.vars.wide<-data.vars.wide[data.vars.wide$inv=='N',]
  }
  if(select.inv=='both'){
    data.vars.wide<-data.vars.wide[data.vars.wide$inv %in% c('I','N'),]
  }
  
  #identify value and non-value columns
  NonValCols<-c('plothalfid1','plotid','plotInvYear', 'plotYear','inv','year')
  ValCols<-colnames(data.vars.wide)[!(colnames(data.vars.wide) %in% NonValCols)]
  
  #remove rows with missing data
  data.vars.wide.rm<-data.vars.wide[!rowSums(is.na(data.vars.wide))>0,]
  df.ord<-data.vars.wide.rm[,ValCols]
  colnames(df.ord)<-variableNames
  
  #save the correlation matrix
  corel<-cor(data.vars.wide.rm[,ValCols]) 
  
  results<-list(df.ord=df.ord, corel=corel, 
                NonValCols=NonValCols, ValCols=ValCols, 
                data.vars.wide.rm=data.vars.wide.rm)
  
  return(results)
}

#fxn to look up raw variable values that correspond to min/max PC values
PullPCVals<-function(df, orig.df, PC, RefVars, min.or.max){
  
  #find the min/max observation
  if(min.or.max=='min'){
    pc.val<-min(df.tmp[,PC])
  }
  if(min.or.max=='max'){
    pc.val<-max(df.tmp[,PC])
  }
  
  #identify plotid and year of that observation
  obs.val<-as.character(df[df[,PC]==pc.val,'plotYear'])
  plotid.val<-strsplit(obs.val, "_")[[1]][1]
  year.val<-strsplit(obs.val, "_")[[1]][2]
  
  #identify RefVars for that observation plotid in the original dataset
  result<-orig.df[orig.df$plotid==plotid.val & 
                    orig.df$year==year.val & 
                    orig.df$inv=='N' & 
                    orig.df$variable %in% c(RefVars),
                  c('variable','value')]
  
  return(result)
  
}
