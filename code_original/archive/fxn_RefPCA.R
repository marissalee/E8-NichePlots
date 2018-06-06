#fxn_RefPCA.R


#Fxn: do the reference condition ordination
RefPCA<-function(diffVar.basic, refVars, df, externalPercBA_AM){
  
  #identify impact measures
  select.diffVarTB<-paste('Diff',diffVar.basic, c('T','B'), sep='_')
  
  #identify variables to remove from refVars and the select.refVars and create dataframe
  excludeVars<-vars.indx[vars.indx$basic.vars == diffVar.basic,'vars']
  select.refVars<-refVars[!refVars %in% excludeVars]
  df.ref<-data.frame(df[,select.refVars], year=df$year)
  rownames(df.ref)<-df$plotYear
  df.ref.rm<-df.ref[!rowSums(is.na(df.ref)),]
  
  #ordinate select.refVars (without the year column)
  tmp<-df.ref.rm[,!(colnames(df.ref.rm) == 'year')]
  tmp.scaled<-scale(tmp)
  PCA.ref<-prcomp(tmp.scaled)
  pPCA.ref<-ggbiplot(PCA.ref, groups=factor(df.ref.rm$year), ellipse=FALSE) + mytheme

  #identify model dataframe
  df.PCA.ref<-data.frame(plotYear=rownames(PCA.ref$x), PCA.ref$x)
  df.sub<-data.frame(plotYear=df$plotYear, 
                     year=df$year, 
                     select.diffVarT=df[,select.diffVarTB[1]], 
                     select.diffVarB=df[,select.diffVarTB[2]], 
                     mv_g.m2_logt=df$mv_g.m2_logt)
  if(externalPercBA_AM=='y'){
    df.sub<-data.frame(df.sub, MycBin=df$MycBin)
  }
  df.mod<-merge(df.sub, df.PCA.ref[,c(1:3)])
  
  #save everything in a list
  results<-list(select.diffVarTB=select.diffVarTB, select.refVars=select.refVars, 
               PCA.ref=PCA.ref, pPCA.ref=pPCA.ref, 
               df.mod=df.mod)
  
  return(results)
  
}
