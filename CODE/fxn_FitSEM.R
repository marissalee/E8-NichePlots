#fxn_FitSEM.R


FitSEM<-function(df.mod, diffVarShort){
  
  #prep dataset
  df.full<-merge(df.mod, data.q2)
  select.diffVar<-paste('Diff',diffVarShort, sep='_')
  selectCols<-c('plotYear','year','mv_g.m2_logt',select.diffVar,'PC1','PC2.use')
  df<-df.full[,selectCols]
  colnames(df)<-c('plotYear','year','mv','impact','PC1','PC2')
  
  #evaluate multivariate normality
  mn<-mult.norm(df[,c('PC1','PC2','mv','impact')], chicrit = 0.005)
  
  #define model
  modlist <- list(
    lme(mv ~ PC1 + PC2, random=~1|year, data=df),
    lme(impact ~ mv + PC1 + PC2 + 
          mv:PC1 + mv:PC2 + PC1:PC2 + 
          mv:PC1:PC2, random= ~1|year, data=df)
  )
  
  #does the model fit the data?
  sem_fit<-sem.fit(modelList=modlist, data=df) 
  
  #path coefs
  coef_nonstd<-sem.coefs(modelList=modlist, data=df, standardize="none")
  coef_std<-sem.coefs(modelList=modlist, data=df, standardize="scale")
  
  #R2 values
  r2<-sem.model.fits(modlist)

  result<-list(mn=mn, sem_fit=sem_fit, coef_nonstd=coef_nonstd, coef_std=coef_std, r2=r2)
  return(result)
  
}