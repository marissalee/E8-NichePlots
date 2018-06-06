#fxn_ExplainDiffVar.R

#Fxn to run the models with reference conditions (excluding linked response variable) and Microstegium biomass predicting different y variables
ScaleFit<-function(df.mod, scale_xs=TRUE){
  
  df.mod.used<-df.mod
  if(scale_xs == TRUE){
    #scale predictors
    df.mod.used[,c('PC1','PC2','mv_g.m2_logt')]<-scale(df.mod.used[,c('PC1','PC2','mv_g.m2_logt')])
  }
  
  #fit model (0-5cm, T)
  modT<-lmer(select.diffVarT ~ PC1 * PC2 * mv_g.m2_logt + (1|year), data=df.mod.used)
  summaryModT<-summary(modT)
  
  #fit model (5-15cm, B)
  modB<-lmer(select.diffVarB ~ PC1 * PC2 * mv_g.m2_logt + (1|year), data=df.mod.used)
  summaryModB<-summary(modB)
  
  #save everything
  results<-list(df.mod.used=df.mod.used, 
                modT=modT, summaryModT=summaryModT,
                modB=modB, summaryModB=summaryModB)
  
  return(results)
}