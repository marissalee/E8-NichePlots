#fxn_ScaleFit_q2.R

#Fxn to run the models with reference conditions (excluding linked response variable) and Microstegium biomass predicting different y variables
ScaleFit_q2<-function(diffVar.basic, df.mod, scale_xs=FALSE, modelFormulaCode, externalPercBA_AM){
  
  df.mod.used<-df.mod
  
  if(externalPercBA_AM=='y'){
    df.mod.used$MycBin<-factor(df.mod.used$MycBin)
  }
  
  if(diffVar.basic %in% c('nitrifd','minzd')){
    df.mod.used$PC2<-df.mod.used$PC2 *-1 #because the variable loadings PC2 are flipped for these ordinations
  }
  
#   if(scale_xs == TRUE){
#     #scale predictors
#     df.mod.used[,c('PC1','PC2','mv_g.m2_logt','MycBin')]<-scale(df.mod.used[,c('PC1','PC2','mv_g.m2_logt','MycBin')])
#   }
  
  if(modelFormulaCode == 'PC1*PC2*Mv'){
    #fit model (0-5cm, T)
    modT<-lmer(select.diffVarT ~ PC1 * PC2 * mv_g.m2_logt + (1|year), data=df.mod.used)
    summaryModT<-summary(modT)
    #fit model (5-15cm, B)
    modB<-lmer(select.diffVarB ~ PC1 * PC2 * mv_g.m2_logt + (1|year), data=df.mod.used)
    summaryModB<-summary(modB)
  }
  
  if(modelFormulaCode == 'PC1+PC2+Mv'){
    #fit model (0-5cm, T)
    modT<-lmer(select.diffVarT ~ PC1 + PC2 + mv_g.m2_logt + (1|year), data=df.mod.used)
    summaryModT<-summary(modT)
    #fit model (5-15cm, B)
    modB<-lmer(select.diffVarB ~ PC1 + PC2 + mv_g.m2_logt + (1|year), data=df.mod.used)
    summaryModB<-summary(modB)
  }
  
  if(modelFormulaCode == 'PC1*PC2*AM*Mv'){
    #fit model (0-5cm, T)
    modT<-lmer(select.diffVarT ~ PC1 * PC2 * MycBin * mv_g.m2_logt + (1|year), data=df.mod.used)
    summaryModT<-summary(modT)
    #fit model (5-15cm, B)
    modB<-lmer(select.diffVarB ~ PC1 * PC2 * MycBin * mv_g.m2_logt + (1|year), data=df.mod.used)
    summaryModB<-summary(modB)
  }
  
  if(modelFormulaCode == 'PC1+PC2+AM+Mv'){
    #fit model (0-5cm, T)
    modT<-lmer(select.diffVarT ~ PC1 + PC2 + MycBin + mv_g.m2_logt + (1|year), data=df.mod.used)
    summaryModT<-summary(modT)
    #fit model (5-15cm, B)
    modB<-lmer(select.diffVarB ~ PC1 + PC2 + MycBin + mv_g.m2_logt + (1|year), data=df.mod.used)
    summaryModB<-summary(modB)
  }
  
  #paste the model summaries together
  summT<-data.frame(term=rownames(summaryModT$coefficients),
             depth=rep('0-5cm',dim(summaryModT$coefficients)[1]),
             summaryModT$coefficients)
  summB<-data.frame(term=rownames(summaryModB$coefficients),
                    depth=rep('5-15cm',dim(summaryModB$coefficients)[1]),
                    summaryModB$coefficients)
  summ<-rbind(summT, summB)
  colnames(summ)[3:7]<-c('est','se','df','tval','pval')
  
  #save everything
  results<-list(df.mod.used=df.mod.used, 
                modT=modT, modB=modB, 
                summ=summ)
  
  return(results)
}