
FitSEM.nitrifdT <- function(impact.df){
  
  resp.diff <- "nitrifd_T_Diff"
  envvars <- c("nhi_T","som_T","nTrees")
  
  # extract environmental variables, mv, and diff response variable
  select.vars <- c(resp.diff, envvars, "mv_g.m2_logt")
  impact.df %>%
    filter(variable %in% select.vars) %>%
    select(-type) %>%
    spread(key = variable, value = value) %>%
    rename_("impact" = resp.diff) %>%
    rename('mv' = "mv_g.m2_logt") -> df
  
  detach(package:lmerTest, TRUE)
  detach(package:lme4, TRUE)
  
  #evaluate multivariate normality
  df.valsonly <- df[,!colnames(df) %in% c("plotid","year")]
  require(QuantPsyc)
  mn <- mult.norm(df.valsonly, chicrit = 0.005) 
  mn
  
  #define model
  require(nlme)
  modlist <- list(
    lme(mv ~ nTrees, random=~1|year, data=df),
    lme(impact ~ mv + nhi_T + som_T + nhi_T:mv + som_T:mv, random= ~1|year, data=df)
  )
  
  #does the model fit the data?
  require(piecewiseSEM)
  sem_fit<-sem.fit(modelList=modlist, data=df, conditional = T) 
  
  #path coefs
  coef_nonstd<-sem.coefs(modelList=modlist, data=df, standardize="none")
  coef_std<-sem.coefs(modelList=modlist, data=df, standardize="scale")
  
  #R2 values
  r2<-sem.model.fits(modlist)
  
  detach(package:QuantPsyc, TRUE)
  detach(package:nlme, TRUE)
  detach(package:piecewiseSEM, TRUE)
  
  result<-list(mn=mn, sem_fit=sem_fit, coef_nonstd=coef_nonstd, coef_std=coef_std, r2=r2)
  return(result)
  
}