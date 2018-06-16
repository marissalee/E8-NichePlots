
make_sem_df.pcs <- function(resp.diff, data.q3, impact.df, ref.scrs.list){
  
  mv.dat <- data.q3[,c("year","plotid","mv","mv.logt")]
  
  ref.pcs <- ref.scrs.list[[resp.diff]]
  ref.pcs %>%
    left_join(mv.dat) %>%
    transform(year = as.factor(year))-> dat1
  
  respvar <- paste0(resp.diff, "_Diff")
  impact.df %>%
    filter(variable == respvar) %>%
    select(year, plotid, value) %>%
    transform(year = factor(year)) %>%
    rename('impact'='value') -> respvar.dat
  dat1 %>%
    left_join(respvar.dat) -> df
  
  return(df)
}

make_sem_df <- function(resp.diff, envvars, impact.df){
  
  real.resp.diff<- paste0(resp.diff,"_Diff")
  select.vars <- c(real.resp.diff, envvars, "mv_g.m2","mv_g.m2_logt")
  
  impact.df %>%
    filter(variable %in% select.vars) %>%
    select(-type) %>%
    spread(key = variable, value = value) %>%
    rename_("impact" = real.resp.diff) %>%
    rename('mv' = "mv_g.m2",
           'mv.logt' = "mv_g.m2_logt") %>%
    mutate(percpar12.logt = log(percpar12))-> df
  
  return(df)
}

FitSEM.pcs <- function(df){
  
  #evaluate multivariate normality
  df.valsonly <- df[,!colnames(df) %in% c("plotid","year","mv")]
  mn <- mult.norm(df.valsonly, chicrit = 0.005) 
  
  #define model
  modlist <- list(
    lme(mv.logt ~ PC1 + PC2, random=~1|year, data=df),
    lme(impact ~ mv.logt + PC1 + PC2 + PC1:mv.logt + PC2:mv.logt + PC1:PC2, random= ~1|year, data=df)
  )
  
  #does the model fit the data?
  sem_fit <- sem.fit(modelList=modlist, data=df, conditional = T) 
  sem_fit
  
  #path coefs
  coef_nonstd<-sem.coefs(modelList=modlist, data=df, standardize="none")
  coef_std<-sem.coefs(modelList=modlist, data=df, standardize="scale")
  coef_std
  
  #R2 values
  r2<-sem.model.fits(modlist)
  
  result<-list(mn=mn, sem_fit=sem_fit, coef_nonstd=coef_nonstd, coef_std=coef_std, r2=r2)
  return(result)
  
}

FitSEM.T.nitrif <- function(df){
  
  #evaluate multivariate normality
  df.valsonly <- df[,!colnames(df) %in% c("plotid","year")]
  mn <- mult.norm(df.valsonly, chicrit = 0.005) 
  
  #define model
  modlist <- list(
    lme(mv.logt ~ BA_total + percpar12.logt, random=~1|year, data=df),
    lme(impact ~ mv.logt + soilmoi_T + noi_T, random= ~1|year, data=df)
  )
  
  #does the model fit the data?
  sem_fit<-sem.fit(modelList=modlist, data=df, conditional = T) 
  sem_fit
  
  #path coefs
  coef_nonstd<-sem.coefs(modelList=modlist, data=df, standardize="none")
  coef_std<-sem.coefs(modelList=modlist, data=df, standardize="scale")
  
  #R2 values
  r2<-sem.model.fits(modlist)
  
  result<-list(mn=mn, sem_fit=sem_fit, coef_nonstd=coef_nonstd, coef_std=coef_std, r2=r2)
  return(result)
  
}

FitSEM.T.minz <- function(df){
  
  #evaluate multivariate normality
  df.valsonly <- df[,!colnames(df) %in% c("plotid","year")]
  mn <- mult.norm(df.valsonly, chicrit = 0.005) 
  
  #define model
  modlist <- list(
    lme(mv.logt ~ BA_total + percpar12.logt, random=~1|year, data=df),
    lme(impact ~ mv.logt + soilmoi_T + noi_T, random= ~1|year, data=df)
  )
  
  #does the model fit the data?
  sem_fit<-sem.fit(modelList=modlist, data=df, conditional = T) 
  sem_fit
  
  #path coefs
  coef_nonstd<-sem.coefs(modelList=modlist, data=df, standardize="none")
  coef_std<-sem.coefs(modelList=modlist, data=df, standardize="scale")
  coef_std
  
  #R2 values
  r2<-sem.model.fits(modlist)
  
  result<-list(mn=mn, sem_fit=sem_fit, coef_nonstd=coef_nonstd, coef_std=coef_std, r2=r2)
  return(result)
  
}

FitSEM.B.ammonif <- function(df){
  
  #evaluate multivariate normality
  df.valsonly <- df[,!colnames(df) %in% c("plotid","year")]
  mn <- mult.norm(df.valsonly, chicrit = 0.005) 
  
  #define model
  modlist <- list(
    lme(mv.logt ~ BA_total + percpar12.logt, random=~1|year, data=df),
    lme(impact ~ nat_g.m2 + noi_B + BA_total + mv.logt + noi_B:mv.logt, 
        random= ~1|year, data=df)
  )
  
  #does the model fit the data?
  sem_fit<-sem.fit(modelList=modlist, data=df, conditional = T) 
  sem_fit
  
  #path coefs
  coef_nonstd<-sem.coefs(modelList=modlist, data=df, standardize="none")
  coef_std<-sem.coefs(modelList=modlist, data=df, standardize="scale")
  coef_std
  
  #R2 values
  r2<-sem.model.fits(modlist)
  
  result<-list(mn=mn, sem_fit=sem_fit, coef_nonstd=coef_nonstd, coef_std=coef_std, r2=r2)
  return(result)
  
}

FitSEM.B.minz <- function(df){
  
  #evaluate multivariate normality
  df.valsonly <- df[,!colnames(df) %in% c("plotid","year")]
  mn <- mult.norm(df.valsonly, chicrit = 0.005) 
  
  #define model
  modlist <- list(
    lme(mv.logt ~ BA_total + percpar12.logt, random=~1|year, data=df),
    lme(impact ~ nat_g.m2 + noi_B + mv.logt + noi_B:mv.logt, 
        random= ~1|year, data=df)
  )
  
  #does the model fit the data?
  sem_fit<-sem.fit(modelList=modlist, data=df, conditional = T) 
  sem_fit
  
  #path coefs
  coef_nonstd<-sem.coefs(modelList=modlist, data=df, standardize="none")
  coef_std<-sem.coefs(modelList=modlist, data=df, standardize="scale")
  coef_std
  
  #R2 values
  r2<-sem.model.fits(modlist)
  
  result<-list(mn=mn, sem_fit=sem_fit, coef_nonstd=coef_nonstd, coef_std=coef_std, r2=r2)
  return(result)
  
}

FitSEM.B.minz_addPath <- function(df){
  
  #evaluate multivariate normality
  df.valsonly <- df[,!colnames(df) %in% c("plotid","year")]
  mn <- mult.norm(df.valsonly, chicrit = 0.005) 
  
  #define model
  modlist <- list(
    lme(mv.logt ~ BA_total + percpar12.logt, random=~1|year, data=df),
    lme(impact ~ nat_g.m2 + noi_B + mv.logt + BA_total + noi_B:mv.logt, 
        random= ~1|year, data=df)
  )
  
  #does the model fit the data?
  sem_fit<-sem.fit(modelList=modlist, data=df, conditional = T) 
  sem_fit
  
  #path coefs
  coef_nonstd<-sem.coefs(modelList=modlist, data=df, standardize="none")
  coef_std<-sem.coefs(modelList=modlist, data=df, standardize="scale")
  coef_std
  
  #R2 values
  r2<-sem.model.fits(modlist)
  
  result<-list(mn=mn, sem_fit=sem_fit, coef_nonstd=coef_nonstd, coef_std=coef_std, r2=r2)
  return(result)
  
}

