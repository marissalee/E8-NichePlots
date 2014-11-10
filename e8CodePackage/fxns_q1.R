#fxns_q1.R


#FUNCTION TO PULL PVALS FROM LINEAR MODEL FIT
GetPvals<-function(fit){
  store<-numeric(0)
  pvalx<-summary(fit)$coefficients[2,4]
  r2<-summary(fit)$r.squared
  row<-c(pvalx, r2)
  store<-rbind(store,row)
  colnames(store)<-c('pvalx','r2')  
  return(store)
}


#FUNCTION TO PULL COEFS FROM LINEAR MODEL FIT
GetCoefs<-function(fit){
  store<-numeric(0)
  int<-summary(fit)$coefficients[1,1]
  coefx<-summary(fit)$coefficients[2,1]
  row<-c(int, coefx)
  store<-rbind(store,row)
  colnames(store)<-c('int', 'coefx')
  return(store)
}





