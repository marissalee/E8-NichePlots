#fxn_StdPathCoefs.R


#Function to standardize path coefs
StdPathCoefs<-function(sem.coef.obj, data){
  
  #1. pull non-standardized path coef
  unstd.coefs<-sem.coef.obj[,'estimate']
  
  #2. identify whether interaction terms are in the models. if present, add those columns to data
  #only deals with 2-way interactions
  numInteractions<-sum(grepl(':',sem.coef.obj[,'predictor'])) 
  if(numInteractions>0){
    colNames.Interactions<-sem.coef.obj[grepl(':',sem.coef.obj[,'predictor']),'predictor']
    i<-0
    for(i in 1:length(colNames.Interactions)){
      termName.vec<-unlist(strsplit(as.character(colNames.Interactions[i]),':'))
      x1<-data[,as.character(termName.vec[1])]
      x2<-data[,as.character(termName.vec[2])]
      x1x2<-x1*x2
      data[,as.character(colNames.Interactions[i])]<-x1x2
    }
  }
  
  #3. identify x and y data columns for each coef
  std.coefs<-list()
  i<-0
  for(i in 1:length(unstd.coefs)){
    
    #identify x and y variable names
    y.var.name<-sem.coef.obj[sem.coef.obj$estimate==unstd.coefs[i],'response']
    x.var.name<-sem.coef.obj[sem.coef.obj$estimate==unstd.coefs[i],'predictor']
    
    #identify x and y data
    y<-data[,as.character(y.var.name)]
    x<-data[,as.character(x.var.name)]
    
    #calculated standardized path coef
    std.coef<-unstd.coefs[i]*sd(x)/sd(y)
    std.coefs[[i]]<-std.coef
    
  }
  sem.coef.obj$std.estimate<-unlist(std.coefs)
  
  return(sem.coef.obj)
}
