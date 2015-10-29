#fxn_FitPlot


FitPlot.Mv<-function(dataset){
  
  require(lme4)
  require(lmerTest)
  require(ggplot2)
  
  #initialize lists
  list.results<-list()
  list.data<-list()
  list.figures<-list()
  
  varLevels<-levels(dataset$variable)
  i <- 0 #VARIABLES
  for (i in 1:length(varLevels)){
    
    #subset data by the current variable
    dataset.sub <- subset(dataset, variable == varLevels[i]) #subset data by variable
    df<-dataset.sub[!is.na(dataset.sub$value) & !is.na(dataset.sub$mv_I),] #remove NAs
    
    #fit the mixed effect model (regression)
    mod <- lmer(mvI_logt ~ value + (1|year), data = df)
    mod0 <- lmer(mvI_logt ~ 1 + (1|year), data = df) #reduced model
    
    #save fit stats
    int<-round(mod@beta[1], digits=2) #intercept
    est<-round(mod@beta[2], digits=2) #slope
    pVal.est<-round(summary(mod)$coefficients[2,'Pr(>|t|)'], digits=2) #pval for slope
    df.est<-round(summary(mod)$coefficients[2,'df'], digits=2) #df for slope
    #McFadden's pseudo-R-squared using loglik (aka likelihood ratio index)
    #see http://www.ats.ucla.edu/stat/mult_pkg/faq/general/Psuedo_RSquareds.htm
    ll<-logLik(mod)
    ll0<-logLik(mod0)
    pR2<-round(1-(ll/ll0), digits=2)
    result<-data.frame(int, est, pVal.est, df.est, pR2)
    
#     #predict values using model fit
    df$pred<-predict(mod, re.form=NA) #this sets the random effects to 0
#     bb <- bootMer(mod,
#                   FUN=function(x)predict(x, re.form=NA),
#                   nsim=500)
#     df$ci.lb <- apply(bb$t, 2, quantile, 0.025)
#     df$ci.ub <- apply(bb$t, 2, quantile, 0.975)
    
    #1. save the model fit results
    list.results[[i]] <- result
    
    #2. save the dataset used for plotting
    list.data[[i]] <- df
    
    #make a plot panel of mv_I vs variable value
    p<-ggplot(df, aes(x=value, y=mvI_logt, color=year)) + 
      geom_point() + mytheme + 
      labs(x=NULL, y=NULL) + guides(color=FALSE) + ggtitle(varLevels[i])
    
    #add model fit if needed
    if(sum(result$pVal.est<0.1)>0){
      p<-p + 
        geom_line(aes(y=pred), color='black', size=1) +
        mytheme
      #geom_ribbon(aes(ymin=ci.lb,ymax=ci.ub),alpha=0.3, color='gray')+
    }
    
    #3. save the plot panel
    list.figures[[i]]<-p
    
  }
  #name lists
  names(list.results)<-varLevels
  names(list.data)<-varLevels
  names(list.figures)<-varLevels
  
  #Save everything in a big list
  result.list<-list(results=list.results, 
                    data=list.data,
                    figures=list.figures)
  
  return(result.list)
  
}
