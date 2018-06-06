#Functions to fit models...

#FXN TO RUN A MIXED EFFECTS MODEL WITH MV_LOGT ~ YEAR * VALUE (1|DEPTH)
FitPlot.MvYrDp<-function(dataset){
  
  require(ggplot2)
  require(lme4)
  require(lmerTest)
  
  #initialize lists
  list.results<-list()
  list.data<-list()
  list.figures<-list()
  
  measCatLevels<-levels(dataset$measCat)
  i <- 0 #VARIABLES
  for (i in 1:length(measCatLevels)){
    
    #subset data by the current variable
    dataset.sub <- subset(dataset, measCat == measCatLevels[i]) #subset data by variable
    df<-dataset.sub[!is.na(dataset.sub$value) & !is.na(dataset.sub$mv_g.m2_I),] #remove NAs
    
    #make year numeric
    df$year <- as.numeric(df$year)
    
    #fit the mixed effect model (regression)
    mod <- lmer(mv_g.m2_I_logt ~ year + value + year:value + (1|depth), data = df)
    
    #save fit stats
    terms<-row.names(anova(mod))
    pvals.a<-round(anova(mod)[,'Pr(>F)'], digits =3)
    result<-data.frame(terms, pvals.a)
    
    #predict values using model fit
    df$pred<-predict(mod, re.form=NA) #this sets the random effects to 0
    
    #1a. save model results
    list.results[[i]] <- result
    
    #2. save the dataset used for plotting
    list.data[[i]] <- df
    
    #make a plot panel of mv_I vs variable value
    df$year<-factor(df$year)
    p<-ggplot(df, aes(x=value, y=mv_g.m2_I_logt, color=year, shape=depth)) + 
      geom_point() + mytheme + 
      labs(x=NULL, y=NULL) + guides(color=FALSE, shape=FALSE) + ggtitle(measCat_names[i])

    #add model fit if needed
    #if value coef differs from 0
    if(sum(result$pvals.a<0.1)>0){
      
      #prep plot annotation text
      list.rows<-list()
      k<-0
      signifrows<-result[result$pvals.a<0.1,]
      for(k in 1:dim(signifrows)[1]){
        list.rows[[k]]<-paste(signifrows[k,'terms'],signifrows[k,'pvals.a'])
      }
      tmp<-laply(list.rows, paste)
      pvalLabel<-paste(tmp, collapse="\n")
      
      #annotate plot
      p<-p + 
        geom_line(aes(y=pred), color='black', size=1) +
        mytheme
      
    }
    
    #3. save the plot panel
    list.figures[[i]]<-p
    
  }
  #name lists
  names(list.results)<-measCatLevels
  names(list.data)<-measCatLevels
  names(list.figures)<-measCatLevels
  
  #Save everything in a big list
  result.list<-list(results=list.results,
                    data=list.data,
                    figures=list.figures)
  
  return(result.list)
  
}

#FXN TO RUN A LINEAR MODEL WITH MV_LOGT ~ YEAR * VALUE (IN DIFF ORDERS)
FitPlot.MvYr<-function(dataset){
  
  require(ggplot2)
  require(lme4)
  require(lmerTest)
  
  #initialize lists
  list.mod1results<-list()
  list.mod2results<-list()
  list.mod1coefs<-list()
  list.data<-list()
  list.figures<-list()

  varLevels<-levels(dataset$variable)
  i <- 0 #VARIABLES
  for (i in 1:length(varLevels)){
    
    #subset data by the current variable
    dataset.sub <- subset(dataset, variable == varLevels[i]) #subset data by variable
    df<-dataset.sub[!is.na(dataset.sub$value) & !is.na(dataset.sub$mv_g.m2_I),] #remove NAs
    
    #make year numeric
    df$year <- as.numeric(df$year)
    
    #fit the mixed effect model (regression)
    mod1 <- lm(mv_g.m2_I_logt ~ year + value + year:value, data=df)
    mod2 <- lm(mv_g.m2_I_logt ~ value + year + value:year, data=df)
    
    #save fit stats
    #mod1
    pvals.a<-round(anova(mod1)[['Pr(>F)']], digits=3)
    tmpresults<-data.frame(terms=rownames(anova(mod1)),pvals.a)
    mod1.results<-tmpresults[tmpresults$terms != 'Residuals',]
    #mod2
    pvals.a<-round(anova(mod2)[['Pr(>F)']], digits=3)
    tmpresults<-data.frame(terms=rownames(anova(mod2)),pvals.a)
    mod2.results<-tmpresults[tmpresults$terms != 'Residuals',]
    #1a. save model results
    list.mod1results[[i]] <- mod1.results
    list.mod2results[[i]] <- mod2.results
    
    #2. save the dataset used for plotting
    list.data[[i]] <- df
    
    #make a plot panel of mv_I vs variable value
    df$year<-factor(df$year)
    p<-ggplot(df, aes(x=value, y=mv_g.m2_I_logt, color=year)) + 
      geom_point() + mytheme + 
      labs(x=NULL, y=NULL) + guides(color=FALSE) + ggtitle(var_names1[i])
    
    #add model fit if needed
    #if value coef differs from 0
    if(sum(mod1.results$pvals.a<0.1)>0){
      
      #prep plot annotation text
      list.rows<-list()
      k<-0
      signifrows<-mod1.results[mod1.results$pvals.a<0.1,]
      for(k in 1:dim(signifrows)[1]){
        list.rows[[k]]<-paste(signifrows[k,'terms'],signifrows[k,'pvals.a'])
      }
      tmp<-laply(list.rows, paste)
      pvalLabel<-paste(tmp, collapse="\n")
      
      #annotate plot
      p<-p + 
        stat_smooth(method='lm') +
        annotate('text', x=0, y=-1, label=pvalLabel, size=3, hjust=0)+
        mytheme
    }
    
    #3. save the plot panel
    list.figures[[i]]<-p
    
  }
  #name lists
  names(list.mod1results)<-varLevels
  names(list.mod2results)<-varLevels
  names(list.data)<-varLevels
  names(list.figures)<-varLevels
  
  #Save everything in a big list
  result.list<-list(mod1L=list.mod1results,mod2L=list.mod2results,
                    data=list.data,
                    figures=list.figures)
  
  return(result.list)
  
}

#FXN TO RUN A MIXED EFFECTS MODEL WITH MV_LOGT ~ VALUE + (1|YEAR)
FitPlot.Mv<-function(dataset){
  
  require(lme4)
  require(lmerTest)
  require(ggplot2)
  
  #initialize lists
  list.results<-list()
  list.results.coef<-list()
  list.data<-list()
  list.figures<-list()
  
  varLevels<-levels(dataset$variable)
  i <- 0 #VARIABLES
  for (i in 1:length(varLevels)){
    
    #subset data by the current variable
    dataset.sub <- subset(dataset, variable == varLevels[i]) #subset data by variable
    df<-dataset.sub[!is.na(dataset.sub$value) & !is.na(dataset.sub$mv_g.m2_I),] #remove NAs
    
    #fit the mixed effect model (regression)
    mod <- lmer(mv_g.m2_I_logt ~ value + (1|year), data = df)
    mod0 <- lmer(mv_g.m2_I_logt ~ 1 + (1|year), data = df) #reduced model
    
    #save fit stats
    terms<-row.names(anova(mod))
    pvals.a<-round(anova(mod)[,'Pr(>F)'], digits =3)
    result.fit<-data.frame(terms, pvals.a)
    
    #predict values using model fit
    df$pred<-predict(mod, re.form=NA) #this sets the random effects to 0
    
    #save coef stats
    int<-round(mod@beta[1], digits=2) #intercept
    est<-round(mod@beta[2], digits=2) #slope
    pVal.est<-round(summary(mod)$coefficients[2,'Pr(>|t|)'], digits=2) #pval for slope
    df.est<-round(summary(mod)$coefficients[2,'df'], digits=2) #df for slope
    #McFadden's pseudo-R-squared using loglik (aka likelihood ratio index)
    #see http://www.ats.ucla.edu/stat/mult_pkg/faq/general/Psuedo_RSquareds.htm
    ll<-logLik(mod)
    ll0<-logLik(mod0)
    pR2<-round(1-(ll/ll0), digits=2)
    result.coef<-data.frame(int, est, pVal.est, df.est, pR2)
    
#     #predict values using model fit
    df$pred<-predict(mod, re.form=NA) #this sets the random effects to 0
#     bb <- bootMer(mod,
#                   FUN=function(x)predict(x, re.form=NA),
#                   nsim=500)
#     df$ci.lb <- apply(bb$t, 2, quantile, 0.025)
#     df$ci.ub <- apply(bb$t, 2, quantile, 0.975)
    
    #1. save the model fit results
    list.results[[i]] <- result.fit
    list.results.coef[[i]] <- result.coef
    
    #2. save the dataset used for plotting
    list.data[[i]] <- df
    
    #make a plot panel of mv_I vs variable value
    p<-ggplot(df, aes(x=value, y=mv_g.m2_I_logt, color=year)) + 
      geom_point() + mytheme + 
      labs(x=NULL, y=NULL) + guides(color=FALSE) + ggtitle(varLevels[i])
    
    #add model fit if needed
    if(sum(result.fit$pvals.a<0.1)>0){
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
  names(list.results.coef)<-varLevels
  names(list.data)<-varLevels
  names(list.figures)<-varLevels
  
  #Save everything in a big list
  result.list<-list(results=list.results, coef= list.results.coef,
                    data=list.data,
                    figures=list.figures)
  
  return(result.list)
  
}

#FXN TO MAKE THE I~N PLOTS
FitPlot.IN<-function(dataset){
  list.figures<-list()
  varLevels<-levels(dataset$variable)
  i <- 0 #VARIABLES
  for (i in 1:length(varLevels)){
    
    #subset data by the current variable
    dataset.sub <- subset(dataset, variable == varLevels[i]) #subset data by variable
    df<-dataset.sub[!is.na(dataset.sub$I) & !is.na(dataset.sub$N),] #remove NAs
    
    #make a plot panel of y=I x=N with year in color
    p<-ggplot(df, aes(x=N, y=I, color=year)) + 
      geom_point() + mytheme + 
      labs(x=NULL, y=NULL) + guides(color=FALSE) + ggtitle(varLevels[i]) +
      geom_abline(intercept=0, slope=1, linetype=2) + coord_fixed()
    
    #save the plot panel
    list.figures[[i]]<-p
    
  }
  names(list.figures)<-varLevels
  
  return(list.figures)
}


