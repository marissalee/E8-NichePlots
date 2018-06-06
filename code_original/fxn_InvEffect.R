#fxn_InvEffect.R

InvEffect<-function(Yvars, YvarNames, df){
  
  #Yvars=variables
  #YvarNames=variableNames
  #df=data.vars.wide.rm
  
  df$inv<-factor(df$inv, levels = c('N','I'))
  
  summary.list<-list()
  posthoc.list<-list()
  anova.list<-list()
  figure.list<-list()
  i<-0
  for(i in 1:length(Yvars)){
    
    curr.Yvar<-as.character(Yvars[i])
    y<-df[,curr.Yvar]
    inv<-df$inv
    year<-df$year
    df.tmp<-data.frame(y,inv,year)
    
    #regression model
    mod<-lmer(y~inv + (1|year), data=df.tmp)
    summary.list[[i]]<-summary(mod)
    anova.list[[i]]<-anova(mod)
    
    #post-hoc tests
    posthoc.list[[i]]<-difflsmeans(mod)$diffs.lsmeans.table
    
    #plot
    df.tmp$year<-factor(df.tmp$year)
    p<-ggplot(df.tmp, aes(x=inv, y=y, color=year)) + geom_boxplot() + 
      mytheme + xlab(NULL) + ylab(NULL) + ggtitle(YvarNames[i]) + guides(color=FALSE)
    figure.list[[i]]<-p
  }
  names(posthoc.list)<-Yvars
  names(summary.list)<-Yvars
  names(anova.list)<-Yvars
  names(figure.list)<-Yvars
  
  #posthoc summary
  posthocSummary<-ldply(lapply(posthoc.list, data.frame))
  posthocSummary[,-1]<-round(posthocSummary[,-1], digits=4)
  colnames(posthocSummary)[1]<-'yVar'
  posthocSummary$terms<-unlist(lapply(posthoc.list, row.names))
  
  #summary summary
  summarySummary<-ldply(lapply(summary.list, 'coefficients'))
  summarySummary[,-1]<-round(summarySummary[,-1], digits=4)
  colnames(summarySummary)[1]<-'yVar'
  summarySummary$terms<-unlist(lapply(lapply(summary.list, 'coefficients'), row.names))
  
  #anova summary
  anovaSummary<-ldply(lapply(anova.list, data.frame))
  anovaSummary[,-1]<-round(anovaSummary[,-1], digits=4)
  colnames(anovaSummary)[1]<-'yVar'
  colnames(anovaSummary)[7]<-'pValue'
  anovaSummary$terms<-unlist(lapply(anova.list, row.names))
  
  results<-list(summarySummary=summarySummary, posthocSummary=posthocSummary,
                anovaSummary=anovaSummary, figures=figure.list)
  return(results)
}
