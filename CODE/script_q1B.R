#script_q1B.R

##############
#B. Investigate individual relationships using mixed effects models with year+site as a random effect.
##############

#0) Subset and reshape
variables<-nVars #for QuickReshape1()
variableNames<-nVarNames #for QuickReshape1()
reshapeResult<-QuickReshape1(variables, variableNames, select.inv='both')
df.ord<-reshapeResult[['df.ord']]
data.vars.wide.rm<-reshapeResult[['data.vars.wide.rm']]
NonValCols<-reshapeResult[['NonValCols']]

#1) Run models
result<-InvEffect(Yvars=variables, YvarNames=variableNames, df=data.vars.wide.rm)

#2) Calculate means and save
data.vars.rm.long<-melt(data.vars.wide.rm, measure.vars=nVars, id.vars = c('inv','year','plotid'))
meanSummary<-ddply(data.vars.rm.long, ~variable+inv+year, summarize,
                   mean=mean(value),
                   n=length(value),
                   se=sd(value)/sqrt(n))

meanSummary[,c('mean','se')]<-round(meanSummary[,c('mean','se')], digits=2)
colnames(meanSummary)[colnames(meanSummary) == 'variable']<-'nVars'
newfilename<-'meanSummary_q1.txt'
write.table(meanSummary, file=paste(figuresPath,newfilename, sep='/'), sep='\t')

#3) Save coefficient and anova summary tables
newfilename<-'summarySummary_q1.txt'
write.table(data.frame(result[['summarySummary']]), file=paste(figuresPath,newfilename, sep='/'), sep='\t')
#data.frame(result[['posthocSummary']])
newfilename<-'posthocSummary_q1.txt'
write.table(data.frame(result[['posthocSummary']]), file=paste(figuresPath,newfilename, sep='/'), sep='\t')

#4) Plot
#i) prep dataframe and params
meanSummary.pretty<-merge(meanSummary, nVars.indx)
meanSummary.pretty$nVarNames<-factor(meanSummary.pretty$nVarNames, levels=nVarNames)
limits <- aes(ymax = mean + se, ymin=mean - se)
meanSummary.pretty$depth<-factor(meanSummary.pretty$depth, levels=c('T','B'))
meanSummary.pretty$depth<-mapvalues(meanSummary.pretty$depth, 
                                    from = c('T','B'), to = c('0-5cm','5-15cm'))
meanSummary.pretty$inv<-mapvalues(meanSummary.pretty$inv, from = inv_order, to = invNames)
basic.nVarNames2<-paste(basic.nVarNames, units)
shapeVec<-c(16,1,16,1)
linetypeVec<-c(1,2,1,2)
colorVec<-c("black","black","darkgray","darkgray")
#ii) loop through each soil response variable
figure.list<-list()
i<-0
for(i in 1:length(basic.nVars)){
  df.current<-meanSummary.pretty[meanSummary.pretty$basic.nVars ==  basic.nVars[i],]
  df.current$year<-factor(df.current$year)
  df.current$group<-paste(df.current$year, df.current$depth, sep=", ")
  p<-ggplot(df.current, aes(x=inv, y=mean, 
                            shape=group, color=group, linetype=group,
                            group=group)) + 
    geom_point(size=2) + geom_errorbar(limits, width=0.2) + geom_line() + 
    mytheme + xlab(NULL) + ylab(basic.nVarNames2[i]) + 
    scale_shape_manual(name='Year and depth', values=shapeVec) + 
    scale_color_manual(name='Year and depth', values=colorVec) +
    scale_linetype_manual(name='Year and depth', values=linetypeVec) +
    guides(shape=FALSE, color=FALSE, linetype=FALSE)
  figure.list[[i]]<-p
}
names(figure.list)<-basic.nVars

#iii) extract the legend
pleg <- ggplot(df.current, aes(x=inv, y=mean, 
                          shape=group, color=group, linetype=group,
                          group=group)) + 
  geom_point(size=2) + geom_errorbar(limits, width=0.2) + geom_line() + 
  mytheme + xlab(NULL) + ylab(basic.nVarNames2[i]) + 
  scale_shape_manual(name='Year and depth', values=shapeVec) + 
  scale_color_manual(name='Year and depth', values=colorVec) +
  scale_linetype_manual(name='Year and depth', values=linetypeVec) +
  theme(legend.key.width=unit(2, 'lines'), legend.title=element_text(size=8))
leg1Grob<-g_legend(pleg)

#iv) put panels together and save
newfilename<-'pScat_q1.pdf'
pdf(paste(figuresPath,newfilename, sep='/'), width = fig.width*2, height = fig.height*1.2)
grid.arrange(
  figure.list[['nhi']] + ylim(c(0,4)),
  figure.list[['noi']] + ylim(c(0,4)),
  leg1Grob,
  figure.list[['ammonifd']] + ylim(c(-0.2,0.55)),
  figure.list[['nitrifd']] + ylim(c(-0.2,0.55)),
  figure.list[['minzd']] + ylim(c(-0.2,0.55)),
  nrow=2, ncol=3
)
dev.off()


