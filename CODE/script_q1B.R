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
meanSummary<-ddply(data.vars.rm.long, ~variable+inv, summarize,
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
lineVec<-c(1,2)
#shapeVec<-c(16,17)
#ii) loop through each soil response variable
figure.list<-list()
i<-0
for(i in 1:length(basic.nVars)){
  df.current<-meanSummary.pretty[meanSummary.pretty$basic.nVars ==  basic.nVars[i],]
  p<-ggplot(df.current, aes(x=inv, y=mean, linetype=depth, group=depth)) + 
    geom_point(size=3) + geom_errorbar(limits, width=0.2) + geom_line() + 
    mytheme + xlab(NULL) + ylab(basic.nVarNames2[i]) + 
    scale_linetype_manual(name='Depth', values=lineVec) + 
    guides(linetype=FALSE) 
  figure.list[[i]]<-p
}
names(figure.list)<-basic.nVars
#iii) extract the legend
pleg <- ggplot(df.current, aes(x=inv, y=mean, linetype=depth, group=depth)) + 
  geom_point() + geom_errorbar(limits, width=0.2) + geom_line() + 
  mytheme + xlab(NULL) + ylab(basic.nVarNames2[i]) + 
  scale_linetype_manual(name='Depth', values=lineVec) + 
  theme(legend.key.width=unit(2.5, 'lines'))
leg1Grob<-g_legend(pleg)
#iv) put panels together and save
newfilename<-'pScat_q1.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*3, height = fig.height*2, res=fig.res)
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


