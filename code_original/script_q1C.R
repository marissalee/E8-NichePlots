#script_q1C.R

##############
#C. For supplement - show how invaded and native plots differ based on...soil moisture, org. matter, ph, litter biomass, understory biom, and light availability

#0) Subset and reshape
someVars<-plotVars[!plotVars %in% nVars]
someVarNames<-plotVarNames[!plotVarNames %in% nVarNames]
variables<-someVars #for QuickReshape1()
variableNames<-someVarNames #for QuickReshape1()
reshapeResult<-QuickReshape1(variables, variableNames, select.inv='both')
df.ord<-reshapeResult[['df.ord']]
data.vars.wide.rm<-reshapeResult[['data.vars.wide.rm']]
NonValCols<-reshapeResult[['NonValCols']]

#1) Run models
result<-InvEffect(Yvars=variables, YvarNames=variableNames, df=data.vars.wide.rm)

#2) Calculate means and save
data.vars.rm.long<-melt(data.vars.wide.rm, measure.vars=someVars, id.vars = c('inv','year','plotid'))
meanSummary<-ddply(data.vars.rm.long, ~variable+inv+year, summarize,
                   mean=mean(value),
                   n=length(value),
                   se=sd(value)/sqrt(n))
meanSummary[,c('mean','se')]<-round(meanSummary[,c('mean','se')], digits=2)
colnames(meanSummary)[colnames(meanSummary) == 'variable']<-'vars'
newfilename<-'meanSummary_q1_suppl.txt'
write.table(meanSummary, file=paste(figuresPath,newfilename, sep='/'), sep='\t')

#3) Save coefficient and anova summary tables
newfilename<-'summarySummary_q1_suppl.txt'
write.table(data.frame(result[['summarySummary']]), file=paste(figuresPath,newfilename, sep='/'), sep='\t')
#data.frame(result[['posthocSummary']])
newfilename<-'posthocSummary_q1_suppl.txt'
write.table(data.frame(result[['posthocSummary']]), file=paste(figuresPath,newfilename, sep='/'), sep='\t')

#4) Plot
#i) prep dataframe and params
meanSummary.pretty<-merge(meanSummary, plotVars.indx)
limits <- aes(ymax = mean + se, ymin=mean - se)
meanSummary.pretty$depth<-factor(meanSummary.pretty$depth, levels=c('T','B'))
meanSummary.pretty$depth<-mapvalues(meanSummary.pretty$depth, 
                                    from = c('T','B'), to = c('0-5cm','5-15cm'))
meanSummary.pretty$inv<-mapvalues(meanSummary.pretty$inv, from = inv_order, to = invNames)
meanSummary.pretty$year<-factor(meanSummary.pretty$year)
meanSummary.pretty$group<-paste(meanSummary.pretty$year, meanSummary.pretty$depth, sep=", ")

#ii) loop through each soil response variable
shapeVec<-c(16,1,16,1)
linetypeVec<-c(1,2,1,2)
colorVec<-c("black","black","darkgray","darkgray")
meanSummary.pretty.soil<-subset(meanSummary.pretty, !is.na(basic.vars))
SoilVAR<-c('soilmoi','som','ph')
figure.list<-list()
i<-0
for(i in 1:length(SoilVAR)){
  df.current<-meanSummary.pretty.soil[meanSummary.pretty.soil$basic.vars ==  SoilVAR[i],]
  ylabName<-unique(df.current$basic.varNames.units)
  p<-ggplot(df.current, aes(x=inv, y=mean,
                            shape=group, color=group, linetype=group, group=group)) + 
    geom_point(size=2) + geom_line() + 
    geom_errorbar(limits, width=0.2, linetype=1) + 
    mytheme + xlab(NULL) + ylab(ylabName) +
    scale_shape_manual(name='Year and depth', values=shapeVec) + 
    scale_color_manual(name='Year and depth', values=colorVec) +
    scale_linetype_manual(name='Year and depth', values=linetypeVec) +
    guides(shape=FALSE, color=FALSE, linetype=FALSE)
  figure.list[[i]]<-p
}
names(figure.list)<-SoilVAR


#iii) extract the legend
pleg <- ggplot(df.current, aes(x=inv, y=mean,
                               shape=group, color=group, linetype=group, group=group)) + 
  geom_point() + geom_line() + 
  geom_errorbar(limits, width=0.2) + 
  mytheme + xlab(NULL) + ylab('') +
  scale_shape_manual(name='Year and depth', values=shapeVec) + 
  scale_color_manual(name='Year and depth', values=colorVec) +
  scale_linetype_manual(name='Year and depth', values=linetypeVec) +
  theme(legend.key.width=unit(2, 'lines'), legend.title=element_text(size=8))
leg1Grob<-g_legend(pleg)

#iv) make the panels that don't have different soil depths
shapeVec2<-c(16,16)
linetypeVec2<-c(1,1)
colorVec2<-c("black","darkgray")
meanSummary.pretty.other<-subset(meanSummary.pretty, is.na(basic.vars))
otherVAR<-c('litter_g.m2','nat_g.m2','percpar')
figure.list2<-list()
i<-0
for(i in 1:length(otherVAR)){
  df.current<-meanSummary.pretty.other[meanSummary.pretty.other$vars ==  otherVAR[i],]
  ylabName<-unique(df.current$basic.varNames.units)
  p<-ggplot(df.current, aes(x=inv, y=mean,
                            shape=group, color=group, linetype=group, group=group)) + 
    geom_point(size=2) + geom_errorbar(limits, width=0.2) +
    mytheme +  geom_line() + xlab(NULL) + ylab(ylabName)+
    scale_shape_manual(name='Year and depth', values=shapeVec2) + 
    scale_color_manual(name='Year and depth', values=colorVec2) +
    scale_linetype_manual(name='Year and depth', values=linetypeVec2)+
    guides(shape=FALSE, color=FALSE, linetype=FALSE)
  figure.list2[[i]]<-p
}
names(figure.list2)<-otherVAR


#iv) put panels together and save
newfilename<-'pScat_q1_supp.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*2.5, height = fig.height*1.5, res=fig.res)
grid.arrange(
  figure.list[['soilmoi']],
  figure.list[['som']],
  figure.list[['ph']],
  figure.list2[['litter_g.m2']],
  figure.list2[['nat_g.m2']],
  figure.list2[['percpar']],
  nrow=2, ncol=3
)

dev.off()


