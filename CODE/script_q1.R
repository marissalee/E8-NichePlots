#script_q1.R

##############
#A. Do an ordination of soil N pools and fluxes.
##############

#1) Subset and reshape
data.vars<-data.choice[data.choice$variable %in% vars, c('plotid','plothalfid1','inv','year','variable','value')]
data.vars$variable<-factor(data.vars$variable, levels=vars)
data.vars.wide<-dcast(data.vars, plothalfid1 + plotid + inv+ year ~ variable, value.var='value')
data.vars.wide$plotInvYear<-paste(data.vars.wide$plothalfid1, data.vars.wide$year, sep="_") #use this to identify rows
data.vars.wide$plotYear<-paste(data.vars.wide$plotid, data.vars.wide$year, sep="_") #use this to block by plotid*year
row.names(data.vars.wide)<-data.vars.wide$plotInvYear
NonValCols<-c('plothalfid1','plotid','plotInvYear', 'plotYear','inv','year')
ValCols<-colnames(data.vars.wide)[!(colnames(data.vars.wide) %in% NonValCols)]
data.vars.wide.rm<-data.vars.wide[!rowSums(is.na(data.vars.wide))>0,] #remove rows with missing data
#cor(data.vars.wide.rm[,ValCols]) #look at correlation matrix

#2) PCA-Ordinate and save plot
df.ord<-data.vars.wide.rm[,ValCols]
colnames(df.ord)<-varNames
df.ord.scaled<-scale(df.ord)
PCA.res<-prcomp(df.ord.scaled)
data.PCA<-data.frame(plotInvYear=rownames(PCA.res$x), PCA.res$x)
data.vars.PCA<-merge(data.vars.wide.rm, data.PCA[,c(1:3)]) #add Mv biomass to this dataframe
data.vars.PCA$plotid<-ldply(strsplit(data.vars.PCA$plothalfid1, "_", fixed=TRUE))$V2
data.vars.PCA$year<-factor(data.vars.PCA$year)
plots<-ggbiplot_q1(pcobj=PCA.res, df=data.vars.PCA)
#plots[['pEllipseIN1']]
newfilename<-'pPCA_q1.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*2.5, height = fig.height*2, res=fig.res)
plots[['pEllipseIN1']] + xlim(c(-2.3,4))
dev.off()
#variable contributions to PC1 and PC2
PCvar<-PCA.var.contrib_q1(pca.obj=PCA.res)
newfilename<-'PCvar_q1.txt'
write.table(PCvar, file=paste(figuresPath,newfilename, sep='/'), sep='\t')

#3) Test the role of invasion status with permMANOVA
data.vars.wide.rm$inv<-factor(data.vars.wide.rm$inv, levels=c('N','I'))
sum(rownames(data.vars.wide.rm) != rownames(df.ord.scaled)) # if this is 0, then rownames match
permMANOVA<-adonis(df.ord.scaled ~ inv, data = data.vars.wide.rm, method='eu')
aov.tab.permmanova<-data.frame(permMANOVA$aov.tab)
aov.tab.permmanova
newfilename<-'permMANOVA_q1.txt'
write.table(aov.tab.permmanova, file=paste(figuresPath,newfilename, sep='/'), sep='\t')


##############
#B. Investigate individual relationships using mixed effects models with year+site as a random effect.
##############

#1) Run models
Yvars<-vars
YvarNames<-varNames
df<-data.vars.PCA
result<-InvEffect(Yvars=Yvars, YvarNames=YvarNames, df=df)

#2) Calculate means and save
data.vars.PCA.long<-melt(data.vars.PCA, measure.vars=vars, id.vars = c('inv','year','plotid'))
meanSummary<-ddply(data.vars.PCA.long, ~variable+inv, summarize,
                   mean=mean(value),
                   n=length(value),
                   se=sd(value)/sqrt(n))
meanSummary[,c('mean','se')]<-round(meanSummary[,c('mean','se')], digits=2)
colnames(meanSummary)[colnames(meanSummary) == 'variable']<-'vars'
newfilename<-'meanSummary_q1.txt'
write.table(meanSummary, file=paste(figuresPath,newfilename, sep='/'), sep='\t')

#3) Save coefficient and anova summary tables
data.frame(result[['summarySummary']])
newfilename<-'summarySummary_q1.txt'
write.table(data.frame(result[['summarySummary']]), file=paste(figuresPath,newfilename, sep='/'), sep='\t')
#data.frame(result[['posthocSummary']])
newfilename<-'posthocSummary_q1.txt'
write.table(data.frame(result[['posthocSummary']]), file=paste(figuresPath,newfilename, sep='/'), sep='\t')

#4) Plot
#i) prep dataframe and params
meanSummary.pretty<-merge(meanSummary, vars.indx)
meanSummary.pretty$varNames<-factor(meanSummary.pretty$varNames, levels=varNames)
limits <- aes(ymax = mean + se, ymin=mean - se)
meanSummary.pretty$depth<-factor(meanSummary.pretty$depth, levels=c('T','B'))
meanSummary.pretty$depth<-mapvalues(meanSummary.pretty$depth, 
                                    from = c('T','B'), to = c('0-5cm','5-15cm'))
meanSummary.pretty$inv<-mapvalues(meanSummary.pretty$inv, from = inv_order, to = invNames)
basic.varNames2<-paste(basic.varNames, units)
lineVec<-c(1,2)
#shapeVec<-c(16,17)
#ii) loop through each soil response variable
figure.list<-list()
i<-0
for(i in 1:length(basic.vars)){
  df.current<-meanSummary.pretty[meanSummary.pretty$basic.vars ==  basic.vars[i],]
  p<-ggplot(df.current, aes(x=inv, y=mean, linetype=depth, group=depth)) + 
    geom_point(size=3) + geom_errorbar(limits, width=0.2) + geom_line() + 
    mytheme + xlab(NULL) + ylab(basic.varNames2[i]) + 
    scale_linetype_manual(name='Depth', values=lineVec) + 
    guides(linetype=FALSE) 
  figure.list[[i]]<-p
}
names(figure.list)<-basic.vars
#iii) extract the legend
pleg <- ggplot(df.current, aes(x=inv, y=mean, linetype=depth, group=depth)) + 
  geom_point() + geom_errorbar(limits, width=0.2) + geom_line() + 
  mytheme + xlab(NULL) + ylab(basic.varNames2[i]) + 
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


