#script_q1A.R

##############
#A. Do an ordination of soil N pools and fluxes.
##############

#1) Subset and reshape
#0) subset and reshape
variables<-nVars #for QuickReshape1()
variableNames<-nVarNames #for QuickReshape1()
reshapeResult<-QuickReshape1(variables, variableNames, select.inv='both')
df.ord<-reshapeResult[['df.ord']]
data.vars.wide.rm<-reshapeResult[['data.vars.wide.rm']]
NonValCols<-reshapeResult[['NonValCols']]

#2) PCA-Ordinate and save plot --- plot and loadings were produced by script_visualize.R
df.ord.scaled<-scale(df.ord)
PCA.res<-prcomp(df.ord.scaled)
data.PCA<-data.frame(plotInvYear=rownames(PCA.res$x), PCA.res$x)
data.vars.PCA<-data.PCA[,c(1:3)]
data.vars.PCA<-merge(data.vars.PCA, data.vars.wide.rm)
screeplot(PCA.res, type='lines') #really should be using 3d

#3) Test the role of invasion status with permMANOVA
data.vars.wide.rm$inv<-factor(data.vars.wide.rm$inv, levels=c('N','I'))
sum(rownames(data.vars.wide.rm) != rownames(df.ord.scaled)) # if this is 0, then rownames match
permMANOVA<-adonis(df.ord.scaled ~ inv, data = data.vars.wide.rm, method='eu')
aov.tab.permmanova<-data.frame(permMANOVA$aov.tab)
aov.tab.permmanova
newfilename<-'permMANOVA_q1.txt'
write.table(aov.tab.permmanova, file=paste(figuresPath,newfilename, sep='/'), sep='\t')


