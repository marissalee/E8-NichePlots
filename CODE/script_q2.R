#script_q2.R


##############
#1. CALCULATE REF ORDINATION, MAKE DATA FRAME, FIX MIXED EFFECTS MODELS FOR 0-5CM AND 5-15CM
##############

#A. Nitrate (Inv-Ref)
diffVar.basic<-'noi'
result<-RefPCA(diffVar.basic=diffVar.basic, refVars=useThese_refVars, df=data.q2, externalPercBA_AM=externalPercBA_AM) # calculate reference condition ordination, make dataframe with PC1, PC2, Mv biomass, impact measure(0-5cm), impact measure (5-15cm), year
pPCA.noi<-result[['pPCA.ref']] #save ref ordination plot
PCvar.noi<-PCA.var.contrib_q2(result.list=result, diffVar.basic=diffVar.basic) #save variable contributions to PCA
dfmod.noi<-result[['df.mod']] #save dataframe
dfmod.noi$year<-as.factor(dfmod.noi$year)
#fit mixed effect models for 0-5cm and 5-15cm
mod.noi<-ScaleFit_q2(diffVar.basic=diffVar.basic, df.mod=dfmod.noi, modelFormulaCode=modelFormulaCode, externalPercBA_AM=externalPercBA_AM) 

#B. Ammonification (Inv-Ref)
diffVar.basic<-'ammonifd'
result<-RefPCA(diffVar.basic=diffVar.basic, refVars=useThese_refVars, df=data.q2, externalPercBA_AM=externalPercBA_AM) # calculate reference condition ordination, make dataframe with PC1, PC2, Mv biomass, impact measure(0-5cm), impact measure (5-15cm), year
pPCA.ammonifd<-result[['pPCA.ref']] #save ref ordination plot
PCvar.ammonifd<-PCA.var.contrib_q2(result.list=result, diffVar.basic=diffVar.basic) #save variable contributions to PCA
dfmod.ammonifd<-result[['df.mod']] #save dataframe
dfmod.ammonifd$year<-as.factor(dfmod.ammonifd$year)
#fit mixed effect models for 0-5cm and 5-15cm
mod.ammonifd<-ScaleFit_q2(diffVar.basic=diffVar.basic, df.mod=dfmod.ammonifd, modelFormulaCode=modelFormulaCode, externalPercBA_AM=externalPercBA_AM) 

#C. Nitrification (Inv-Ref)
diffVar.basic<-'nitrifd'
result<-RefPCA(diffVar.basic=diffVar.basic, refVars=useThese_refVars, df=data.q2, externalPercBA_AM=externalPercBA_AM) # calculate reference condition ordination, make dataframe with PC1, PC2, Mv biomass, impact measure(0-5cm), impact measure (5-15cm), year
pPCA.nitrifd<-result[['pPCA.ref']] #save ref ordination plot
PCvar.nitrifd<-PCA.var.contrib_q2(result.list=result, diffVar.basic=diffVar.basic) #save variable contributions to PCA
dfmod.nitrifd<-result[['df.mod']] #save dataframe
dfmod.nitrifd$year<-as.factor(dfmod.nitrifd$year)
#fit mixed effect models for 0-5cm and 5-15cm
mod.nitrifd<-ScaleFit_q2(diffVar.basic=diffVar.basic, df.mod=dfmod.nitrifd, modelFormulaCode=modelFormulaCode, externalPercBA_AM=externalPercBA_AM) 


#D. Mineralization (Inv-Ref)
diffVar.basic<-'minzd'
result<-RefPCA(diffVar.basic=diffVar.basic, refVars=useThese_refVars, df=data.q2, externalPercBA_AM=externalPercBA_AM) # calculate reference condition ordination, make dataframe with PC1, PC2, Mv biomass, impact measure(0-5cm), impact measure (5-15cm), year
pPCA.minzd<-result[['pPCA.ref']] #save ref ordination plot
PCvar.minzd<-PCA.var.contrib_q2(result.list=result, diffVar.basic=diffVar.basic) #save variable contributions to PCA
dfmod.minzd<-result[['df.mod']] #save dataframe
dfmod.minzd$year<-as.factor(dfmod.minzd$year)
#fit mixed effect models for 0-5cm and 5-15cm
mod.minzd<-ScaleFit_q2(diffVar.basic=diffVar.basic, df.mod=dfmod.minzd, modelFormulaCode=modelFormulaCode, externalPercBA_AM=externalPercBA_AM) 
mod.minzd



##############
#2. SAVE PCA PLOTS AND LOADINGS
##############

#Ordination plots
newfilename<-'pPCAref_q2.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*4, height = fig.height*4, res=fig.res)
grid.arrange(pPCA.noi + guides(color=FALSE) + ggtitle('Nitrate (Inv.-Ref.)'), 
             pPCA.ammonifd + guides(color=FALSE) + ggtitle('Ammonif. (Inv.-Ref.)'),
             pPCA.nitrifd + guides(color=FALSE) + ggtitle('Nitrif. (Inv.-Ref.)'),
             pPCA.minzd + guides(color=FALSE) + ggtitle('Mineraliz. (Inv.-Ref.)'))
dev.off()

#Loadings
PCvar<-rbind(PCvar.noi, PCvar.ammonifd, PCvar.nitrifd, PCvar.minzd)
#PC1
PC1var<-PCvar[PCvar$PC=='PC1',]
PC1var.c<-dcast(PC1var, diffVar+PC+var ~ valueType, value.var = 'value')
PC1var.c.o<-orderBy(~diffVar-contrib, PC1var.c)
newfilename<-'PC1var_q2.txt'
write.table(PC1var.c.o, file=paste(figuresPath,newfilename, sep='/'), sep='\t')
#PC2
PC2var<-PCvar[PCvar$PC=='PC2',]
PC2var.c<-dcast(PC2var, diffVar+PC+var ~ valueType, value.var = 'value')
PC2var.c.o<-orderBy(~diffVar-contrib, PC2var.c)
newfilename<-'PC2var_q2.txt'
write.table(PC2var.c.o, file=paste(figuresPath,newfilename, sep='/'), sep='\t')



##############
#3. SAVE MODEL SUMMARY TABLES
##############
mod.list<-list(noi=mod.noi[['summ']], ammonifd=mod.ammonifd[['summ']], nitrifd=mod.nitrifd[['summ']], minzd=mod.minzd[['summ']])
mod.df<-ldply(mod.list)

#model summary tables
newfilename<-'summaryTable_mod_Diffs.txt'
write.table(mod.df, file=paste(figuresPath,newfilename, sep='/'), sep='\t')


