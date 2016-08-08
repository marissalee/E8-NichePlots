#script_visualize.R


### soil N pools and fluxes in invaded and reference plots ###
nVars
#0) subset and reshape
variables<-nVars #for QuickReshape1()
variableNames<-nVarNames #for QuickReshape1()
reshapeResult<-QuickReshape1(variables, variableNames, select.inv='both')
df.ord<-reshapeResult[['df.ord']]
data.vars.wide.rm<-reshapeResult[['data.vars.wide.rm']]
NonValCols<-reshapeResult[['NonValCols']]

#NMDS
df.ord.pos <- apply(df.ord, 2, minShift)
#2dplot
mod_NMDS<-metaMDS(df.ord.pos, distance='euclidean',
                  autotransform=FALSE, noshare=FALSE, wascores = TRUE,
                  k=2) # The number of reduced dimensions
nmdsResult<-NMDSplots(nmds=mod_NMDS, 
                      data.vars.wide.rm=data.vars.wide.rm, 
                      NonValCols=NonValCols)
#just points
nmdsResult[['p']] + xlim(-10,8) + ylim(-5,5) + coord_equal()
newfilename<-'NMDS_plotNvars_points.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*3, height = fig.height*3, res=fig.res)
nmdsResult[['p']] + xlim(-10,8) + ylim(-5,5) + coord_equal()
dev.off()
#site vectors
nmdsResult[['p.vecs']] + xlim(-10,8) + ylim(-5,5) + coord_equal()
newfilename<-'NMDS_plotNvars_vecs.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*3, height = fig.height*3, res=fig.res)
nmdsResult[['p.vecs']] + xlim(-10,8) + ylim(-5,5) + coord_equal()
dev.off()

#PCA
df.ord.scaled<-scale(df.ord)
PCA.res<-prcomp(df.ord.scaled)
data.PCA<-data.frame(plotInvYear=rownames(PCA.res$x), PCA.res$x)
data.vars.PCA<-data.PCA[,c(1:3)]
data.vars.PCA<-merge(data.vars.PCA, data.vars.wide.rm)
screeplot(PCA.res, type='lines') #really should be using 3d
# #3-d
# tmp<-data.frame(df.ord.scaled, inv=factor(data.vars.PCA$inv))
# plot(bpca(tmp[-11],method='hj',d=1:3),
#      rgl.use=TRUE,
#      var.col='brown',
#      var.factor=1,
#      var.cex=1,
#      obj.names=FALSE,
#      obj.cex=.8,
#      obj.col=c('gray', 'black')[unclass(tmp$inv)],
#      simple.axes=FALSE, box=TRUE)
# #this is not super informative
#2d
plots<-ggbiplot_q1(pcobj=PCA.res, df=data.vars.PCA)
#just points
plots[['pEllipseIN1']]
newfilename<-'PCA_plotNvars_points.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*2, height = fig.height*2, res=fig.res)
plots[['pEllipseIN1']] + xlim(c(-3,4)) + ylim(c(-4,2)) + coord_equal()
dev.off()
#site vectors
plots[['pEllipseIN2']]
newfilename<-'PCA_plotNvars_vecs.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*2, height = fig.height*2, res=fig.res)
plots[['pEllipseIN2']] + xlim(c(-3,4)) + ylim(c(-4,2)) + coord_equal()
dev.off()

#variable contributions to PC1 and PC2
PCvar<-PCA.var.contrib_q1(pca.obj=PCA.res)
PCvar
newfilename<-'PCvar_plotNvars.txt'
write.table(PCvar, file=paste(figuresPath,newfilename, sep='/'), sep='\t')






### everything measured in invaded and reference plots ###
plotVars
#0) subset and reshape
variables<-plotVars #for QuickReshape1()
variableNames<-plotVarNames #for QuickReshape1()
reshapeResult<-QuickReshape1(variables, variableNames,  select.inv='both')
df.ord<-reshapeResult[['df.ord']]
data.vars.wide.rm<-reshapeResult[['data.vars.wide.rm']]
NonValCols<-reshapeResult[['NonValCols']]

#NMDS
df.ord.pos <- apply(df.ord, 2, minShift)
mod_NMDS<-metaMDS(df.ord.pos, distance='euclidean',
                  autotransform=FALSE, noshare=FALSE, wascores = TRUE,
                  k=2) # The number of reduced dimensions
nmdsResult<-NMDSplots(nmds=mod_NMDS, data.vars.wide.rm=data.vars.wide.rm,
                      NonValCols=NonValCols)
#just points
nmdsResult[['p']] + xlim(c(-1900,1500)) + ylim(c(-500,500)) + coord_equal()
newfilename<-'NMDS_plotallVars_points.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*2, height = fig.height*2, res=fig.res)
nmdsResult[['p']] + xlim(c(-1900,1500)) + ylim(c(-500,500)) + coord_equal()
dev.off()
#site vectors
nmdsResult[['p.vecs']] + xlim(c(-1900,1500)) + ylim(c(-500,500)) + coord_equal()
newfilename<-'NMDS_plotallVars_vecs.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*2, height = fig.height*2, res=fig.res)
nmdsResult[['p.vecs']] + xlim(c(-1900,1500)) + ylim(c(-500,500)) + coord_equal()
dev.off()

#PCA
df.ord.scaled<-scale(df.ord)
PCA.res<-prcomp(df.ord.scaled)
data.PCA<-data.frame(plotInvYear=rownames(PCA.res$x), PCA.res$x)
data.vars.PCA<-data.PCA[,c(1:3)]
data.vars.PCA<-merge(data.vars.PCA, data.vars.wide.rm)
plots<-ggbiplot_q1(pcobj=PCA.res, df=data.vars.PCA)
#just points
plots[['pEllipseIN1']]
newfilename<-'PCA_plotallVars_points.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*2, height = fig.height*2, res=fig.res)
plots[['pEllipseIN1']] 
dev.off()
#site vectors
plots[['pEllipseIN2']]
newfilename<-'PCA_plotallVars_vecs.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*2, height = fig.height*2, res=fig.res)
plots[['pEllipseIN2']] 
dev.off()

#variable contributions to PC1 and PC2
PCvar<-PCA.var.contrib_q1(pca.obj=PCA.res)
PCvar
newfilename<-'PCvar_plotallVars.txt'
write.table(PCvar, file=paste(figuresPath,newfilename, sep='/'), sep='\t')


### everything measured in reference plots only ###
allVars
#0) subset and reshape
variables<-allVars #for QuickReshape1()
variableNames<-allVarNames #for QuickReshape1()
reshapeResult<-QuickReshape1(variables, variableNames, select.inv='reference')
df.ord<-reshapeResult[['df.ord']]
data.vars.wide.rm<-reshapeResult[['data.vars.wide.rm']]
NonValCols<-reshapeResult[['NonValCols']]

#NMDS
df.ord.pos <- apply(df.ord, 2, minShift)
nmds<-metaMDS(df.ord.pos, distance='euclidean',
              autotransform=FALSE, noshare=FALSE, wascores = TRUE,
              k=2) # The number of reduced dimensions
#extract scores
variableScores <- data.frame(id=rownames(nmds$species), nmds$species)
variableScores$angle <- with(variableScores, (180/pi) * atan(MDS2 / MDS1))
variableScores$hjust <- with(variableScores, (1 - 1.1 * sign(MDS1)) / 2)
sampleScores <- data.frame(id=row.names(nmds$points), data.vars.wide.rm[,NonValCols], nmds$points)
p<-ggplot(sampleScores, aes(x=MDS1, y=MDS2)) + 
  geom_point(color='darkgray') + 
  geom_segment(mapping=aes(x=0, y=0, xend=MDS1, yend=MDS2), 
               data=variableScores, arrow = arrow(length = unit(0.5, "cm")), color=1) +
  geom_text(mapping=aes(x=MDS1, y=MDS2, label=id, angle = angle, hjust = hjust), 
            data=variableScores, color=1, size=3) +
  mytheme
p + xlim(-2100,1300) + ylim(c(-500,500)) + coord_equal()
newfilename<-'NMDS_refVars_points.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*2, height = fig.height*2, res=fig.res)
p + xlim(-2100,1300) + ylim(c(-500,500)) + coord_equal()
dev.off()

#PCA
df.ord.scaled<-scale(df.ord)
PCA.res<-prcomp(df.ord.scaled)
data.PCA<-data.frame(plotInvYear=rownames(PCA.res$x), PCA.res$x)
data.vars.PCA<-data.PCA[,c(1:3)]
data.vars.PCA<-merge(data.vars.PCA, data.vars.wide.rm)
plots<-ggbiplot_custom(pcobj=PCA.res, df=data.vars.PCA)
plots + xlim(-2,3)
newfilename<-'PCA_plotrefVars_points.png'
png(paste(figuresPath,newfilename, sep='/'), units='in', width = fig.width*2.25, height = fig.height*2.25, res=fig.res)
plots + xlim(-2,3)
dev.off()

#variable contributions to PC1 and PC2
PCvar<-PCA.var.contrib_q1(pca.obj=PCA.res)
PCvar
newfilename<-'PCvar_plotrefVars.txt'
write.table(PCvar, file=paste(figuresPath,newfilename, sep='/'), sep='\t')

