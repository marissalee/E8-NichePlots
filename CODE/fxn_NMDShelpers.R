#fxn_NMDShelpers.R

#minShift: used to make all the values positive for NMDS
minShift <- function(x) {x + abs(min(x))} 

#veganCovEllipse: used in NMDSplots
veganCovEllipse<-function (cov, center = c(0, 0), scale = 1, npoints = 100){
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}

#NMDS scree plot
#where x is the name of the data frame variable
NMDS.scree<-function(x, distance) { 
  plot(rep(1,10),
       replicate(10,metaMDS(x,autotransform=F,k=1, distance=distance)$stress/100),
       xlim=c(1,nrow(x)),ylim=c(0,0.5),
       xlab="# of Dimensions",ylab="Stress",
       main="NMDS stress plot")
  
  for (i in 1:(nrow(x)-2)) {
    points(rep(i+1,10),replicate(10,metaMDS(x,autotransform=F,k=i+1, distance=distance)$stress/100))
  }
}


#NMDSplots: used to make NMDS plots that indicate invaded vs reference group, with and without arrows for each site
NMDSplots<-function(nmds, data.vars.wide.rm ,NonValCols){
  
  #extract scores
  variableScores <- data.frame(id=rownames(nmds$species), nmds$species)
  variableScores$angle <- with(variableScores, (180/pi) * atan(MDS2 / MDS1))
  variableScores$hjust <- with(variableScores, (1 - 1.1 * sign(MDS1)) / 2)
  sampleScores <- data.frame(id=row.names(nmds$points), data.vars.wide.rm[,NonValCols], nmds$points)
  sampleScores.m<-melt(sampleScores, measure.vars=c('MDS1','MDS2'))
  sampleScores.c<-dcast(sampleScores.m, plotYear~inv+variable)
  
  #calculate group ellipses
  plot(nmds)
  ord<-ordiellipse(nmds, sampleScores$inv, display = "sites", kind = "sd", conf = 0.95, label = T)
  
  #set up dataframe
  df_ell <- data.frame()
  for(g in unique(sampleScores$inv)){
    df_ell <- rbind(df_ell, 
                    cbind(as.data.frame(
                      with(sampleScores[sampleScores$inv==g,],
                           veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,
                                           ord[[g]]$scale))),inv=g))
  }
  
  #center of ellipses
  NMDS.mean<-aggregate(sampleScores[,c('MDS1', 'MDS2')],list(inv=sampleScores$inv),mean)
  
  #stress
  stressVal<-round(nmds$stress, digits=3)
  stressCh<-paste('Stress =',stressVal)
  
  #make plots
  #without vectors for sites
  p<-ggplot(sampleScores, aes(x=MDS1, y=MDS2)) + 
    geom_point(aes(shape=inv), color='darkgray') + 
    geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2, linetype=inv), size=0.5, color='blue') +
    geom_segment(mapping=aes(x=0, y=0, xend=MDS1, yend=MDS2), 
                 data=variableScores, arrow = arrow(length = unit(0.5, "cm")), color=1) +
    geom_text(mapping=aes(x=MDS1, y=MDS2, label=id, angle = angle, hjust = hjust), 
              data=variableScores, color=1, size=3) +
    mytheme + 
    scale_shape_manual(name='Plot type', values=c(16,1), labels=c('Invaded','Reference'))+
    scale_linetype_manual(name='Plot type', values=c(3,1), labels=c('Invaded','Reference'))

  #with vectors for sites
  p.vecs<-ggplot(sampleScores, aes(x=MDS1, y=MDS2)) + 
    geom_segment(mapping=aes(x=N_MDS1, y=N_MDS2, xend=I_MDS1, yend=I_MDS2), 
                 data=sampleScores.c, arrow = arrow(length = unit(0.25, "cm")), color='darkgray') +
    geom_path(data=df_ell, aes(x=NMDS1, y=NMDS2, linetype=inv), size=0.5, color='blue') +
    geom_segment(mapping=aes(x=0, y=0, xend=MDS1, yend=MDS2), 
                 data=variableScores, arrow = arrow(length = unit(0.5, "cm")), color=1) +
    geom_text(mapping=aes(x=MDS1, y=MDS2, label=id, angle = angle, hjust = hjust), 
              data=variableScores, color=1, size=3) + 
    mytheme +
    scale_shape_manual(name='Plot type', values=c(16,1), labels=c('Invaded','Reference'))+
    scale_linetype_manual(name='Plot type', values=c(3,1), labels=c('Invaded','Reference'))
  
  #save everything
  results<-list(p=p, p.vecs=p.vecs, stressVal=stressVal)
  
  return(results)
}


#NMDSplots.3d: used to make NMDS plots that indicate invaded vs reference group and segments for each site
NMDSplots.3d<-function(nmds, data.vars.wide.rm, NonValCols){
  
  #extract variable and sample scores
  variableScores <- data.frame(id=rownames(nmds$species), nmds$species)
  variableScores$angle <- with(variableScores, (180/pi) * atan(MDS2 / MDS1))
  variableScores$hjust <- with(variableScores, (1 - 1.1 * sign(MDS1)) / 2)
  variableScores$startVecs<-rep(0,dim(variableScores)[1])
  sampleScores <- data.frame(id=row.names(nmds$points), data.vars.wide.rm[,NonValCols], nmds$points)
  sampleScores.m<-melt(sampleScores, measure.vars=c('MDS1','MDS2','MDS3'))
  sampleScores.c<-dcast(sampleScores.m, plotYear~inv+variable)
  
  #start plot (opens new window)
  require(scatterplot3d)
  plot3d(x=sampleScores$MDS1, y=sampleScores$MDS2, z=sampleScores$MDS3, 
         xlab='x', ylab='y', zlab='z', type='n')
  #add invaded and reference points
  points3d(x=sampleScores[sampleScores$inv=='I','MDS1'], 
           y=sampleScores[sampleScores$inv=='I','MDS2'],
           z=sampleScores[sampleScores$inv=='I','MDS3'], color='darkgray') 
  points3d(x=sampleScores[sampleScores$inv=='N','MDS1'], 
           y=sampleScores[sampleScores$inv=='N','MDS2'],
           z=sampleScores[sampleScores$inv=='N','MDS3'], color=1) 
  #add segments that connect invaded and reference plots in the same site
  segments3d(x=as.vector(t(sampleScores.c[,c('N_MDS1','I_MDS1')])),
             y=as.vector(t(sampleScores.c[,c('N_MDS2','I_MDS2')])),
             z=as.vector(t(sampleScores.c[,c('N_MDS3','I_MDS3')])), color='gray')
  #add variable vectors
  segments3d(x=as.vector(t(variableScores[,c('startVecs','MDS1')])),
             y=as.vector(t(variableScores[,c('startVecs','MDS2')])),
             z=as.vector(t(variableScores[,c('startVecs','MDS3')])), color=2)
  #add variable vector labels
  text3d(x=variableScores$MDS1, 
         y=variableScores$MDS2, 
         z=variableScores$MDS3, variableScores$id, color=2) 
}



