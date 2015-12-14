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
  ord<-ordiellipse(nmds, sampleScores$inv, display = "sites", kind = "se", conf = 0.95, label = T)
  
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
    scale_linetype_manual(name='Plot type', values=c(1,2), labels=c('Invaded','Reference')) +
    annotate("text", x=5, y=3, label=stressCh, size=3)

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
    scale_linetype_manual(name='Plot type', values=c(1,2), labels=c('Invaded','Reference'))+
    annotate("text", x=5, y=3, label=stressCh, size=3)
  
  #save everything
  results<-list(p=p, p.vecs=p.vecs)
  
  return(results)
}
