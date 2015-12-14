#my theme for E8 figures

####################
#SAVE FIGURES AND TABLES
####################
#FIGURES
figuresPath<-file.path(getwd()[1], "FIGURES_TABLES") #where to put the saved plots
fig.height<-2.5 #inches
fig.width<- 2.5 #inches
fig.res<-300

#SYNTHESIZED DATAFRAMES
synthdataPath<-file.path(getwd()[1], "DATA", "DATA_SYNTHESIZED") #where to put the clean dataframes


####################
#FACTOR NAMES
####################

#1) general
measCat_FULL_order<-c('nhi', 'noi', 'toti','ammonifd', 'nitrifd', 'minzd', 'soilmoi','som','ph') #used in script_cleanMerge.R
measCat_order<-c('nhi', 'noi', 'ammonifd', 'nitrifd', 'minzd', 'soilmoi','som','ph') #used in script_cleanMerge.R
depth_order<-c("T","B") #used in script_cleanMerge.R
inv_order<-c("I","N") #used in script_cleanMerge.R 
invNames<-c("Invaded","Reference") 

#2) soil N plot measurements
basic.nVars<-c('nhi','noi','ammonifd','nitrifd','minzd') 
nVars<-c(paste(basic.nVars, "T", sep="_"), paste(basic.nVars, "B", sep="_")) 
basic.nVarNames<-c('Ammonium','Nitrate','Ammonif.','Nitrif.','Mineraliz.') 
nVarNames<-c(paste(basic.nVarNames, "0-5cm", sep=" "), paste(basic.nVarNames, "5-15cm", sep=" ")) 
basic.nVars.order<-c(1,2,3,4,5) 
units<-c('(ug/G)','(ug/G)','(ug/G*d)','(ug/G*d)','(ug/G*d)') 
depth<-c(rep("T", length(basic.nVars)),rep("B", length(basic.nVars))) 
nVars.indx<-data.frame(nVars, nVarNames, basic.nVars, basic.nVarNames, basic.nVars.order, depth, units) 
nVars.indx<-orderBy(~basic.nVars.order, nVars.indx) 
nVars<-factor(nVars.indx$nVars, levels=nVars)
nVarNames<-factor(nVars.indx$nVarNames, levels=nVarNames) 


#3) year and depth
depthYearLevels<-c('T_2012','T_2013','B_2012','B_2013') #used in Q1
depthYearNames<-c('0-5cm, 2012','0-5cm, 2013','5-15cm, 2012','5-15cm, 2013') #used in Q1

#4) all plot and site measurements
allVars<-c(as.character(nVars), 
           'soilmoi_T', 'soilmoi_B','som_T','som_B','ph_T','ph_B',
           'nat_g.m2','litter_g.m2','percpar',
           'nTrees','BA_total','PercBA_AM')
allVarNames<-c(as.character(nVarNames),
               'Moisture (0-5cm)','Moisture (5-15cm)','Org. matter (0-5cm)','Org. matter (5-15cm)','pH (0-5cm)','pH (5-15cm)',
               'Understory biomass','Litter biomass','Light avail.','Number of trees','Tree basal area','%AM basal area')

#version with plot-aggregated light availability (because of missing data)
allVars.AggPAR<-c(as.character(nVars), 
                  'soilmoi_T', 'soilmoi_B','som_T','som_B','ph_T','ph_B',
                  'nat_g.m2','litter_g.m2','percpar.AggY',
                  'nTrees','BA_total','PercBA_AM')

#make a version of allVars that does not include %AM
allVars2<-allVars[allVars != 'PercBA_AM']
#make a version of allVars.AggPAR that does not include %AM
allVars2.AggPAR<-allVars.AggPAR[allVars.AggPAR != 'PercBA_AM']

#) plot and site measurements
siteVars<-c('nTrees', 'BA_total', 'PercBA_AM')
siteVarNames<-allVarNames[allVars %in% siteVars]
plotVars<-allVars[!allVars %in% siteVars]
plotVarNames<-allVarNames[!allVars %in% siteVars]
depth2<-c(rep(c("T","B"), (length(plotVars)-3)/2),rep("NA", 3))
plotVars.indx<-data.frame(vars=plotVars, 
                          plotVarNames=plotVarNames, 
                          depth=depth2)

#5) Microstegium biomass
mvVar<-c('mv_g.m2')


####################
#PLOT TEMPLATE
####################

mytheme <- theme_bw(base_size = 10, base_family = "Helvetica") +
  theme(panel.border = element_rect(colour = "black"),      #put a black box around the plotting area
        axis.line = element_line(colour = "black"),                 #axis lines are in black
        panel.grid.major = element_blank(),                         #turn off the gridlines
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(face='bold.italic', hjust=0.05),         #turn off the x axis facet labels
        strip.text.y = element_text(face='bold.italic', hjust=0.05),
        strip.background = element_rect(fill = 'white', colour='black'),    #make y axis facet labels be italic and top justified
        legend.key = element_blank(),                               #turn off box around legend
        plot.title=element_text(hjust=0, vjust=0.5, face='bold'), #style and position of the panel label
        plot.margin = unit(c(0.05,0.05,0.05,0.05),"in")
        )

#FXN TO EXTRACT LEGEND
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#figure theme
fill.vec<-c(1,1)
shape.vec<-c(16,17)
linetype.vec<-c(1,2)


