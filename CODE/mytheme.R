#my theme for ggplots in MIIN

require(ggplot2)
require(grid)

#Factor levels
measCat_order<-c('nhi', 'noi', 
                 'ammonifd', 'nitrifd', 'minzd',
                 'soilmoi','som')
depth_order<-c("T","B")
inv_order<-c("I","N")
measCat_F<-paste(measCat_order,"F", sep="_")
measCat_B<-paste(measCat_order,"B", sep="_")
measCat_T<-paste(measCat_order,"T", sep="_")
otherPH_vars<-c('mv_g.m2','nat_g.m2','litter_g.m2','total_g.m2','percpar','percparI','percparPlot')
P_vars<-c('nTrees','BA_total','PercBA_AM')

#Factor labels
prettyColNames<-c('NH4+','NO3-','Ammonif.','Nitrif.','Mineraliz.',
                  'Soil moisture','Soil organic matter',
                  '%PAR',
                  'Plant biomass','Litter biomass',
                  'Number of trees','Basal area','%AM basal area')
measCat_names<-c("Ammonium (ug/G)","Nitrate (ug/G)",
               "Ammonification (ug/G*d)","Nitrification (ug/G*d)","Mineralization (ug/G*d)",
               "Soil Moisture (%)","Soil organic matter (%)") 
otherPH_names<-c('Microstegium biomass','Non-Mv understory biomass','Litter biomass', 
                 'Total understory biomass','%PAR','%PAR invaded area','%PAR plot')
P_names<-c('Number of trees in plot',
           'Tree basal area in plot','%AM-associated trees in plot')

#my ggplot template
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
  
#ggplot keys
yearLine<-c(1,2,1)
yearShape<-c(1,2,3)

#fxn to extract legend from a plot
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

