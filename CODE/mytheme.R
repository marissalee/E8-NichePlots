
# categorical variable orders

# inv_levels <- function(){
#   inv_order<-c("I","N")
#   invNames<-c("Invaded","Reference") 
#   
#   inv.levels <- list(inv_order = inv_order, invNames = invNames)
#   return(inv.levels)
# }

soiln_levels <- function(){
  basic.nVars<-c('nhi','noi','ammonifd','nitrifd','minzd') 
  nVars<-c(paste(basic.nVars, "T", sep="_"), paste(basic.nVars, "B", sep="_")) 
  basic.nVarNames<-c('Ammonium','Nitrate','Ammonif.','Nitrif.','Mineraliz.') 
  nVarNames<-c(paste(basic.nVarNames, "0-5cm", sep=" "), paste(basic.nVarNames, "5-15cm", sep=" ")) 
  
  basic.nVars.order<-c(1,2,3,4,5) 
  units<-c('(ug/G)','(ug/G)','(ug/G*d)','(ug/G*d)','(ug/G*d)') 
  depth<-c(rep("T", length(basic.nVars)),rep("B", length(basic.nVars))) 
  nVars.indx<-data.frame(nVars, nVarNames, basic.nVars, basic.nVarNames, basic.nVars.order, depth, units) 
  nVars.indx %>%
    arrange(basic.nVars.order) -> nVars.indx 
  nVars<-factor(nVars.indx$nVars, levels=nVars)
  nVarNames<-factor(nVars.indx$nVarNames, levels=nVarNames) 
  
  soiln.levels <- list(nVars = nVars, nVarNames = nVarNames, nVars.indx = nVars.indx, basic.nVars = basic.nVars, basic.nVarNames = basic.nVarNames, units = units)
  return(soiln.levels)
}

allVars_levels <- function(){
  soiln.levels <- soiln_levels()
  allVars<-c(as.character(soiln.levels$nVars), 
             'soilmoi_T', 'soilmoi_B','som_T','som_B','ph_T','ph_B',
             'nat_g.m2','litter_g.m2','percpar12',
             'nTrees','BA_total')
  allVarNames<-c(as.character(soiln.levels$nVarNames),
                 'Moisture (0-5cm)','Moisture (5-15cm)','Org. matter (0-5cm)','Org. matter (5-15cm)','pH (0-5cm)','pH (5-15cm)',
                 'Understory biomass','Litter biomass','Light avail.','Number of trees','Tree basal area')
  siteVars<-c('nTrees', 'BA_total')
  
  allVars.levels <- list(allVars = allVars, allVarNames = allVarNames, siteVars = siteVars)
  return(allVars.levels)
}

plotVars_levels <- function(){
  allVars.levels <- allVars_levels()
  plotVars <- allVars.levels$allVars[!allVars.levels$allVars %in% allVars.levels$siteVars]
  plotVarNames <- allVars.levels$allVarNames[!allVars.levels$allVars %in% allVars.levels$siteVars]
  
  plotVars.levels <- list(plotVars = plotVars, plotVarNames = plotVarNames)
  return(plotVars.levels)
}



# ggplot theme
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

# extract legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



