# factor names

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
           'nat_g.m2','litter_g.m2','percpar.AggY',
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
#add more info about variables by depth
basic.vars<-c(basic.nVars, 'soilmoi','som','ph')
vars<-c(paste(basic.vars, "T", sep="_"), paste(basic.vars, "B", sep="_")) 
basic.varNames<-c(basic.nVarNames,'Moisture','Org. matter','pH') 
varNames<-c(paste(basic.varNames, "0-5cm", sep=" "), paste(basic.varNames, "5-15cm", sep=" ")) 
basic.vars.order<-c(1,2,3,4,5,6,7,8) 
var.units<-c(units, c('(%)','(%)',''))
var.depth<-c(rep("T", length(basic.vars)),rep("B", length(basic.vars))) 
vars.indx<-data.frame(vars, varNames, basic.vars, basic.varNames, basic.vars.order, var.depth, var.units) 
vars.indx<-orderBy(~basic.vars.order, vars.indx) 
#merge
plotVars.indx<-merge(plotVars.indx,vars.indx, all.x=TRUE)
#fill in data for non-depth measurements
plotVars.indx[is.na(plotVars.indx$basic.vars.order),'basic.vars.order']<-c(9,10,11)
plotVars.indx$basic.varNames<-as.character(plotVars.indx$basic.varNames)
plotVars.indx[is.na(plotVars.indx$var.depth),'basic.varNames']<-as.character(plotVars.indx[is.na(plotVars.indx$var.depth),'plotVarNames'])
plotVars.indx$var.units<-as.character(plotVars.indx$var.units)
plotVars.indx[is.na(plotVars.indx$var.depth),'var.units']<-c('(g/m2)','(g/m2)','(%)')
plotVars.indx$basic.varNames.units<-paste(plotVars.indx$basic.varNames, plotVars.indx$var.units, sep=" ")
plotVars.indx$var.depth<-factor(plotVars.indx$var.depth, levels=depth_order)
plotVars.indx<-orderBy(~basic.vars.order+var.depth, plotVars.indx)