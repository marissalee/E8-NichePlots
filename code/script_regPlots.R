
#load libraries
library("tidyverse") # ggplot2, tidyr, readr, dplyr
library("grid") # grid.arrange()
library("gridExtra") #grid.arrange()
library("lme4") #for mixed-effects models
library("lmerTest") #for lmer p-values
library("merTools")

#load fxns
source('code/fxns_predictHelpers.R')
source('code/fxns_helpers.R')
source('code/mytheme.R')

#plotting params
shapeVec<-c(16,17)
linetypeVec<-c(1,2)
fillVec<-c(1,1)

#-----------------------------------------#
# Mv biomass

plot.list <- readRDS(file = paste0("data_synth/","mv.RData"))
df <- plot.list$df
mod <- plot.list$mod

# x = percpar
df.pred <- XYPlot_DF(n = 100, 
                     xVar = "percpar12.logt", 
                     constantVar = c("BA_total"), 
                     df = df, mod = mod)

p.percpar<- ggplot(data=df.pred, 
               mapping=aes(x=percpar12.logt, y=ypred, ymin=CI.lwr, ymax=CI.upr, 
                           shape=factor(year), linetype=factor(year), fill=factor(year)))  +
  geom_ribbon(alpha=0.5) +
  geom_line() +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') +
  #geom_line(data=df, mapping=aes(x=percpar12.logt, y=mv.logt,
  #                               group=factor(plotid)), inherit.aes = F, color = "gray") +
  geom_point(data=df, aes(y=mv.logt, ymin=NULL, ymax=NULL)) +
  scale_shape_manual(name = "Year", values=shapeVec) +
  scale_linetype_manual(name = "Year", values=linetypeVec) +
  scale_fill_manual(name = "Year", values=fillVec) + mytheme +
  ylab("M.v. biomass (log g/m2)") + 
  xlab("Light avail. (log %)")
p.percpar
  

# x = BA_total
df.pred <- XYPlot_DF(n = 100, 
                     xVar = "BA_total", 
                     constantVar = c("percpar12.logt"), 
                     df = df, mod = mod)
p.treeba<- ggplot(data=df.pred, 
                   mapping=aes(x=BA_total, y=ypred, ymin=CI.lwr, ymax=CI.upr, 
                               shape=factor(year), linetype=factor(year), fill=factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') +
  #geom_line(data=df, mapping=aes(x=BA_total, y=mv.logt,
  #                               group=factor(plotid)), inherit.aes = F, color = "gray") +
  geom_point(data=df, aes(y=mv.logt, ymin=NULL, ymax=NULL)) +
  scale_shape_manual(name = "Year", values=shapeVec) +
  scale_linetype_manual(name = "Year", values=linetypeVec) +
  scale_fill_manual(name = "Year", values=fillVec) + mytheme +
  ylab("M.v. biomass (log g/m2)") + 
  xlab("Tree basal area (m2)")
p.treeba

leg <- g_legend(p.treeba)

# grid.arrange(p.percpar + 
#                guides(shape = F, fill = F, linetype = F) +
#                ggtitle("a"),
#              p.treeba + 
#                guides(shape = F, fill = F, linetype = F) +
#                ggtitle("b"),
#              leg,
#              ncol = 2)

pdf(file = paste0('output/','plots_mv.pdf'), width = 3, height = 3)
p.percpar + 
  guides(shape = F, fill = F, linetype = F)
dev.off()


#-----------------------------------------#
# Mineralization T

plot.list <- readRDS(file = paste0("data_synth/","minzdT.RData"))
df <- plot.list$df
mod <- plot.list$mod

# x = noi
df.pred <- XYPlot_DF(n = 100, 
                     xVar = "noi_T", 
                     constantVar = c("soilmoi_T","mv.logt"), 
                     df = df, mod = mod)
p.noi<- ggplot(data=df.pred, 
               mapping=aes(x=noi_T, y=ypred, ymin=CI.lwr, ymax=CI.upr, 
                           shape=factor(year), linetype=factor(year), fill=factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') +
  #geom_line(data=df, mapping=aes(x=noi_T, y=minzd_T_Diff,
  #                               group=factor(plotid)), inherit.aes = F, color = "gray") +
  geom_point(data=df, aes(y=minzd_T_Diff, ymin=NULL, ymax=NULL)) +
  scale_shape_manual(name = "Year", values=shapeVec) +
  scale_linetype_manual(name = "Year", values=linetypeVec) +
  scale_fill_manual(name = "Year", values=fillVec) + mytheme +
  ylab(expression(paste(Delta," Mineraliz. 0-5cm (Inv.-Ref.)"))) + 
  xlab('Soil nitrate (ugN/G)')
p.noi

# x = soilmoi
df.pred <- XYPlot_DF(n = 100, 
                     xVar = "soilmoi_T", 
                     constantVar = c("noi_T","mv.logt"), 
                     df = df, mod = mod)
p.soilmoi<- ggplot(data=df.pred, 
                   mapping=aes(x=soilmoi_T, y=ypred, ymin=CI.lwr, ymax=CI.upr, 
                               shape=factor(year), linetype=factor(year), fill=factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') +
  #geom_line(data=df, mapping=aes(x=soilmoi_T, y=minzd_T_Diff,
  #                               group=factor(plotid)), inherit.aes = F, color = "gray") +
  geom_point(data=df, aes(y=minzd_T_Diff, ymin=NULL, ymax=NULL)) +
  scale_shape_manual(name = "Year", values=shapeVec) +
  scale_linetype_manual(name = "Year", values=linetypeVec) +
  scale_fill_manual(name = "Year", values=fillVec) + mytheme +
  ylab(expression(paste(Delta," Mineraliz. 0-5cm (Inv.-Ref.)"))) + 
  xlab('Soil moisture (%)')
p.soilmoi

leg <- g_legend(p.soilmoi)

# #pdf(file = paste0('output/','plots_minzdT.pdf'), width = 6, height = 6)
# grid.arrange(p.noi + 
#                guides(shape = F, fill = F, linetype = F) +
#                ggtitle("a"),
#              p.soilmoi + 
#                guides(shape = F, fill = F, linetype = F) +
#                ggtitle("b"),
#              leg,
#              ncol = 2)
# #dev.off()
p.minz.noi <- p.noi
p.minz.soilmoi <- p.soilmoi



#-----------------------------------------#
# Nitrification T

plot.list <- readRDS(file = paste0("data_synth/","nitrifdT.RData"))
df <- plot.list$df
mod <- plot.list$mod

# x = noi
df.pred <- XYPlot_DF(n = 100, 
                     xVar = "noi_T", 
                     constantVar = c("soilmoi_T","mv.logt"), 
                     df = df, mod = mod)
p.noi<- ggplot(data=df.pred, 
               mapping=aes(x=noi_T, y=ypred, ymin=CI.lwr, ymax=CI.upr, 
                           shape=factor(year), linetype=factor(year), fill=factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') +
  #geom_line(data=df, mapping=aes(x=noi_T, y=nitrifd_T_Diff,
  #                               group=factor(plotid)), inherit.aes = F, color = "gray") +
  geom_point(data=df, aes(y=nitrifd_T_Diff, ymin=NULL, ymax=NULL)) +
  scale_shape_manual(name = "Year", values=shapeVec) +
  scale_linetype_manual(name = "Year", values=linetypeVec) +
  scale_fill_manual(name = "Year", values=fillVec) + mytheme +
  ylab(expression(paste(Delta," Nitrif. 0-5cm (Inv.-Ref.)"))) + 
  xlab('Soil nitrate (ugN/G)')
p.noi

# x = soilmoi
df.pred <- XYPlot_DF(n = 100, 
                     xVar = "soilmoi_T", 
                     constantVar = c("noi_T","mv.logt"), 
                     df = df, mod = mod)
p.soilmoi<- ggplot(data=df.pred, 
                   mapping=aes(x=soilmoi_T, y=ypred, ymin=CI.lwr, ymax=CI.upr, 
                               shape=factor(year), linetype=factor(year), fill=factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') +
  #geom_line(data=df, mapping=aes(x=soilmoi_T, y=nitrifd_T_Diff,
  #                               group=factor(plotid)), inherit.aes = F, color = "gray") +
  geom_point(data=df, aes(y=nitrifd_T_Diff, ymin=NULL, ymax=NULL)) +
  scale_shape_manual(name = "Year", values=shapeVec) +
  scale_linetype_manual(name = "Year", values=linetypeVec) +
  scale_fill_manual(name = "Year", values=fillVec) + mytheme +
  ylab(expression(paste(Delta," Nitrif. 0-5cm (Inv.-Ref.)"))) + 
  xlab('Soil moisture (%)')
p.soilmoi
leg <- g_legend(p.soilmoi)


pdf(file = paste0('output/','plots_minznitrifdT.pdf'), width = 6, height = 6)
grid.arrange(p.noi + 
               guides(shape = F, fill = F, linetype = F) +
               ggtitle("a"),
             p.soilmoi + 
               guides(shape = F, fill = F, linetype = F)+
               ggtitle("b"),
             p.minz.noi + 
               guides(shape = F, fill = F, linetype = F)+
               ggtitle("c"),
             p.minz.soilmoi + 
               guides(shape = F, fill = F, linetype = F)+
               ggtitle("d"),
             ncol = 2)
dev.off()




#-----------------------------------------#
# Mineralization B

plot.list <- readRDS(file = paste0("data_synth/","minzdB.RData"))
df <- plot.list$df
mod <- plot.list$mod

# x = noi
df.pred <- XYPlot_DF(n = 100, 
                     xVar = "noi_B", 
                     constantVar = c("nat_g.m2","mv.logt","BA_total"), 
                     df = df, mod = mod)
p.noi<- ggplot(data=df.pred, 
               mapping=aes(x=noi_B, y=ypred, ymin=CI.lwr, ymax=CI.upr, 
                           shape=factor(year), linetype=factor(year), fill=factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') +
  geom_point(data=df, aes(y=minzd_B_Diff, ymin=NULL, ymax=NULL)) +
  scale_shape_manual(name = "Year", values=shapeVec) +
  scale_linetype_manual(name = "Year", values=linetypeVec) +
  scale_fill_manual(name = "Year", values=fillVec) + mytheme +
  ylab(expression(paste(Delta," Mineraliz. 5-15cm (Inv.-Ref.)"))) + 
  xlab('Soil nitrate (ugN/G)')
p.noi

# x = nat
df.pred <- XYPlot_DF(n = 100, 
                     xVar = "nat_g.m2", 
                     constantVar = c("noi_B","mv.logt","BA_total"), 
                     df = df, mod = mod)
p.nat<- ggplot(data=df.pred, 
               mapping=aes(x=nat_g.m2, y=ypred, ymin=CI.lwr, ymax=CI.upr, 
                           shape=factor(year), linetype=factor(year), fill=factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') +
  geom_point(data=df, aes(y=minzd_B_Diff, ymin=NULL, ymax=NULL)) +
  scale_shape_manual(name = "Year", values=shapeVec) +
  scale_linetype_manual(name = "Year", values=linetypeVec) +
  scale_fill_manual(name = "Year", values=fillVec) + mytheme +
  ylab(expression(paste(Delta," Mineraliz. 5-15cm (Inv.-Ref.)"))) + 
  xlab('Understory biomass (g/m2)')
p.nat

# x = mv*noi
df.fact <- SetupFactors(df = df, catVar = 'noi_B', 
                   labelNames=c('Low nitrate (<1.6)', 'High nitrate (>1.6)'))
ggplot(df.fact, aes(x = factorLevel, y = noi_B)) +
  geom_point()
range(df.fact[df.fact$factorLevel=="High nitrate (>1.6)","noi_B"])
range(df.fact[df.fact$factorLevel=="Low nitrate (<1.6)","noi_B"])
df.pred <- InteractionPlot_DF(n = 100, 
                     xVar = "mv.logt", 
                     factorVar = "noi_B",
                     factorBin = 'factorLevel',
                     constantVar = c("nat_g.m2","BA_total"), 
                     df = df.fact, mod = mod)
p.mv<- ggplot(data=df.pred, 
       mapping=aes(x = mv.logt, 
                   y = ypred, 
                   ymin = CI.lwr, ymax = CI.upr, 
                   shape = factor(year), linetype = factor(year), fill = factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  facet_grid(~factorLevel) +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') + 
  mytheme + ylab(expression(paste(Delta," Mineraliz. 5-15cm (Inv.-Ref.)"))) + 
  xlab('M.v. biomass (log g/m2)') + 
  geom_point(data=df.fact, aes(y=minzd_B_Diff, ymin=NULL, ymax=NULL))+
  scale_shape_manual(values=shapeVec) +
  scale_linetype_manual(values=linetypeVec) +
  scale_fill_manual(values=fillVec)
p.mv

### alternative way of showing the interaction....
p.noiint <- ggplot(df.fact, aes(y = minzd_B_Diff, x = mv.logt, 
                                color = noi_B, 
                                shape = factor(year))) +
  geom_point(alpha =.8, size = 3) + mytheme +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab(expression(paste(Delta," Mineraliz. 5-15cm (Inv.-Ref.)"))) +
  xlab("M.v. biomass (log g/m2)") +
  scale_color_continuous(name = "Soil nitrate (ugN/G)") +
  scale_shape_discrete(name = "Year")
p.noiint
leg <- g_legend(p.noiint)

# x = BA_total
df.pred <- XYPlot_DF(n = 100, 
                     xVar = "BA_total", 
                     constantVar = c("noi_B","mv.logt","nat_g.m2"), 
                     df = df, mod = mod)
p.percpar<- ggplot(data=df.pred, 
                   mapping=aes(x=BA_total, y=ypred, ymin=CI.lwr, ymax=CI.upr, 
                               shape=factor(year), linetype=factor(year), fill=factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') +
  geom_point(data=df, aes(y=minzd_B_Diff, ymin=NULL, ymax=NULL)) +
  scale_shape_manual(name = "Year", values=shapeVec) +
  scale_linetype_manual(name = "Year", values=linetypeVec) +
  scale_fill_manual(name = "Year", values=fillVec) + mytheme +
  ylab(expression(paste(Delta," Mineraliz. 5-15cm (Inv.-Ref.)"))) + 
  xlab('Tree basal area (m2)')
p.percpar

pdf(file = paste0('output/','plots_minzdB_v1.pdf'), width = 6, height = 9)
grid.arrange(p.noiint + 
               guides(shape = F, color = F) +
               ggtitle("a"),
             leg,
             p.percpar + 
               guides(shape = F, linetype = F, fill = F) +
               ggtitle("b"),
             p.treeba +
               guides(shape = F, fill = F, linetype = F) +
               ggtitle("d"),
             p.nat + 
               guides(shape = F, linetype = F, fill = F) +
               ggtitle("c"),
             nrow = 3
)
dev.off()





#-----------------------------------------#
# Ammonification B

plot.list <- readRDS(file = paste0("data_synth/","ammonifdB.RData"))
df <- plot.list$df
mod <- plot.list$mod

# x = noi
df.pred <- XYPlot_DF(n = 100, 
                     xVar = "noi_B", 
                     constantVar = c("nat_g.m2","mv.logt","BA_total"), 
                     df = df, mod = mod)
p.noi<- ggplot(data=df.pred, 
               mapping=aes(x=noi_B, y=ypred, ymin=CI.lwr, ymax=CI.upr, 
                           shape=factor(year), linetype=factor(year), fill=factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') +
  geom_point(data=df, aes(y=ammonifd_B_Diff, ymin=NULL, ymax=NULL)) +
  scale_shape_manual(name = "Year", values=shapeVec) +
  scale_linetype_manual(name = "Year", values=linetypeVec) +
  scale_fill_manual(name = "Year", values=fillVec) + mytheme +
  ylab(expression(paste(Delta," Ammonif. 5-15cm (Inv.-Ref.)"))) + 
  xlab('Soil nitrate (ugN/G)')
p.noi

# x = nat
df.pred <- XYPlot_DF(n = 100, 
                     xVar = "nat_g.m2", 
                     constantVar = c("noi_B","mv.logt","BA_total"), 
                     df = df, mod = mod)
p.nat<- ggplot(data=df.pred, 
               mapping=aes(x=nat_g.m2, y=ypred, ymin=CI.lwr, ymax=CI.upr, 
                           shape=factor(year), linetype=factor(year), fill=factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') +
  geom_point(data=df, aes(y=ammonifd_B_Diff, ymin=NULL, ymax=NULL)) +
  scale_shape_manual(name = "Year", values=shapeVec) +
  scale_linetype_manual(name = "Year", values=linetypeVec) +
  scale_fill_manual(name = "Year", values=fillVec) + mytheme +
  ylab(expression(paste(Delta," Ammonif. 5-15cm (Inv.-Ref.)"))) + 
  xlab('Understory biomass (g/m2)')
p.nat

# x = mv*noi
df.fact <- SetupFactors(df = df, catVar = 'noi_B', 
                        labelNames=c('Low nitrate (<1.6)', 'High nitrate (>1.6)'))
df.pred <- InteractionPlot_DF(n = 100, 
                              xVar = "mv.logt", 
                              factorVar = "noi_B",
                              factorBin = 'factorLevel',
                              constantVar = c("nat_g.m2","BA_total"), 
                              df = df.fact, mod = mod)
p.mv<- ggplot(data=df.pred, 
              mapping=aes(x = mv.logt, 
                          y = ypred, 
                          ymin = CI.lwr, ymax = CI.upr, 
                          shape = factor(year), linetype = factor(year), fill = factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  facet_grid(~factorLevel) +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') + 
  mytheme + ylab(expression(paste(Delta," Ammonif. 5-15cm (Inv.-Ref.)"))) + 
  xlab('M.v. biomass (log g/m2)') + 
  geom_point(data=df.fact, aes(y=ammonifd_B_Diff, ymin=NULL, ymax=NULL))+
  scale_shape_manual(values=shapeVec) +
  scale_linetype_manual(values=linetypeVec) +
  scale_fill_manual(values=fillVec)
p.mv

### alternative way of showing the interaction....
p.noiint <- ggplot(df.fact, aes(y = ammonifd_B_Diff, x = mv.logt, 
                                color = noi_B, 
                                shape = factor(year))) +
  geom_point(alpha =.8, size = 3) + mytheme +
  geom_hline(yintercept = 0, linetype = 2) +
  ylab(expression(paste(Delta," Ammonif. 5-15cm (Inv.-Ref.)"))) +
  xlab("M.v. biomass (log g/m2)") +
  scale_color_continuous(name = "Soil nitrate (ugN/G)") +
  scale_shape_discrete(name = "Year")
p.noiint
leg <- g_legend(p.noiint)

# x = BA_total
df.pred <- XYPlot_DF(n = 100, 
                     xVar = "BA_total", 
                     constantVar = c("noi_B","mv.logt","nat_g.m2"), 
                     df = df, mod = mod)
p.percpar<- ggplot(data=df.pred, 
               mapping=aes(x=BA_total, y=ypred, ymin=CI.lwr, ymax=CI.upr, 
                           shape=factor(year), linetype=factor(year), fill=factor(year)))  +  
  geom_ribbon(alpha=0.5) +
  geom_line() +
  geom_abline(intercept=0,slope=0, linetype=3, color='gray50') +
  geom_point(data=df, aes(y=ammonifd_B_Diff, ymin=NULL, ymax=NULL)) +
  scale_shape_manual(name = "Year", values=shapeVec) +
  scale_linetype_manual(name = "Year", values=linetypeVec) +
  scale_fill_manual(name = "Year", values=fillVec) + mytheme +
  ylab(expression(paste(Delta," Ammonif. 5-15cm (Inv.-Ref.)"))) + 
  xlab('Tree basal area (m2)')
p.percpar

pdf(file = paste0('output/','plots_ammonifdB_v1.pdf'), width = 6, height = 9)
grid.arrange(p.noiint + 
               guides(shape = F, color = F) +
               ggtitle("a"),
             leg,
             p.percpar + 
               guides(shape = F, linetype = F, fill = F) +
               ggtitle("b"),
             textGrob(""),
             p.nat + 
               guides(shape = F, linetype = F, fill = F) +
               ggtitle("c"),
             nrow = 3
)
dev.off()

