# E8 niche plots

#figuresPath <- "output/"
#fig.height <- 2.5 #inches
#fig.width <- 2.5 #inches
#fig.res <- 300

#load libraries
library("tidyverse") # ggplot2, tidyr, readr, dplyr
library("grid") # grid.arrange()
library("gridExtra") #grid.arrange()
library("vegan") #for adonis fxn to do perMANOVAs
library("lme4") #for mixed-effects models
library('scales') # for ggbiplot fxn
library("lmerTest") #for lmer p-values
#library("GGally") # for ggpairs()

#load functions
source('code/fxns_helpers.R')
source('code/mytheme.R')
source('code/fxns_cleanMerge.R')
source('code/fxns_q1.R')
source('code/fxns_q2.R')
source('code/fxns_q3.R')
source('code/fxns_FitSEM.R')




# -------------------------------------------------------------------#
# Load data
data <- load_data()

# beware that there is a different terminology for plot types in the manuscript and in the code
# 18 sites each with 1 invaded and 1 reference plot (using manuscript terminology) ... this includes 2 sites w/o trees
# hereafter in this code, site = plot and plot = plothalf
length(unique(data$plotid))

# plot and plothalf measurements include ...
unique(data[, c('variable','measLevel')])

# percpar12, nTrees, and BA_total were measured in 2012, hence the values are repeated

# missing samples?
data %>%
  group_by(variable, inv, year) %>%
  summarize(n = sum(!is.na(value))) -> n.samps
data %>%
  filter(is.na(value)) 
# reason for why there *were* NAs ...
# previously, samples with a below detection value for initial/final NH4/NO3, the mineralization rate was not calculated; cell is 'NA'
# currently, mineralization rates were calculated with dummy value (0.0001) if an initial or final inorganic N concentration was below detection



# -------------------------------------------------------------------#
# 1. Q1: Do soil N pools and fluxes shift in response to the presence of Microstegium? 
#A. Do an ordination of soil N pools and fluxes and test the role of invasion status with permMANOVA
pca.soiln <- plot_pca_soiln(data)
#pdf(file = paste0("output/",'PCA_plotNvars_points.pdf'), width = 5, height = 5)
pca.soiln[['p']] # add vectors?
#dev.off()
pca.soiln[['perm']]
#write.table(pca.soiln[['perm']], file=paste('output','permMANOVA_q1.txt', sep='/'), sep='\t')

#B. Investigate individual relationships using mixed effects models with year+site as a random effect and identify N variables that increase/decrease across sites.
inveffect <- inveffect_models(data)
write.csv(inveffect$means.df, file=paste0('output/','means_q1.csv'))
write.csv(inveffect$anova.df, file=paste0('output/','anova_q1.csv'))
plot_inveffect(means.df = inveffect$means.df) # pScat_q1.pdf




# -------------------------------------------------------------------#
# 2. Q2: Do reference plot conditions and/or Microstegium biomass predict impact magnitudes?

# calculate impact magnitude for each target variable (nitrate, nitrification, other variables detected in Q1 ordination)
diffVars <- c('noi_T','noi_B','ammonifd_T','ammonifd_B','nitrifd_T','nitrifd_B','minzd_T','minzd_B')
impact.df <- make_impact.df(data, diffVars)

# (1) "Backward" -- Compare model w/ all reference variables vs empty; if full model explains some variation...
# Do model selection on model w/ all reference variables with step() to reduce the number of reference variables
# Use the reference variables included in the final model to build the q2 test model (resp ~ ref + mv + ref:mv + (1|year))
an.list <- lapply(diffVars, function(x){
  result <- refvars_modelselection_keepMv(diffVar = x, impact.df = impact.df)
  return(result$an)
})
names(an.list) <- diffVars
an.df <- list_to_df(an.list)
an.df
write.csv(an.df, file = paste0('output/','anova_q2_allvEmpty.csv'))

an.df %>%
  filter(pval < 0.05) -> an.df.signif # resp variables that are explained by ref variables > empty model
an.df.signif

# for each resp variable that is explained by some combination of mv and ref conditions....
# (a) do model selection on additive model where you keep mv.logt to reduce the number of ref condition variables
# (b) given select ref conditions variables, build a mv*ref model; do model selection where you keep mv.logt
# print summary of the final model

mod.reduce.list <- list()
new.mod.reduce.list <- list()

#ammonifd_B
result <- refvars_modelselection_keepMv(diffVar = "ammonifd_B", impact.df = impact.df)
mod <- lmer(result$modelFormula, data = result$df)
mod.reduce <- step(mod, reduce.random = FALSE, keep.effs = "mv.logt")
mod.reduce.list[['ammonifd_B']] <- data.frame(term = row.names(mod.reduce$anova.table), mod.reduce$anova.table)
mod.reduce.list[['ammonifd_B']]
new.mod <- lmer(ammonifd_B_Diff ~ nat_g.m2 + noi_B + percpar12.logt + mv.logt +
                  nat_g.m2:mv.logt + noi_B:mv.logt + percpar12.logt:mv.logt + (1|year), data = result$df)
new.mod.reduce <- step(new.mod, reduce.random = FALSE, keep.effs = "mv.logt")
new.mod.reduce.list[['ammonifd_B']]<- data.frame(term = row.names(new.mod.reduce$anova.table), new.mod.reduce$anova.table)
new.mod.reduce.list[['ammonifd_B']]

#nitrifd_T
result <- refvars_modelselection_keepMv(diffVar = "nitrifd_T", impact.df = impact.df)
mod <- lmer(result$modelFormula, data = result$df)
mod.reduce <- step(mod, reduce.random = FALSE, keep.effs = "mv.logt")
mod.reduce.list[['nitrifd_T']] <- data.frame(term = row.names(mod.reduce$anova.table), mod.reduce$anova.table)
mod.reduce.list[['nitrifd_T']] 
new.mod <- lmer(nitrifd_T_Diff ~ noi_T + soilmoi_T + mv.logt +
                  noi_T:mv.logt + soilmoi_T:mv.logt + (1|year), data = result$df)
new.mod.reduce <- step(new.mod, reduce.random = FALSE, keep.effs = "mv.logt")
new.mod.reduce.list[['nitrifd_T']]<- data.frame(term = row.names(new.mod.reduce$anova.table), new.mod.reduce$anova.table)
new.mod.reduce.list[['nitrifd_T']]

#minzd_T
result <- refvars_modelselection_keepMv(diffVar = "minzd_T", impact.df = impact.df)
mod <- lmer(result$modelFormula, data = result$df)
mod.reduce <- step(mod, reduce.random = FALSE, keep.effs = "mv.logt")
mod.reduce.list[['minzd_T']] <- data.frame(term = row.names(mod.reduce$anova.table), mod.reduce$anova.table)
mod.reduce.list[['minzd_T']] 
new.mod <- lmer(minzd_T_Diff ~ noi_T + soilmoi_T + mv.logt +
                  noi_T:mv.logt + soilmoi_T:mv.logt + (1|year), data = result$df)
new.mod.reduce <- step(new.mod, reduce.random = FALSE, keep.effs = "mv.logt")
new.mod.reduce.list[['minzd_T']] <- data.frame(term = row.names(new.mod.reduce$anova.table), new.mod.reduce$anova.table)
new.mod.reduce.list[['minzd_T']]

#minzd_B
result <- refvars_modelselection_keepMv(diffVar = "minzd_B", impact.df = impact.df)
mod <- lmer(result$modelFormula, data = result$df)
mod.reduce <- step(mod, reduce.random = FALSE, keep.effs = "mv.logt")
mod.reduce.list[['minzd_B']] <- data.frame(term = row.names(mod.reduce$anova.table), mod.reduce$anova.table)
new.mod <- lmer(minzd_B_Diff ~ nat_g.m2 + noi_B + mv.logt +
                  nat_g.m2:mv.logt + noi_B:mv.logt + (1|year), data = result$df)
new.mod.reduce <- step(new.mod, reduce.random = FALSE, keep.effs = "mv.logt")
new.mod.reduce.list[['minzd_B']] <- data.frame(term = row.names(new.mod.reduce$anova.table), new.mod.reduce$anova.table)
new.mod.reduce.list[['minzd_B']]

# export anova tables
mod.reduce.df <- list_to_df(mod.reduce.list)
write.csv(mod.reduce.df, file = paste0("output/", "anova_q2_modreduce.csv"))
new.mod.reduce.df <- list_to_df(new.mod.reduce.list)
write.csv(new.mod.reduce.df, file = paste0("output/", "anova_q2_FINALmodreduce.csv"))

## make dfs for plots and export because ....
### warning: dplyr::select doesn't work after you load merTools package

# minzd_T
new.mod.reduce.df %>%
  filter(source == "minzd_T")
result <- refvars_modelselection_keepMv(diffVar = "minzd_T", impact.df = impact.df)
mod <- lmer(minzd_T_Diff ~ noi_T + soilmoi_T + mv.logt + (1|year), data = result$df.notscaled)
plot.list <- list(df = result$df.notscaled, mod = mod)
saveRDS(plot.list, file = paste0("data_synth/","minzdT.RData"))

# nitrifd_T
new.mod.reduce.df %>%
  filter(source == "nitrifd_T")
result <- refvars_modelselection_keepMv(diffVar = "nitrifd_T", impact.df = impact.df)
mod <- lmer(nitrifd_T_Diff ~ noi_T + soilmoi_T + mv.logt + (1|year), data = result$df.notscaled)
plot.list <- list(df = result$df.notscaled, mod = mod)
saveRDS(plot.list, file = paste0("data_synth/","nitrifdT.RData"))

# minzd_B
new.mod.reduce.df %>%
  filter(source == "minzd_B")
result <- refvars_modelselection_keepMv(diffVar = "minzd_B", impact.df = impact.df)
mod <- lmer(minzd_B_Diff ~ nat_g.m2 + noi_B + mv.logt + noi_B:mv.logt + (1|year), data = result$df.notscaled)
plot.list <- list(df = result$df.notscaled, mod = mod)
saveRDS(plot.list, file = paste0("data_synth/","minzdB.RData"))

# ammonifd_B
new.mod.reduce.df %>%
  filter(source == "ammonifd_B")
result <- refvars_modelselection_keepMv(diffVar = "ammonifd_B", impact.df = impact.df)
mod <- lmer(ammonifd_B_Diff ~ nat_g.m2 + noi_B + percpar12.logt + mv.logt + noi_B:mv.logt + (1|year), data = result$df.notscaled)
plot.list <- list(df = result$df.notscaled, mod = mod)
saveRDS(plot.list, file = paste0("data_synth/","ammonifdB.RData"))

# make plots using this script...
# source('script_regPlots.R')


# -------------------------------------------------------------------#
# 3. Q3: Do reference plot conditions predict Microstegium biomass?

#Test relationship between reference variables and Microstegium biomass using mixed effects models with year as a random effect.
pca.ref <- plot_pca_ref(data)
data.q3 <- pca.ref$curr.data
refvars <- colnames(data.q3)[!colnames(data.q3) %in% c("plotid","year","mv","mv.logt")]

# all reference variables
data.q3 %>%
  mutate(ammonifd = (ammonifd_B + ammonifd_T)/2) %>%
  mutate(minzd = (minzd_B + minzd_T)/2) %>%
  mutate(nitrifd = (nitrifd_B + nitrifd_T)/2) %>%
  mutate(nhi = (nhi_B + nhi_T)/2) %>%
  mutate(noi = (noi_B + noi_T)/2) %>%
  mutate(soilmoi = (soilmoi_B + soilmoi_T)/2) %>%
  mutate(som = (som_B + som_T)/2) %>%
  mutate(ph = (ph_B + ph_T)/2) %>%
  mutate(percpar12.logt = log(percpar12)) %>%
  select(plotid, year, nhi, noi, ammonifd, minzd, nitrifd, soilmoi, som, ph,
         BA_total, litter_g.m2, nat_g.m2, nTrees, percpar12.logt, mv.logt) -> tmp

tmp.justvals <- tmp[,!colnames(tmp) %in% c("plotid","year")]
tmp.justvals.s <- scale(tmp.justvals)
tmp.s <- data.frame(tmp[, c("plotid","year")], tmp.justvals.s)

#remove ammonifd, nitrifd, minzd to avoid circularity
mod <- lmer(mv.logt ~ nhi + noi + soilmoi + som + ph +
              BA_total + litter_g.m2 + nat_g.m2 + nTrees +
              percpar12.logt + (1|year), data = tmp.s)
mod.step <- step(mod, reduce.random = FALSE)
an.df <- data.frame(row.names(mod.step$anova.table), mod.step$anova.table)

write.csv(an.df, file = paste0('output/','anova_q3.csv'))


## make df for plots and export because ....
### warning: dplyr::select doesn't work after you load merTools package
mod <- lmer(mv.logt ~ BA_total + percpar12.logt + (1|year), data = tmp) #plot unscaled data
plot.list <- list(df = tmp, mod = mod)
saveRDS(plot.list, file = paste0("data_synth/","mv.RData"))

# reference plot relationships relative to light availability
p.ba <- ggplot(data = tmp, aes(x = BA_total, y = percpar12.logt)) +
  geom_point(alpha =.8) + mytheme +
  ylab("Light avail. (log %)") + xlab("Tree basal area (m2)")
p.ntrees <- ggplot(data = tmp, aes(x = nTrees, y = percpar12.logt)) +
  geom_point(alpha =.8) + mytheme +
  ylab("Light avail. (log %)") + xlab("Number of trees")
p.soilmoi <- ggplot(data = tmp, aes(x = soilmoi, y = percpar12.logt)) +
  geom_point(alpha =.8) + mytheme +
  ylab("Light avail. (log %)") + xlab("Soil moisture (%)")
p.nat <- ggplot(data = tmp, aes(x = nat_g.m2, y = percpar12.logt)) +
  geom_point(alpha =.8) + mytheme +
  ylab("Light avail. (log %)") + xlab("Understory biomass (g/m2)")
p.litter <- ggplot(data = tmp, aes(x = litter_g.m2, y = percpar12.logt)) +
  geom_point(alpha =.8) + mytheme +
  ylab("Light avail. (log %)") + xlab("Litter biomass (g/m2)")

pdf(file = paste0('output/','light_refvars.pdf'), width = 6, height = 9)
grid.arrange( 
             p.ba + ggtitle("a"), 
             p.ntrees + ggtitle("b"),
             p.soilmoi + ggtitle("c"),
             p.nat + ggtitle("d"), 
             p.litter + ggtitle("e"), 
             ncol = 2)
dev.off()

# lack of relationship between invader biomass and native biomass
p.nat <- ggplot(data = data.q3, aes(x = nat_g.m2, y = mv)) +
  geom_point(alpha =.8) + mytheme + 
  ylab("Invader biomass (g/m2)") + 
  xlab("Understory biomass (g/m2)")
ggsave(p.nat, file = paste0('output/','invbiom_natbiom.pdf'), width = 3, height = 3)

# -------------------------------------------------------------------#
# 4. Q4: What is the relative importance of reference conditions on impact - direct and indirectly
# there's an incompatibility betweeen dplyr and either QuantPsyc, nlme, or piecewiseSEM
# make the dfs first, then load these packages
resp.diff.vec <- c("nitrifd_T","minzd_T","ammonifd_B","minzd_B")

# Use modSel variables
#mod.mv <- lmer(mv.logt ~ BA_total + percpar12.logt + (1|year), data = tmp.s)
new.mod.reduce.df %>%
  filter(source == "minzd_B")

envvars.list <- list()
envvars.list[['nitrifd_T']] <- c("noi_T","soilmoi_T","percpar12","BA_total")
envvars.list[['minzd_T']] <- c("noi_T","soilmoi_T","percpar12","BA_total")
envvars.list[['ammonifd_B']] <- c("percpar12","noi_B","nat_g.m2","BA_total")
envvars.list[['minzd_B']] <- c("noi_B","nat_g.m2","percpar12","BA_total")
envvars.list

df.list <- list()
for(i in 1:length(resp.diff.vec)){
  df.list[[i]]<- make_sem_df(resp.diff = resp.diff.vec[i],
              envvars = envvars.list[[resp.diff.vec[i]]],
              impact.df = impact.df)
}
names(df.list) <- resp.diff.vec

detach(package:lmerTest, TRUE)
detach(package:lme4, TRUE)
require(QuantPsyc)
require(nlme)
require(piecewiseSEM)

# Use modSel variables .....
names(df.list)

sem.nitrifd_T <- FitSEM.T.nitrif(df = df.list[['nitrifd_T']])
sem.nitrifd_T

sem.minzd_T <- FitSEM.T.minz(df = df.list[['minzd_T']])
sem.minzd_T

sem.ammonifd_B <- FitSEM.B.ammonif(df = df.list[['ammonifd_B']])
sem.ammonifd_B

sem.minzd_B <- FitSEM.B.minz(df = df.list[['minzd_B']])
sem.minzd_B

