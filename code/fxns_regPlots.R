
make_plots_minzdT <- function(){
  

}

make_plots_nitrifdT <- function(){
  
  plot.list <- readRDS(file = paste0("data_synth/","nitrifdT.RData"))
  df <- plot.list$df
  mod <- plot.list$mod
  
  # x = noi
  p.noi <- ggplot(plot.df, aes(y = nitrifd_T_Diff, x = noi_T, 
                               shape = factor(year))) +
    geom_point(alpha =.8, size = 3) + mytheme +
    geom_hline(yintercept = 0, linetype = 2) +
    ylab("Nitrif. 0-5cm diff.\n(Invaded - Ref.)") +
    xlab("Soil nitrate (ugN/G*d)")
  p.soilmoi <- ggplot(plot.df, aes(y = nitrifd_T_Diff, x = soilmoi_T, 
                                   shape = factor(year))) +
    geom_point(alpha =.8, size = 3) + mytheme +
    geom_hline(yintercept = 0, linetype = 2) +
    ylab("Nitrif. 0-5cm diff.\n(Invaded - Ref.)") +
    xlab("Soil moisture (%)") +
    scale_shape_discrete(name = "Year")
  leg <- g_legend(p.soilmoi)
  
  pdf(file = paste0('output/','plots_nitrifdT.pdf'), width = 6, height = 6)
  grid.arrange(p.noi + guides(shape = F),
               p.soilmoi + guides(shape = F),
               leg,
               ncol = 2)
  dev.off()
  
}

make_plots_minzdB <- function(){
  
  plot.list <- readRDS(file = paste0("data_synth/","minzdB.RData"))
  df <- plot.list$df
  mod <- plot.list$mod
  
  p.noi <- ggplot(plot.df, aes(y = minzd_B_Diff, x = noi_B, 
                               shape = factor(year))) +
    geom_point(alpha =.8, size = 3) + mytheme +
    geom_hline(yintercept = 0, linetype = 2) +
    ylab("Mineraliz. 5-15cm diff.\n(Invaded - Ref.)") +
    xlab("Soil nitrate (ugN/G*d)")
  p.nat <- ggplot(plot.df, aes(y = minzd_B_Diff, x = nat_g.m2, 
                               shape = factor(year))) +
    geom_point(alpha =.8, size = 3) + mytheme +
    geom_hline(yintercept = 0, linetype = 2) +
    ylab("Mineraliz. 5-15cm diff.\n(Invaded - Ref.)") +
    xlab("Understory biomass (g/m2)")
  p.noiint <- ggplot(plot.df, aes(y = minzd_B_Diff, x = mv_g.m2_logt, 
                                  color = noi_B, 
                                  shape = factor(year))) +
    geom_point(alpha =.8, size = 3) + mytheme +
    geom_hline(yintercept = 0, linetype = 2) +
    ylab("Mineraliz. 5-15cm diff.\n(Invaded - Ref.)") +
    xlab("M.v. biomass (log g/m2)") +
    scale_color_continuous(name = "Soil nitrate (ugN/G*d)") +
    scale_shape_discrete(name = "Year")
  leg <- g_legend(p.noiint)
  pdf(file = paste0('output/','plots_minzdB.pdf'), width = 6, height = 6)
  grid.arrange(p.noi + guides(shape = F), 
               p.nat + guides(shape = F),
               p.noiint + guides(shape = F, color = F),
               leg,
               nrow = 2
  )
  dev.off()
  
}

make_plots_ammonifdB <- function(){
  
  plot.list <- readRDS(file = paste0("data_synth/","ammonifdB.RData"))
  df <- plot.list$df
  mod <- plot.list$mod
  
  p.noi <- ggplot(plot.df, aes(y = ammonifd_B_Diff, x = noi_B, 
                               shape = factor(year))) +
    geom_point(alpha =.8, size = 3) + mytheme +
    geom_hline(yintercept = 0, linetype = 2) +
    ylab("Ammonif. 5-15cm diff.\n(Invaded - Ref.)") +
    xlab("Soil nitrate (ugN/G*d)")
  p.nat <- ggplot(plot.df, aes(y = ammonifd_B_Diff, x = nat_g.m2, 
                               shape = factor(year))) +
    geom_point(alpha =.8, size = 3) + mytheme +
    geom_hline(yintercept = 0, linetype = 2) +
    ylab("Ammonif. 5-15cm diff.\n(Invaded - Ref.)") +
    xlab("Understory biomass (g/m2)")
  p.percpar <- ggplot(plot.df, aes(y = ammonifd_B_Diff, x = log(percpar12), 
                                   shape = factor(year))) +
    geom_point(alpha =.8, size = 3) + mytheme +
    geom_hline(yintercept = 0, linetype = 2) +
    ylab("Ammonif. 5-15cm diff.\n(Invaded - Ref.)") +
    xlab("Light avail. (log %)")
  p.noiint <- ggplot(plot.df, aes(y = ammonifd_B_Diff, x = mv_g.m2_logt, 
                                  color = noi_B, 
                                  shape = factor(year))) +
    geom_point(alpha =.8, size = 3) + mytheme +
    geom_hline(yintercept = 0, linetype = 2) +
    ylab("Ammonif. 5-15cm diff.\n(Invaded - Ref.)") +
    xlab("M.v. biomass (log g/m2)") +
    scale_color_continuous(name = "Soil nitrate (ugN/G*d)") +
    scale_shape_discrete(name = "Year")
  leg <- g_legend(p.noiint)
  pdf(file = paste0('output/','plots_ammonifdB.pdf'), width = 9, height = 6)
  grid.arrange(p.noi + guides(shape = F), 
               p.nat + guides(shape = F),
               textGrob(""),
               p.noiint + guides(shape = F, color = F),
               p.percpar + guides(shape = F),
               leg,
               nrow = 2
  )
  dev.off()
  
}

make_plots_mv <- function(){
  
  plot.list <- readRDS(file = paste0("data_synth/","mv.RData"))
  df <- plot.list$df
  mod <- plot.list$mod
  
  p.ba <- ggplot(data = tmp, aes(x = BA_total, y = mv.logt)) +
    geom_point(alpha =.8) + mytheme +
    ylab("Invader biomass (log g/m2)") + xlab("Tree basal area (m2)")
  p.light <- ggplot(data = tmp, aes(x = percpar12.logt, y = mv.logt)) +
    geom_point(alpha =.8) + mytheme +
    ylab("Invader biomass (log g/m2)") + xlab("Light availability (log %)")
  # * indicates that this values are an average of 0-5 and 5-15cm
  pdf(file = paste0('output/','invbiom_refvars.pdf'), width = 6, height = 3)
  grid.arrange(p.light + ggtitle("a"), 
               p.ba + ggtitle("b"),
               ncol = 2)
  dev.off()

}