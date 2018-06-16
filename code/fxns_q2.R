
make_impact.df <- function(data, diffVars){
  
  # calculate difference (I - N)
  data %>%
    filter(variable %in% diffVars) %>%
    select(year, plotid, inv, variable, value) %>%
    spread(key = inv, value = value) %>%
    mutate(Diff = I - N) %>%
    select(-c(I, N)) %>%
    transform(variable = paste0(variable, "_Diff")) %>%
    rename('value' = 'Diff') %>%
    mutate(type = "diff") -> data.diff
  
  # identify reference plot condition variables
  data %>%
    filter(inv == "N") %>%
    select(year, plotid, variable, value) %>%
    mutate(type = "ref") -> data.ref
  
  # identify microstegium biomass
  data %>%
    filter(inv == "I" & variable == "mv_g.m2") %>%
    transform(value = log(value)) %>%
    transform(variable = "mv_g.m2_logt") %>%
    select(year, plotid, variable, value) %>%
    mutate(type = "mv") -> data.mv
  
  # create df
  impact.df <- rbind(data.diff, data.ref, data.mv)
  
  return(impact.df)
}

### use "backward" model selction for reference variables ###
refvars_modelselection_keepMv <- function(diffVar, impact.df){
  
  # identify reference data
  allVars.levels <- allVars_levels()
  refVars <- allVars.levels$allVars
  diffVar.basic <- strsplit(diffVar, "_")[[1]][1]
  depth <- strsplit(diffVar, "_")[[1]][2]
  curr.refVars <- refVars[!grepl(diffVar.basic, refVars)]
  keep.refVars.soil <- curr.refVars[grepl(paste0("_", depth), curr.refVars)]
  curr.refVars <- c(keep.refVars.soil, refVars[17:21])
  # get rid of minzd because it is highly correlated with ammonifd/nitrifd
  curr.refVars<- curr.refVars[!grepl("minzd", curr.refVars)]
  if(diffVar.basic == "minzd"){
    # get rid of ammonifd/nitrifd because it is highly correlated with minzd
    curr.refVars<- curr.refVars[!grepl("ammonifd", curr.refVars)]
    curr.refVars<- curr.refVars[!grepl("nitrifd", curr.refVars)]
  }
  # get rid of light avail because it can't directly influence soil N pools/fluxes
  curr.refVars <- curr.refVars[curr.refVars != "percpar12"]
  impact.df %>%
    filter(variable %in% curr.refVars) %>%
    spread(key = variable, value = value) %>%
    select(-type) -> refvars.df
  colnames(refvars.df)
  
  # identify mv biomass
  impact.df %>%
    filter(variable == "mv_g.m2_logt") %>%
    select(year, plotid, value) %>%
    rename('mv.logt'='value') -> mv.df
  
  #join predictors
  refvars.df %>%
    left_join(mv.df) -> pred.df 
  
  # scale the predictor variables
  df.s <- scale(pred.df[,!colnames(pred.df) %in% c("plotid","year")])
  df <- data.frame(plotid = pred.df$plotid, year = pred.df$year, df.s)
  
  # add response variable
  impact.df %>%
    filter(grepl(diffVar, variable) & type == "diff") %>%
    spread(key = variable, value = value) %>%
    select(-type) -> resp.df
  df %>%
    left_join(resp.df) %>%
    transform(year = factor(year)) -> curr.df
  
  # add response variable to non-scaled predictors
  pred.df %>%
    left_join(resp.df) %>%
    transform(year = factor(year)) -> curr.df.notscaled
  
  # create model formulas
  refVars.str <- paste(colnames(df)[!colnames(df) %in% c('plotid','year')], collapse = " + ")
  respVars.vec <- colnames(resp.df)[!colnames(resp.df) %in% c('plotid','year')]
  modelFormula <- paste0(respVars.vec, " ~ ", refVars.str, " + (1|year)")
  modelFormula0 <- paste0(respVars.vec, " ~ ", "1 + (1|year)")
  .env <- environment() ## identify the environment
  fo <- as.formula(modelFormula, env = .env)
  
  # run full model and do model selection
  require(lmerTest)
  mod <- lmer(modelFormula, data = curr.df)
  mod0 <- lmer(modelFormula0, data = curr.df) 
  an <- anova(mod, mod0)
  an.df <- data.frame(model = c("1","full"), data.frame(an))
  an.df %>%
    rename('pval' = 'Pr..Chisq.') -> an.df
  result <- list(an = an.df, df = curr.df, modelFormula = fo, df.notscaled = curr.df.notscaled)
  return(result)
  
}

batch_ifkey_reducenrun <- function(key.resp, impact.df){
  
  refvars.keep <- list()
  #ammonifd_B
  result <- refvars_modelselection(diffVar = key.resp[1], impact.df = impact.df)
  mod <- lmer(result$modelFormula, data = result$df)
  mod.reduce <- step(mod, reduce.random = FALSE)
  mod.reduce.df <- data.frame(term = row.names(mod.reduce$anova.table), mod.reduce$anova.table)
  mod.reduce.df %>%
    filter(elim.num == "kept") -> tmp
  refvars.keep[[1]]<- tmp
  #nitrifd_T
  result <- refvars_modelselection(diffVar = key.resp[2], impact.df = impact.df)
  mod <- lmer(result$modelFormula, data = result$df)
  mod.reduce <- step(mod, reduce.random = FALSE)
  mod.reduce.df <- data.frame(term = row.names(mod.reduce$anova.table), mod.reduce$anova.table)
  mod.reduce.df %>%
    filter(elim.num == "kept") -> refvars.keep[[2]]
  #minzd_T
  result <- refvars_modelselection(diffVar = key.resp[3], impact.df = impact.df)
  mod <- lmer(result$modelFormula, data = result$df)
  mod.reduce <- step(mod, reduce.random = FALSE)
  mod.reduce.df <- data.frame(term = row.names(mod.reduce$anova.table), mod.reduce$anova.table)
  mod.reduce.df %>%
    filter(elim.num == "kept") -> refvars.keep[[3]]
  #minzd_B
  result <- refvars_modelselection(diffVar = key.resp[4], impact.df = impact.df)
  mod <- lmer(result$modelFormula, data = result$df)
  mod.reduce <- step(mod, reduce.random = FALSE)
  mod.reduce.df <- data.frame(term = row.names(mod.reduce$anova.table), mod.reduce$anova.table)
  mod.reduce.df %>%
    filter(elim.num == "kept") -> refvars.keep[[4]]
  names(refvars.keep) <- key.resp
  refvars.keep <- list_to_df(refvars.keep)
  refvars.keep %>%
    rename('respVar' = 'source',
           'refVar' = 'term') -> refvars.keep
  key.var.indx <- unique(refvars.keep[,c("respVar","refVar")])
  
  # use these refvars to build the full model
  an.df <- batch_run_q2_keyrespvars(key.var.indx = key.var.indx, impact.df = impact.df)
  an.df
  
  return(an.df)
  
}

run_q2_keyrespvars <- function(diffVar, impact.df, refVars){
  
  #identify reference data
  impact.df %>%
    filter(variable %in% refVars) %>%
    spread(key = variable, value = value) %>%
    arrange(plotid, year) %>%
    select(-type) -> ref.df
  
  # add mv
  impact.df %>%
    filter(variable == "mv_g.m2_logt") %>%
    spread(key = variable, value = value) %>%
    select(-type) -> mv.df
  ref.df %>%
    left_join(mv.df) -> df
  
  # scale the predictor variables
  df.s <- scale(df[,!colnames(df) %in% c("plotid","year")])
  df <- data.frame(plotid = df$plotid, year = df$year, df.s)
  
  # add response variable
  impact.df %>%
    filter(grepl(diffVar, variable) & type == "diff") %>%
    spread(key = variable, value = value) %>%
    select(-type) -> resp.df
  df %>%
    left_join(resp.df) %>%
    transform(year = factor(year)) -> curr.df
  
  # create model formulas
  ref.vars <- colnames(ref.df)[!colnames(ref.df) %in% c('plotid','year')]
  mv <- "mv_g.m2_logt"
  ref.vars.bymv <- paste(ref.vars, mv, sep = "*")
  resp.var <- colnames(resp.df)[!colnames(resp.df) %in% c('plotid','year')]
  modelFormula <- paste0(resp.var, " ~ ", 
                         paste(ref.vars.bymv, collapse = " + "),
                         " + (1|year)")
  .env <- environment() ## identify the environment
  modelFormula <- as.formula(modelFormula, env = .env)
  
  # run model
  mod <- lmer(modelFormula, data = curr.df)
  an <- data.frame(term = row.names(anova(mod)), anova(mod))
  return(an)
}

batch_run_q2_keyrespvars <- function(key.var.indx, impact.df){
  
  respVars <- unique(key.var.indx$respVar)
  an.list <- list()
  for(i in 1:length(respVars)){
    refVars <- key.var.indx[key.var.indx$respVar == respVars[i], "refVar"]
    
    an.list[[i]] <- run_q2_keyrespvars(diffVar = respVars[i], 
                                       impact.df = impact.df, 
                                       refVars = as.character(refVars))
    
  }
  names(an.list) <- respVars
  an.df <- list_to_df(an.list)
  an.df %>%
    rename('respvar' = 'source',
           'pval' = 'Pr..F.') -> an.df
  
  return(an.df)
}

make_q2back_b_plots <- function(an.df.ifkey.signif, impact.df){
  
  an.df.ifkey.signif
  
  an.df.ifkey.signif %>%
    filter(respvar == "ammonifd_B")
  impact.df %>%
    filter(variable %in% c("ammonifd_B_Diff","noi_B","percpar12","som_B","mv_g.m2_logt")) %>%
    select(-type) %>%
    spread(key = variable, value = value) -> plot.df
  p.ammonifd.b <- ggplot(plot.df, aes(x = mv_g.m2_logt, y = ammonifd_B_Diff, 
                                      color = noi_B)) +
    geom_point(size = 3, alpha = .8) + mytheme +
    geom_hline(yintercept = 0, linetype = 2) +
    scale_color_continuous(name = "Nitrate", 
                           guide = guide_legend(
                             direction = "horizontal",
                             title.position = "top",
                             label.position = "bottom",
                             label.hjust = 0.5,
                             label.vjust = 1,
                             order = 1)
    ) +
    ylab("Ammonif. 5-15cm diff.\n(Invaded - Ref.)") + 
    xlab("Invader biomass\n (log trans.)")
  p.ammonifd.b
  leg1 <- g_legend(p.ammonifd.b)
  
  an.df.ifkey.signif %>%
    filter(respvar == "minzd_B")
  impact.df %>%
    filter(variable %in% c("minzd_B_Diff","nat_g.m2","noi_B","percpar12","mv_g.m2_logt")) %>%
    select(-type) %>%
    spread(key = variable, value = value) -> plot.df
  p.minzd.b <- ggplot(plot.df, aes(x = mv_g.m2_logt, y = minzd_B_Diff, color = noi_B)) +
    geom_point(size = 3, alpha = .8) + mytheme +
    geom_hline(yintercept = 0, linetype = 2) +
    scale_color_continuous(name = "Nitrate", breaks = c(2,4,6),
                           guide = guide_legend(
                             direction = "horizontal",
                             title.position = "top",
                             label.position = "bottom",
                             label.hjust = 0.5,
                             label.vjust = 1,
                             order = 1)
    ) +
    ylab("Mineraliz. 5-15cm diff.\n(Invaded - Ref.)") + xlab("Invader biomass\n (log trans.)")
  leg4 <- g_legend(p.minzd.b)
  p.minzd.b 
  
  p.minzd.b.nat <- ggplot(plot.df, aes(x = log(nat_g.m2), y = minzd_B_Diff, color = noi_B)) +
    geom_point(size = 3, alpha = .8) + mytheme +
    geom_hline(yintercept = 0, linetype = 2) +
    scale_color_continuous(name = "Nitrate", 
                           guide = guide_legend(
                             direction = "horizontal",
                             title.position = "top",
                             label.position = "bottom",
                             label.hjust = 0.5,
                             label.vjust = 1,
                             order = 1)
    ) +
    ylab("Mineraliz. 5-15cm diff.\n(Invaded - Ref.)") + xlab("Understory biomass\n (log trans.)")
  leg5 <- g_legend(p.minzd.b.nat)
  p.minzd.b.nat
  
  pdf(file = paste0('output/','q2ModSel_b_plots.pdf'), width = 6, height = 6)
  grid.arrange(p.ammonifd.b + guides(color = F, size = F), 
               leg1,
               p.minzd.b + guides(color = F),
               p.minzd.b.nat + guides(color = F),
               ncol = 2)
  dev.off()
  
}
