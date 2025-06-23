# Models

# Introductory part ####

# attaching libraries
pacman::p_load(moments, ape, lme4, effects, ggResidpanel, performance, afex,# needed for mixed() and attaches lme4 automatically.
               mice, permutes, buildmer, interactions, qqconf,
               
               boot,lmerTest,pbkrtest,emmeans,multcomp,ggpattern, car, coin, glmmTMB,
               DHARMa, broom, Matrix, tidyverse, MASS, dendextend, factoextra,
               Rtsne, elasticnet, insight)

#--------------------------------------------------------------------------------------# 
# converting columns to factors and numeric

HEADER$New_plot_ID <- as.factor(HEADER$New_plot_ID)
HEADER$Latitude <- as.numeric(HEADER$Latitude)
HEADER$Longitude <- as.numeric(HEADER$Longitude)
HEADER$Tree_layer <- as.numeric(HEADER$Tree_layer)
HEADER$Herb_layer <- as.numeric(HEADER$Herb_layer)
HEADER$Moss_layer <- as.numeric(HEADER$Moss_layer)

#--------------------------------------------------------------------------------------#

# Creating plot.class column in the HEADER
HEADER$plot.class <- paste(HEADER$plot_age, HEADER$class, sep=".")

# Converting plot_age column into factor type
HEADER$plot_age <- factor(HEADER$plot_age, levels = c("Old", "New"))

#--------------------------------------------------------------------------------------#
# Creating spatial plot groups for modelling

group1 <- c("1279.New", "1279.Old", "1283.New", "1283.Old", "1913.New", "1913.Old", "1915.New", "1915.Old", "2056.New", "2056.Old",
            "2057.New", "2057.Old", "2067.New", "2067.Old", "2068.New", "2068.Old", "2069.New", "2069.Old", "2070.New", "2070.Old")

group2 <- c("1307.New","1307.Old","1308.New","1308.Old","1309.New","1309.Old","1310.New","1310.Old","1312.New","1312.Old",
            "1315.New","1315.Old","1414.New","1414.Old","1418.New","1418.Old","1419.New","1419.Old","1420.New","1420.Old","1421.New",
            "1421.Old","1422.New","1422.Old","1424.New","1424.Old","1425.New","1425.Old","1426.New","1426.Old","1427.New","1427.Old",
            "1428.New","1428.Old","1429.New","1429.Old","1430.New","1430.Old","1431.New","1431.Old","1433.New","1433.Old","1434.New",
            "1434.Old","1718.New","1718.Old","1719.New","1719.Old","1726.New","1726.Old","1727.New","1727.Old","1729.New","1729.Old",
            "1746.New","1746.Old","1747.New","1747.Old","1748.New","1748.Old","1750.New","1750.Old","2052.New","2052.Old","2053.New",
            "2053.Old","2054.New","2054.Old", "2084.New","2084.Old","2085.New","2085.Old","2087.New","2087.Old",
            "2088.New","2088.Old")

group3 <- c("2094.New","2094.Old","2095.New","2095.Old","2096.New","2096.Old","2097.New","2097.Old","2098.New","2098.Old",
            "2099.New","2099.Old","2101.New","2101.Old","2108.New","2108.Old","2110.New","2110.Old","2111.New","2111.Old",
            "2112.New", "2112.Old")

group4 <- c("101.New","101.Old","106.New","106.Old","1074.New","1074.Old","1075.New","1075.Old","1076.New","1076.Old",
            "1080.New","1080.Old","1081.New","1081.Old","1082.New","1082.Old","1083.New","1083.Old","1084.New","1084.Old","1110.New",
            "1110.Old","1158.New","1158.Old","126.New","126.Old","1333.New","1333.Old","1334.New","1334.Old","1335.New","1335.Old",
            "1336.New","1336.Old","1341.New","1341.Old","1343.New","1343.Old","1344.New","1344.Old","1345.New","1345.Old","14.New",
            "14.Old","1400.New","1400.Old","1401.New","1401.Old","1402.New","1402.Old","1403.New","1403.Old","1437.New","1437.Old",
            "159.New","159.Old","16.New","16.Old","160.New","160.Old","161.New","161.Old","162.New","162.Old","195.New","195.Old",
            "199.New","199.Old","200.New","200.Old","201.New","201.Old","205.New","205.Old","220.New","220.Old","221.New","221.Old",
            "224.New","224.Old","225.New","225.Old","226.New","226.Old","227.New","227.Old","230.New", "230.Old","234.New","234.Old",
            "254.New","254.Old","256.New","256.Old","257.New","257.Old","277.New","277.Old","279.New","279.Old","281.New","281.Old",
            "282.New","282.Old","283.New","283.Old","284.New","284.Old","286.New","286.Old","287.New","287.Old","289.New","289.Old",
            "29.New","29.Old","290.New","290.Old","33.New","33.Old","34.New","34.Old","38.New","38.Old","398.New","398.Old","40.New",
            "40.Old","403.New","403.Old","406.New","406.Old","414.New","414.Old","415.New","415.Old","44.New","44.Old","456.New",
            "456.Old","457.New","457.Old","462.New","462.Old","474.New","474.Old","476.New","476.Old","54.New","54.Old","56.New",
            "56.Old","57.New","57.Old","65.New","65.Old","76.New","76.Old","78.New","78.Old","79.New","79.Old","80.New","80.Old",
            "81.New","81.Old","83.New","83.Old","90.New","90.Old","92.New","92.Old")

group5 <- c("1087.New","1087.Old","1089.New","1089.Old","1090.New","1090.Old","1091.New","1091.Old","1093.New","1093.Old",
            "1098.New","1098.Old","1114.New","1114.Old","1115.New","1115.Old","1116.New","1116.Old","1320.New","1320.Old","1321.New",
            "1321.Old","1323.New","1323.Old")

HEADER$terr <- NA
HEADER$terr[which(HEADER$New_plot_ID%in%group1)] <- "group_1"
HEADER$terr[which(HEADER$New_plot_ID%in%group2)] <- "group_2"
HEADER$terr[which(HEADER$New_plot_ID%in%group3)] <- "group_3"
HEADER$terr[which(HEADER$New_plot_ID%in%group4)] <- "group_4"
HEADER$terr[which(HEADER$New_plot_ID%in%group5)] <- "group_5"
any(is.na(HEADER$terr))
HEADER$terr.class <- paste(HEADER$terr, HEADER$class, sep=".")
HEADER$terr.class <- as.factor(HEADER$terr.class)
levels(HEADER$terr.class)
table(HEADER$terr.class, HEADER$class)

#--------------------------------------------------------------------------------------#
# back_log10_seed_mass_cwm BACK 

HEADER$back_log10_seed_mass_cwm <- 10^HEADER$log10_seed_mass_cwm
HEADER$log_back_log10_seed_mass_cwm <- log10(HEADER$back_log10_seed_mass_cwm)

# back_log10_canopy_height_cwm BACK 
HEADER$back_log10_canopy_height_cwm <- 10^HEADER$log10_canopy_height_cwm
HEADER$log_back_log10_canopy_height_cwm <- log10(HEADER$back_log10_canopy_height_cwm)

#--------------------------------------------------------------------------------------#
# Spatial autocorrelation all####

# Calculating an inversed distance Matrix and adding it as a variable into data frame for
# the further including in models where it is needed.
dists <- as.matrix(dist(cbind(HEADER$Longitude, HEADER$Latitude), method = "euclidean"))

dists_auto <- 1/dists 
diag(dists_auto) <- 0
dist <- pcnm(dists_auto)
HEADER$PCNM1 <-dist$vectors[,1]# this creates a column in the data frame and we should add this
# as predictor variable to the model if we have spatial correlation in response variable!

# Separating the dataset into subsets of class####
only_FB <- HEADER[HEADER$class=="FB",]
only_KC <- HEADER[HEADER$class=="KC",]

# For each response variable, we tested for spatial autocorrelation with Moran’s I autocorrelation 
# coefficient using an inversed distance matrix (package ape: Paradis & Schliep, 2019; Table S4). If 
# applicable (i.e. where Moran’s I was significant at the 0.05 level), we included the first axis of a principal 
# coordinates of neighbour matrix (PCNM) applied to site coordinates, as covariate in the model 
# (package ‘vegan’: Oksanen et al., 2020). 

# Check for SPATIAL AUTOCORR ####
# 
# only_new <- HEADER[HEADER$plot_age=="New",]
# 
# # 1. dist matrix
# distsonly_new <- as.matrix(dist(cbind(only_new$Longitude, only_new$Latitude), method = "euclidean"))
# 
# # 2.inverse
# dists_autoonly_new <- 1/distsonly_new
# 
# diag(dists_autoonly_new) <- 0
# dists_autoonly_new[is.infinite(dists_autoonly_new) | is.na(dists_autoonly_new)] <- 0
# 
# distonly_new <- pcnm(dists_autoonly_new)
# only_new$PCNM1 <-distonly_new$vectors[,1]#

(moran_result <- Moran.I(HEADER$Evenness, dists_auto))
(moran_result <- Moran.I(HEADER$Specnumber, dists_auto))
(moran_result <- Moran.I(HEADER$Shannon, dists_auto))
(moran_result <- Moran.I(HEADER$L_mean, dists_auto))
(moran_result <- Moran.I(HEADER$T_mean, dists_auto))
(moran_result <- Moran.I(HEADER$R_mean, dists_auto))
(moran_result <- Moran.I(HEADER$M_mean, dists_auto))
(moran_result <- Moran.I(HEADER$N_mean, dists_auto))
(moran_result <- Moran.I(HEADER$disturb_sev_herb_cwm, dists_auto))
(moran_result <- Moran.I(HEADER$log_back_log10_seed_mass_cwm, dists_auto))
(moran_result <- Moran.I(HEADER$log_back_log10_canopy_height_cwm, dists_auto))
(moran_result <- Moran.I(HEADER$sla_cwm, dists_auto))

#-------------------------------------------------------------------------------------# 

# MODELS ####

#--------------------------------------------------------------------------------------# 
# Evenness ####

qqnorm(HEADER$Evenness)
qqline(HEADER$Evenness)
skewness(HEADER$Evenness)

# Moran's I test performing 
(moran_result <- Moran.I(HEADER$Evenness, dists_auto))
write.table(moran_result) 
 
mod.evenness <- lmer(Evenness ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova_results <- anova(mod.evenness, ddf = "Kenward-Roger", type = "III")
anova_results
Moran.I(x = residuals(mod.evenness), weight = dists_auto)

plot(allEffects(mod.evenness))
plot(mod.evenness)

ggResidpanel::resid_panel(mod.evenness, plots = c("resid", "qq", "hist"), nrow = 1)
check_collinearity(mod.evenness) # if high colinearity so we'll check the additive effects only model
plot(resid(mod.evenness))

# Calculating the effects object 
eff <- effect("plot_age * class", mod.evenness)
eff

# Calculating marginal means for simple model
marginal_means_Ev <- as.data.frame(effect("plot_age:class", mod.evenness))
marginal_means_Ev$plot_age <- factor(marginal_means_Ev$plot_age, levels = c("Old", "New"))
marginal_means_Ev <- marginal_means_Ev[order(marginal_means_Ev$class, marginal_means_Ev$plot_age), ]
marginal_means_Ev$class_age <- paste(marginal_means_Ev$class, marginal_means_Ev$plot_age, sep=".")
marginal_means_Ev$class_age <- factor(marginal_means_Ev$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

round(marginal_means_Ev$fit,2)
round((marginal_means_Ev$fit-marginal_means_Ev$se),2)
round((marginal_means_Ev$fit+marginal_means_Ev$se),2)

print(marginal_means_Ev)
round(marginal_means_Ev$fit, digits=2)
round(marginal_means_Ev$se, digits=2)

means_model <- cbind(marginal_means_Ev, parameter = "evenness")

#-------------------------------------------------------------------------------------#

# Creating a new data frame
results_df <- data.frame(
  `F (1,155)` = anova_results$`F value`,
  `P` = anova_results$`Pr(>F)`,
  # 'significance' = symnum(anova_results$`Pr(>F)`, corr = FALSE, na = FALSE,
  #                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  #                         symbols = c("***", "**", "*", ".", " ")),
  row.names = rownames(anova_results), check.names = F 
)

results_df <- as.data.frame(t(results_df))
results_df$'F, P' <- rownames(results_df)

# Rounding fit and se
marginal_means_Ev$fit <- round(marginal_means_Ev$fit, 2)
marginal_means_Ev$se <- round(marginal_means_Ev$se, 2)

# Creating the "fit ± se" column
marginal_means_Ev$mean_se <- paste0(marginal_means_Ev$fit, " ± ", marginal_means_Ev$se)

# Creating a new frame
result_table <- reshape(
  marginal_means_Ev[, c("plot_age", "class", "mean_se")],
  idvar = "plot_age",
  timevar = "class",
  direction = "wide"
)

# Giving new names to some columns
colnames(result_table) <- c(" ", "FB", "KC")
result_table$` ` <- as.character(result_table$` `)
result_table$` `[which(result_table$` `=="New")] <- "Evenness (Shannon)_New"
result_table$` `[result_table$` `=="Old"] <- "Evenness (Shannon)_Old"
results_df_all <- cbind(result_table, results_df[c('F, P','plot_age', 'class', 'plot_age:class')])


#-------------------------------------------------------------------------------------#

# # 3. Checking Evenness model with PERM
#  set.seed(101)
#  p1 <- perm.lmer(Evenness ~ plot_age * class + PCNM1 + (1 | Old_plot_ID),
#                  buildmerControl = list(direction = "order", crit = "LRT", quiet = TRUE, ddf = "lme4"),
#                  data = HEADER, nperm = 1000, type = "anova")
#  p1
# 
# # the result show the same pattern!
# perm_mod.ev1 <- mixed(
#   formula = Evenness ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   method = "PB",  # parametric bootstrap method
#   args_test = list(nsim = 10000),
#   type = 3,  # Type III tests
#   progress = TRUE,  # Show progress bar
#   check_contrasts = TRUE  # Ensure contrasts are set correctly
# )
# anova(perm_mod.ev1, ddf = "Kenward-Roger", type = "III")
# 
# perm_mod.ev <- mixed(
#   formula = Evenness ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   method = "KR",  # Kenward-Roger approximation for degrees-of-freedom is calculated using lmerTest
#   args_test = list(nsim = 10000),
#   type = 3,  # Type III tests
#   progress = TRUE,  # Show progress bar
#   check_contrasts = TRUE,  # Ensure contrasts are set correctly
#   control = lmerControl(optCtrl = list(maxfun = 1e6))
# )
# 
# # summary(perm_mod.ev)
# anova(perm_mod.ev, ddf = "Kenward-Roger", type = "III")

#--------------------------------------------------------------------------------------# 
# Shannon ####

qqnorm(HEADER$Shannon)
qqline(HEADER$Shannon)
skewness(HEADER$Shannon)

# Moran's I test performing 
(moran_result <- Moran.I(HEADER$Shannon, dists_auto))
write.table(moran_result)

mod.shan <- lmer(Shannon ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
#summary(mod.shan)
anova_results <- anova(mod.shan, ddf = "Kenward-Roger", type = "III") 
plot(mod.shan)
plot(allEffects(mod.shan))
ggResidpanel::resid_panel(mod.shan, plots = c("resid", "qq", "hist"), nrow = 1)
check_collinearity(mod.shan)
plot(resid(mod.shan))

# Calculating the effects object
eff <- effect("plot_age * class", mod.shan)
eff

# Calculating marginal means
marginal_means_SH <- as.data.frame(effect("plot_age:class", mod.shan))
marginal_means_SH$plot_age <- factor(marginal_means_SH$plot_age, levels = c("Old", "New"))
marginal_means_SH <- marginal_means_SH[order(marginal_means_SH$class, marginal_means_SH$plot_age), ]
marginal_means_SH$class_age <- paste(marginal_means_SH$class, marginal_means_SH$plot_age, sep=".")
marginal_means_SH$class_age <- factor(marginal_means_SH$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))


round(marginal_means_SH$fit,2)
round((marginal_means_SH$fit-marginal_means_SH$se),2)
round((marginal_means_SH$fit+marginal_means_SH$se),2)

print(marginal_means_SH)
round(marginal_means_SH$fit, digits=2)
round(marginal_means_SH$se, digits=2)

means_modelShann <- cbind(marginal_means_SH, parameter = "Shannon")
means_model <- rbind(means_model, means_modelShann)

#------------------------------------------------------------------------------------#

# Creating a new data frame
results_df <- data.frame(
  `F (1,155)` = anova_results$`F value`,
  `P` = anova_results$`Pr(>F)`,
  # 'significance' = symnum(anova_results$`Pr(>F)`, corr = FALSE, na = FALSE,
  #                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  #                         symbols = c("***", "**", "*", ".", " ")),
  row.names = rownames(anova_results), check.names = F 
)

results_df <- as.data.frame(t(results_df))
results_df$'F, P' <- rownames(results_df)

# Rounding fit and se
marginal_means_SH$fit <- round(marginal_means_SH$fit, 2)
marginal_means_SH$se <- round(marginal_means_SH$se, 2)

# Creating the "fit ± se" column
marginal_means_SH$mean_se <- paste0(marginal_means_SH$fit, " ± ", marginal_means_SH$se)

# Creating a new frame
result_table <- reshape(
  marginal_means_SH[, c("plot_age", "class", "mean_se")],
  idvar = "plot_age",
  timevar = "class",
  direction = "wide"
)

# Giving new names to some columns
colnames(result_table) <- c(" ", "FB", "KC")
result_table$` ` <- as.character(result_table$` `)
result_table$` `[which(result_table$` `=="New")] <- "Shannon index_New"
result_table$` `[result_table$` `=="Old"] <- "Shannon index_Old"
results_df_sha <- cbind(result_table, results_df[c('F, P','plot_age', 'class', 'plot_age:class')])

#
results_df_all <- rbind(results_df_all, results_df_sha)

#------------------------------------------------------------------------------------#
# # Checking Shannon model with PERM
# set.seed(101)
# p1 <- perm.lmer(Shannon ~ plot_age * class + PCNM1 + (1 | Old_plot_ID),
#                 buildmerControl = list(direction = "order", crit = "LRT", quiet = TRUE, ddf = "lme4"),
#                 data = HEADER, nperm = 1000, type = "anova")
# p1
# 
# perm_mod.shan <- mixed(
#   formula = Shannon ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   method = "PB",  # parametric bootstrap method
#   args_test = list(nsim = 10000),
#   type = 3,  # Type III tests
#   progress = TRUE,  # Show progress bar
#   check_contrasts = TRUE  # Ensure contrasts are set correctly
# )
# anova(perm_mod.ev, ddf = "Kenward-Roger", type = "III")
# 
# perm_mod.ev <- mixed(
#   formula = Shannon ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   method = "KR",  # Kenward-Roger approximation for degrees-of-freedom is calculated using lmerTest
#   args_test = list(nsim = 10000),
#   type = 3,  # Type III tests
#   progress = TRUE,  # Show progress bar
#   check_contrasts = TRUE,  # Ensure contrasts are set correctly
#   control = lmerControl(optCtrl = list(maxfun = 1e6))
# )
# 
# # summary(perm_mod.ev)
# anova(perm_mod.ev, ddf = "Kenward-Roger", type = "III")

#--------------------------------------------------------------------------------------# 
# Richness ####

qqnorm(HEADER$Specnumber)
qqline(HEADER$Specnumber)
skewness(HEADER$Specnumber)

# Moran's I test performing 
write.table(moran_result) 
Moran.I(HEADER$Specnumber, dists_auto)

mod.richplot <- lmer(Specnumber ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
(anova_results <- anova(mod.richplot, ddf = "Kenward-Roger", type = "III"))
ggResidpanel::resid_panel(mod.richplot, plots = c("resid", "qq", "hist"), nrow = 1)
# Moran.I(x = residuals(mod.richplot), weight = dists_auto)
# write.table(Moran.I(x = residuals(mod.richplot), weight = dists_auto))
# cat_plot(mod.richplot, pred = plot_age, modx = class, interval = TRUE)

# Calculating the effects object
(eff <- effect("plot_age * class", mod.richplot))

# Calculate marginal means
marginal_means_ri <- as.data.frame(effect("plot_age:class", mod.richplot))
marginal_means_ri$plot_age <- factor(marginal_means_ri$plot_age, levels = c("Old", "New"))
marginal_means_ri <- marginal_means_ri[order(marginal_means_ri$class, marginal_means_ri$plot_age), ]
marginal_means_ri$class_age <- paste(marginal_means_ri$class, marginal_means_ri$plot_age, sep=".")
marginal_means_ri$class_age <- factor(marginal_means_ri$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

round(marginal_means_ri$fit,1)
round((marginal_means_ri$fit-marginal_means_ri$se),1)
round((marginal_means_ri$fit+marginal_means_ri$se),1)

print(marginal_means_ri)
round(marginal_means_ri$fit, digits=2)
round(marginal_means_ri$se, digits=2)

means_modelrich <- cbind(marginal_means_ri, parameter = "richness")
means_model <- rbind(means_model, means_modelrich)

# # Checking Specnumber model with PERM and glmer
# set.seed(101)
# p1 <- perm.lmer(Specnumber ~ plot_age * class + PCNM1 + (1 | Old_plot_ID),
#                 buildmerControl = list(direction = "order", crit = "LRT", quiet = TRUE, ddf = "lme4"),
#                 data = HEADER, nperm = 1000, type = "anova")
# p1
# 
# # Poisson regressions with species richness
# mod3 <- glmer(Specnumber ~ plot_age * class + PCNM1 + (1|Old_plot_ID), family=poisson, data=HEADER)
# summary(mod3)
# Anova(mod3, type = "III")
# 
# AIC(mod3);AIC(mod.richplot)
# 
# perm_mod.spec <- mixed(
#   formula = Specnumber ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   method = "KR",  # parametric bootstrap method
#   args_test = list(nsim = 10000),
#   type = 3,  # Type III tests
#   progress = TRUE,  # Show progress bar
#   check_contrasts = TRUE  # Ensure contrasts are set correctly
# )
# 
# # summary(perm_mod.spec)
# anova(perm_mod.spec, ddf = "Kenward-Roger", type = "III")
# 

#--------------------------------------------------------------------------------------#
# Creating a new data frame
results_df <- data.frame(
  `F (1,155)` = anova_results$`F value`,
  `P` = anova_results$`Pr(>F)`,
  # 'significance' = symnum(anova_results$`Pr(>F)`, corr = FALSE, na = FALSE,
  #                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  #                         symbols = c("***", "**", "*", ".", " ")),
  row.names = rownames(anova_results), check.names = F 
)

results_df <- as.data.frame(t(results_df))
results_df$'F, P' <- rownames(results_df)

# Rounding fit and se
means_modelrich$fit <- round(means_modelrich$fit, 2)
means_modelrich$se <- round(means_modelrich$se, 2)

# Creating the "fit ± se" column
means_modelrich$mean_se <- paste0(means_modelrich$fit, " ± ", means_modelrich$se)

# Creating a new frame
result_table <- reshape(
  means_modelrich[, c("plot_age", "class", "mean_se")],
  idvar = "plot_age",
  timevar = "class",
  direction = "wide"
)

# Giving new names to some columns
colnames(result_table) <- c(" ", "FB", "KC")
result_table$` ` <- as.character(result_table$` `)
result_table$` `[which(result_table$` `=="New")] <- "Species_richness_New"
result_table$` `[result_table$` `=="Old"] <- "Species_richness_Old"
results_df_rich <- cbind(result_table, results_df[c('F, P','plot_age', 'class', 'plot_age:class')])

#--------------------------------------------------------------------------------------# 
# Cryptogams richness ####

qqnorm(HEADER$Specnumber.crypt)
qqline(HEADER$Specnumber.crypt)
skewness(HEADER$Specnumber.crypt)
HEADER$Specnumber.crypt <- as.numeric(HEADER$Specnumber.crypt)

# Moran's I test performing 
(moran_result <- Moran.I(HEADER$Specnumber.crypt, dists_auto))
write.table(moran_result) 

mod.richcrplot <- lmer(Specnumber.crypt ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
(anova_results <- anova(mod.richcrplot, ddf = "Kenward-Roger", type = "III"))
ggResidpanel::resid_panel(mod.richcrplot, plots = c("resid", "qq", "hist"), nrow = 1)

# Calculating the effects object
(eff <- effect("plot_age * class", mod.richcrplot))

# Calculating marginal means
marginal_means_cr <- as.data.frame(effect("plot_age:class", mod.richcrplot))
marginal_means_cr$plot_age <- factor(marginal_means_cr$plot_age, levels = c("Old", "New"))
marginal_means_cr <- marginal_means_cr[order(marginal_means_cr$class, marginal_means_cr$plot_age), ]
marginal_means_cr$class_age <- paste(marginal_means_cr$class, marginal_means_cr$plot_age, sep=".")
marginal_means_cr$class_age <- factor(marginal_means_cr$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

print(marginal_means_cr)
round(marginal_means_cr$fit, digits=2)
round(marginal_means_cr$se, digits=2)

round(marginal_means_cr$fit, digits=1)
round(marginal_means_cr$fit-marginal_means_cr$se, digits=1)
round(marginal_means_cr$fit+marginal_means_cr$se, digits=1)


means_modelcr <- cbind(marginal_means_cr, parameter = "richness_crypto")
means_model <- rbind(means_model, means_modelcr)

# # Checking Specnumber.crypt model  
# perm_mod.spec.cr <- mixed(
#   formula = Specnumber.crypt ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   method = "KR",  # parametric bootstrap method
#   args_test = list(nsim = 10000),
#   type = 3,  # Type III tests
#   progress = TRUE,  # Show progress bar
#   check_contrasts = TRUE  # Ensure contrasts are set correctly
# )
# anova(perm_mod.spec.cr, ddf = "Kenward-Roger", type = "III")

#--------------------------------------------------------------------------------------#
# Creating a new data frame
results_df <- data.frame(
  `F (1,155)` = anova_results$`F value`,
  `P` = anova_results$`Pr(>F)`,
  # 'significance' = symnum(anova_results$`Pr(>F)`, corr = FALSE, na = FALSE,
  #                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  #                         symbols = c("***", "**", "*", ".", " ")),
  row.names = rownames(anova_results), check.names = F 
)

results_df <- as.data.frame(t(results_df))
results_df$'F, P' <- rownames(results_df)

# Rounding fit and se
means_modelcr$fit <- round(means_modelcr$fit, 2)
means_modelcr$se <- round(means_modelcr$se, 2)

# Creating the "fit ± se" column
means_modelcr$mean_se <- paste0(means_modelcr$fit, " ± ", means_modelcr$se)

# Creating a new frame
result_table <- reshape(
  means_modelcr[, c("plot_age", "class", "mean_se")],
  idvar = "plot_age",
  timevar = "class",
  direction = "wide"
)

# Giving new names to some columns
colnames(result_table) <- c(" ", "FB", "KC")
result_table$` ` <- as.character(result_table$` `)
result_table$` `[which(result_table$` `=="New")] <- "Cryptogam number per plot_New"
result_table$` `[result_table$` `=="Old"] <- "Cryptogam number per plot_Old"

results_df_cr <- cbind(result_table, results_df[c('F, P','plot_age', 'class', 'plot_age:class')])

results_df_all <- rbind(results_df_all, results_df_cr)


#--------------------------------------------------------------------------------------# 
# 
# # cryptogam richness per class
# 
# # FB
# mod.Specnumber.cryptF <- lmer(Specnumber.crypt ~ plot_age + PCNM1 + (1|Old_plot_ID), data = only_FB)
# anova(mod.Specnumber.cryptF, ddf = "Kenward-Roger", type = "II")
# plot(mod.Specnumber.cryptF)
# 
# # KC
# mod.Specnumber.cryptK <- lmer(Specnumber.crypt ~ plot_age + PCNM1 + (1|Old_plot_ID), data = only_KC)
# anova(mod.Specnumber.cryptK, ddf = "Kenward-Roger", type = "II")
# plot(mod.Specnumber.cryptK)
# 
# boxplot(Specnumber.crypt ~ plot_age* class, data=HEADER,
#         las=1, ylab="Species number cryptogam", xlab="Plot group", col=c("grey", "forestgreen"), ylim=c(0,17))

#--------------------------------------------------------------------------------------# 
# Phanero richness ####

qqnorm(HEADER$Specnumber.phan)
qqline(HEADER$Specnumber.phan)
skewness(HEADER$Specnumber.phan)

# Moran's I test performing 
(moran_result <- Moran.I(HEADER$Specnumber.phan, dists_auto))
write.table(moran_result) 
                                        
mod.rich.ph <- lmer(Specnumber.phan ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova(mod.rich.ph, ddf = "Kenward-Roger", type = "III")
ggResidpanel::resid_panel(mod.rich.ph, plots = c("resid", "qq", "hist"), nrow = 1)

check_collinearity(mod.rich.ph) # if collinearity is high, we'll check  additive effects only model
plot(resid(mod.rich.ph))

# Calculating the effects object
(eff <- effect("plot_age * class", mod.rich.ph))

# Calculating marginal means
marginal_meansphan <- as.data.frame(effect("plot_age * class", mod.rich.ph))
marginal_meansphan$plot_age <- factor(marginal_meansphan$plot_age, levels = c("Old", "New"))
marginal_meansphan <- marginal_meansphan[order(marginal_meansphan$class, marginal_meansphan$plot_age), ]
marginal_meansphan$class_age <- paste(marginal_meansphan$class, marginal_meansphan$plot_age, sep=".")
marginal_meansphan$class_age <- factor(marginal_meansphan$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

# View the marginal means
print(marginal_meansphan)
round(marginal_meansphan$fit, digits=1)
round(marginal_meansphan$fit-marginal_meansphan$se, digits=1)
round(marginal_meansphan$fit+marginal_meansphan$se, digits=1)

means_modelPhan <- cbind(marginal_meansphan, parameter = "richness_phanero")
means_model <- rbind(means_model, means_modelPhan)

# # Checking Specnumber.phan model with PERM
# set.seed(101)
# p1ph <- perm.lmer(Specnumber.phan ~ plot_age * class + PCNM1 + (1 |Old_plot_ID),
#                   buildmerControl = list(direction = "order", crit = "LRT", quiet = TRUE, ddf = "lme4"),
#                   data = HEADER, nperm = 1000, type = "anova")
# p1ph
# 
# perm_mod.spec.ph <- mixed(
#   formula = Specnumber.phan ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   method = "KR",  # parametric bootstrap method
#   args_test = list(nsim = 10000),
#   type = 3,  # Type III tests
#   progress = TRUE,  # Show progress bar
#   check_contrasts = TRUE  # Ensure contrasts are set correctly
# )
# anova(perm_mod.spec.ph, ddf = "Kenward-Roger", type = "III")

#--------------------------------------------------------------------------------------#
# Creating a new data frame
results_df <- data.frame(
  `F (1,155)` = anova_results$`F value`,
  `P` = anova_results$`Pr(>F)`,
  # 'significance' = symnum(anova_results$`Pr(>F)`, corr = FALSE, na = FALSE,
  #                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  #                         symbols = c("***", "**", "*", ".", " ")),
  row.names = rownames(anova_results), check.names = F 
)

results_df <- as.data.frame(t(results_df))
results_df$'F, P' <- rownames(results_df)

# Rounding fit and se
means_modelPhan$fit <- round(means_modelPhan$fit, 2)
means_modelPhan$se <- round(means_modelPhan$se, 2)

# Creating the "fit ± se" column
means_modelPhan$mean_se <- paste0(means_modelPhan$fit, " ± ", means_modelPhan$se)

# Creating a new frame
result_table <- reshape(
  means_modelPhan[, c("plot_age", "class", "mean_se")],
  idvar = "plot_age",
  timevar = "class",
  direction = "wide"
)
# Giving new names to some columns
colnames(result_table) <- c(" ", "FB", "KC")
result_table$` ` <- as.character(result_table$` `)
result_table$` `[which(result_table$` `=="New")] <- "Phanerogam number per plot_New"
result_table$` `[result_table$` `=="Old"] <- "Phanerogam number per plot_Old"
results_df_ph <- cbind(result_table, results_df[c('F, P','plot_age', 'class', 'plot_age:class')])


#
results_df_all <- rbind(results_df_all, results_df_ph)
#--------------------------------------------------------------------------------------# 
#--------------------------------------------------------------------------------------#
# MEAN ELLENBERG MODELS ####
#--------------------------------------------------------------------------------------# 
# L_mean ####

(moran_result <- Moran.I(HEADER$L_mean, dists_auto))
write.table(moran_result) 

qqnorm(HEADER$L_mean)
qqline(HEADER$L_mean)
any(is.na(HEADER$L_mean))
skewness(HEADER$L_mean)

# L_mean (simple model)
mod.L_mean <- lmer(L_mean ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova_results <- anova(mod.L_mean, ddf = "Kenward-Roger", type = "III")
anova_results
plot(mod.L_mean)
plot(allEffects(mod.L_mean))

boxplot(L_mean ~ plot_age * class, col="lightyellow", data=HEADER)
ggResidpanel::resid_panel(mod.L_mean, plots = c("resid", "qq", "hist"), nrow = 1)

mod.L_meanF <- lmer(L_mean ~ plot_age + PCNM1 + (1|Old_plot_ID), data = only_FB)
anova(mod.L_meanF, ddf = "Kenward-Roger", type = "III")
plot(mod.L_meanF)

mod.L_meanK <- lmer(L_mean ~ plot_age + PCNM1 + (1|Old_plot_ID), data = only_KC)
anova(mod.L_meanK, ddf = "Kenward-Roger", type = "III")
plot(mod.L_meanK)

check_collinearity(mod.L_mean) # if high colinearity so we'll check the additive effects only model
plot(resid(mod.L_mean))

leastsquare = emmeans(mod.L_mean,
                      pairwise ~ plot_age:class,
                      adjust = "tukey")
cld(leastsquare$contrasts)

# Calculating the effects object
(eff <- effect("plot_age * class", mod.L_mean))

# Calculating marginal means
marginal_means_li <- as.data.frame(effect("plot_age * class", mod.L_mean))
marginal_means_li$plot_age <- factor(marginal_means_li$plot_age, levels = c("Old", "New"))
marginal_means_li <- marginal_means_li[order(marginal_means_li$class, marginal_means_li$plot_age), ]
marginal_means_li$class_age <- paste(marginal_means_li$class, marginal_means_li$plot_age, sep=".")
marginal_means_li$class_age <- factor(marginal_means_li$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

round(marginal_means_li$fit, digits=2)
round(marginal_means_li$se, digits=2)

round(marginal_means_li$fit, 2)
round((marginal_means_li$fit-marginal_means_li$se),2)
round((marginal_means_li$fit+marginal_means_li$se),2)

means_modelL <- cbind(marginal_means_li, parameter = "L_mean")
means_model <- rbind(means_model, means_modelL)

#--------------------------------------------------------------------------------------#

# Creating a new data frame
results_df <- data.frame(
  `F (1,155)` = anova_results$`F value`,
  `P` = anova_results$`Pr(>F)`,
  # 'significance' = symnum(anova_results$`Pr(>F)`, corr = FALSE, na = FALSE,
  #                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  #                         symbols = c("***", "**", "*", ".", " ")),
  row.names = rownames(anova_results), check.names = F 
)

results_df <- as.data.frame(t(results_df))
results_df$'F, P' <- rownames(results_df)

# Rounding fit and se
marginal_means_li$fit <- round(marginal_means_li$fit, 2)
marginal_means_li$se <- round(marginal_means_li$se, 2)

# Creating the "fit ± se" column
marginal_means_li$mean_se <- paste0(marginal_means_li$fit, " ± ", marginal_means_li$se)

# Creating a new frame
result_table <- reshape(
  marginal_means_li[, c("plot_age", "class", "mean_se")],
  idvar = "plot_age",
  timevar = "class",
  direction = "wide"
)

# Giving new names to some columns
colnames(result_table) <- c(" ", "FB", "KC")
result_table$` ` <- as.character(result_table$` `)
result_table$` `[which(result_table$` `=="New")] <- "Light (mean)_New"
result_table$` `[result_table$` `=="Old"] <- "Light (mean)_Old"
results_df_l_mean <- cbind(result_table, results_df[c('F, P','plot_age', 'class', 'plot_age:class')])

#
results_df_all <- rbind(results_df_all, results_df_l_mean)

#--------------------------------------------------------------------------------------#

# # Checking model with PERM
# perm_mod.L_mean <- mixed(
#   formula = L_mean ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   per_parameter = 10000,
#   progress = interactive(),
#   return = "mixed"
# )
# anova(perm_mod.L_mean, ddf = "Kenward-Roger", type = "III")
# 
#--------------------------------------------------------------------------------------# 
# T_mean ####

(moran_result <- Moran.I(HEADER$T_mean, dists_auto))
write.table(moran_result) 

qqnorm(HEADER$T_mean)
qqline(HEADER$T_mean)
skewness(HEADER$T_mean)

mod.T_mean <- lmer(T_mean ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova_results <- anova(mod.T_mean, ddf = "Kenward-Roger", type = "III")
anova_results
plot(mod.T_mean)
boxplot(T_mean ~ plot_age * class, col="lightyellow", data=HEADER)
ggResidpanel::resid_panel(mod.T_mean, plots = c("resid", "qq", "hist"), nrow = 1)

mod.T_meanK <- lmer(T_mean ~ plot_age  + PCNM1 + (1|Old_plot_ID), data = only_KC)
anova(mod.T_meanK, ddf = "Kenward-Roger", type = "III")
plot(mod.T_meanK)

mod.T_meanF <- lmer(T_mean ~ plot_age  + PCNM1 + (1|Old_plot_ID), data = only_FB)
anova(mod.T_meanF, ddf = "Kenward-Roger", type = "III")
plot(mod.T_meanF)

check_collinearity(mod.T_mean)
plot(resid(mod.T_mean))
plot(allEffects(mod.T_mean))

leastsquare = emmeans(mod.T_mean,
                      pairwise ~ plot_age:class,
                      adjust = "tukey")
cld(leastsquare$contrasts)

# Calculating the effects object
(eff <- effect("plot_age * class", mod.T_mean))

# Calculating marginal means
marginal_means_te <- as.data.frame(effect("plot_age * class", mod.T_mean))
marginal_means_te$plot_age <- factor(marginal_means_te$plot_age, levels = c("Old", "New"))
marginal_means_te <- marginal_means_te[order(marginal_means_te$class, marginal_means_te$plot_age), ]
marginal_means_te$class_age <- paste(marginal_means_te$class, marginal_means_te$plot_age, sep=".")
marginal_means_te$class_age <- factor(marginal_means_te$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))
# View the marginal means
print(marginal_means_te)

round(marginal_means_te$fit, digits=2)
round(marginal_means_te$se, digits=2)

round(marginal_means_te$fit, 2)
round((marginal_means_te$fit-marginal_means_te$se),2)
round((marginal_means_te$fit+marginal_means_te$se),2)

means_modelT <- cbind(marginal_means_te, parameter = "T_mean")
means_model <- rbind(means_model, means_modelT)

#-------------------------------------------------------------------------------------#

# Creating a new data frame
results_df <- data.frame(
  `F (1,155)` = anova_results$`F value`,
  `P` = anova_results$`Pr(>F)`,
  # 'significance' = symnum(anova_results$`Pr(>F)`, corr = FALSE, na = FALSE,
  #                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  #                         symbols = c("***", "**", "*", ".", " ")),
  row.names = rownames(anova_results), check.names = F 
)

results_df <- as.data.frame(t(results_df))
results_df$'F, P' <- rownames(results_df)

# Rounding fit and se
marginal_means_te$fit <- round(marginal_means_te$fit, 2)
marginal_means_te$se <- round(marginal_means_te$se, 2)

# Creating the "fit ± se" column
marginal_means_te$mean_se <- paste0(marginal_means_te$fit, " ± ", marginal_means_te$se)

# Creating a new frame
result_table <- reshape(
  marginal_means_te[, c("plot_age", "class", "mean_se")],
  idvar = "plot_age",
  timevar = "class",
  direction = "wide"
)
# Giving new names to some columns
colnames(result_table) <- c(" ", "FB", "KC")
result_table$` ` <- as.character(result_table$` `)
result_table$` `[which(result_table$` `=="New")] <- "Temperature (mean)_New"
result_table$` `[result_table$` `=="Old"] <- "Temperature (mean)_Old"
results_df_t_mean <- cbind(result_table, results_df[c('F, P','plot_age', 'class', 'plot_age:class')])


#
results_df_all <- rbind(results_df_all, results_df_t_mean)

#-------------------------------------------------------------------------------------#

# # Checking model with PERM
# perm_mod.T_mean <- mixed(
#   formula = T_mean ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   per_parameter = 10000,
#   progress = interactive(),
#   return = "mixed"
# )
# anova(perm_mod.T_mean, ddf = "Kenward-Roger", type = "III")

#--------------------------------------------------------------------------------------# 
# M_mean ####

(moran_result <- Moran.I(HEADER$M_mean, dists_auto))
write.table(moran_result) 

qqnorm(HEADER$M_mean)
qqline(HEADER$M_mean)
skewness(HEADER$M_mean)

mod.M_mean <- lmer(M_mean ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova_results <- anova(mod.M_mean, ddf = "Kenward-Roger", type = "III")
anova_results
plot(mod.M_mean)
ggResidpanel::resid_panel(mod.M_mean, plots = c("resid", "qq", "hist"), nrow = 1)

check_collinearity(mod.M_mean)
plot(resid(mod.M_mean))

# Calculating the effects object
(eff <- effect("plot_age * class", mod.M_mean))

# Calculating marginal means
marginal_meansm <- as.data.frame(effect("plot_age * class", mod.M_mean))

marginal_meansm$plot_age <- factor(marginal_meansm$plot_age, levels = c("Old", "New"))
marginal_meansm <- marginal_meansm[order(marginal_meansm$class, marginal_meansm$plot_age), ]
marginal_meansm$class_age <- paste(marginal_meansm$class, marginal_meansm$plot_age, sep=".")
marginal_meansm$class_age <- factor(marginal_meansm$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

print(marginal_meansm)

round(marginal_meansm$fit, digits=2)
round(marginal_meansm$se, digits=2)

round(marginal_meansm$fit, 2)
round((marginal_meansm$fit-marginal_meansm$se),2)
round((marginal_meansm$fit+marginal_meansm$se),2)

means_modelM <- cbind(marginal_meansm, parameter = "M_mean")
means_model <- rbind(means_model, means_modelM)

#------------------------------------------------------------------------------------#

# Creating a new data frame
results_df <- data.frame(
  `F (1,155)` = anova_results$`F value`,
  `P` = anova_results$`Pr(>F)`,
  # 'significance' = symnum(anova_results$`Pr(>F)`, corr = FALSE, na = FALSE,
  #                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  #                         symbols = c("***", "**", "*", ".", " ")),
  row.names = rownames(anova_results), check.names = F 
)

results_df <- as.data.frame(t(results_df))
results_df$'F, P' <- rownames(results_df)

# Rounding fit and se
marginal_meansm$fit <- round(marginal_meansm$fit, 2)
marginal_meansm$se <- round(marginal_meansm$se, 2)

# Creating the "fit ± se" column
marginal_meansm$mean_se <- paste0(marginal_meansm$fit, " ± ", marginal_meansm$se)

# Creating a new frame
result_table <- reshape(
  marginal_meansm[, c("plot_age", "class", "mean_se")],
  idvar = "plot_age",
  timevar = "class",
  direction = "wide"
)
# Giving new names to some columns
colnames(result_table) <- c(" ", "FB", "KC")
result_table$` ` <- as.character(result_table$` `)
result_table$` `[which(result_table$` `=="New")] <- "M (mean)_New"
result_table$` `[result_table$` `=="Old"] <- "M (mean)_Old"
results_df_m_mean <- cbind(result_table, results_df[c('F, P','plot_age', 'class', 'plot_age:class')])

#
results_df_all <- rbind(results_df_all, results_df_m_mean)

#------------------------------------------------------------------------------------#
# # Checking model  
# mod.M_meanK <- lmer(M_mean ~ plot_age + PCNM1 +(1|Old_plot_ID), data = only_KC)
# anova(mod.M_meanK)
# plot(mod.M_meanK)
# 
# mod.M_meanF <- lmer(M_mean ~ plot_age + PCNM1 + (1|Old_plot_ID), data = only_FB)
# anova(mod.M_meanF)
# plot(mod.M_meanF)
# 
# perm_mod.M_mean <- mixed(
#   formula = M_mean ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   per_parameter = 10000,
#   progress = interactive(),
#   return = "mixed"
# )
# 
# anova(perm_mod.M_mean, ddf = "Kenward-Roger", type = "III")

#--------------------------------------------------------------------------------------# 
# R_mean ####

(moran_result <- Moran.I(HEADER$R_mean, dists_auto))
write.table(moran_result) 

qqnorm(HEADER$R_mean)
qqline(HEADER$R_mean)
skewness(HEADER$R_mean)

mod.R_mean <- lmer(R_mean ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova_results <- anova(mod.R_mean, ddf = "Kenward-Roger", type = "III")
anova_results
plot(mod.R_mean)
check_collinearity(mod.R_mean)
plot(resid(mod.R_mean))
ggResidpanel::resid_panel(mod.R_mean, plots = c("resid", "qq", "hist"), nrow = 1)

# Calculating the effects object
(eff <- effect("plot_age * class", mod.R_mean))

# Calculating marginal means
marginal_meansR <- as.data.frame(effect("plot_age * class", mod.R_mean))
marginal_meansR$plot_age <- factor(marginal_meansR$plot_age, levels = c("Old", "New"))
marginal_meansR <- marginal_meansR[order(marginal_meansR$class, marginal_meansR$plot_age), ]
marginal_meansR$class_age <- paste(marginal_meansR$class, marginal_meansR$plot_age, sep=".")
marginal_meansR$class_age <- factor(marginal_meansR$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

print(marginal_meansR)
round(marginal_meansR$fit, digits=2)
round(marginal_meansR$se, digits=2)

round(marginal_meansR$fit, 2)
round((marginal_meansR$fit-marginal_meansR$se),2)
round((marginal_meansR$fit+marginal_meansR$se),2)

means_modelR <- cbind(marginal_meansR, parameter = "R_mean")
means_model <- rbind(means_model, means_modelR)

# # Checking model with PERM
# perm_mod.R_mean <- mixed(
#   formula = R_mean ~ plot_age * class + PCNM1 +  (1|Old_plot_ID),
#   data = HEADER,
#   per_parameter = 10000,
#   progress = interactive(),
#   return = "mixed"
# )
# anova(perm_mod.R_mean, ddf = "Kenward-Roger", type = "III")

#--------------------------------------------------------------------------------------#

# Creating a new data frame
results_df <- data.frame(
  `F (1,155)` = anova_results$`F value`,
  `P` = anova_results$`Pr(>F)`,
  # 'significance' = symnum(anova_results$`Pr(>F)`, corr = FALSE, na = FALSE,
  #                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  #                         symbols = c("***", "**", "*", ".", " ")),
  row.names = rownames(anova_results), check.names = F 
)

results_df <- as.data.frame(t(results_df))
results_df$'F, P' <- rownames(results_df)

# Rounding fit and se
marginal_meansR$fit <- round(marginal_meansR$fit, 2)
marginal_meansR$se <- round(marginal_meansR$se, 2)

# Creating the "fit ± se" column
marginal_meansR$mean_se <- paste0(marginal_meansR$fit, " ± ", marginal_meansR$se)

# Creating a new frame
result_table <- reshape(
  marginal_meansR[, c("plot_age", "class", "mean_se")],
  idvar = "plot_age",
  timevar = "class",
  direction = "wide"
)
# Giving new names to some columns
colnames(result_table) <- c(" ", "FB", "KC")
result_table$` ` <- as.character(result_table$` `)
result_table$` `[which(result_table$` `=="New")] <- "Reaction (R mean)_New"
result_table$` `[result_table$` `=="Old"] <- "Reaction (R mean)_Old"
results_df_R_mean <- cbind(result_table, results_df[c('F, P','plot_age', 'class', 'plot_age:class')])

#
results_df_all <- rbind(results_df_all, results_df_R_mean)

#--------------------------------------------------------------------------------------# 
# N_mean ####

(moran_result <- Moran.I(HEADER$N_mean, dists_auto))
write.table(moran_result) 

qqnorm(HEADER$N_mean)
qqline(HEADER$N_mean)
skewness(HEADER$N_mean)

mod.N_mean <- lmer(N_mean ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova_results <- anova(mod.N_mean, ddf = "Kenward-Roger", type = "III")
anova_results
plot(mod.N_mean)
ggResidpanel::resid_panel(mod.N_mean, plots = c("resid", "qq", "hist"), nrow = 1)

check_collinearity(mod.N_mean)
plot(resid(mod.N_mean))

# Calculating the effects object
eff <- effect("plot_age * class", mod.N_mean)

# Calculating marginal means
marginal_meansN <- as.data.frame(effect("plot_age * class", mod.N_mean))
marginal_meansN$plot_age <- factor(marginal_meansN$plot_age, levels = c("Old", "New"))
marginal_meansN <- marginal_meansN[order(marginal_meansN$class, marginal_meansN$plot_age), ]
marginal_meansN$class_age <- paste(marginal_meansN$class, marginal_meansN$plot_age, sep=".")
marginal_meansN$class_age <- factor(marginal_meansN$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

print(marginal_meansN)
round(marginal_meansN$fit, digits=2)
round(marginal_meansN$se, digits=2)

round(marginal_meansN$fit, 2)
round((marginal_meansN$fit-marginal_meansN$se),2)
round((marginal_meansN$fit+marginal_meansN$se),2)

means_modelNN <- cbind(marginal_meansN, parameter = "N_mean")
means_model <- rbind(means_model, means_modelNN)

# # Checking model with PERM
# perm_mod.N_mean <- mixed(
#   formula = N_mean ~ plot_age * class + PCNM1 +  (1|Old_plot_ID),
#   data = HEADER,
#   per_parameter = 10000,
#   progress = interactive(),
#   return = "mixed"
# )
# 
# anova(perm_mod.N_mean, ddf = "Kenward-Roger", type = "III")

#--------------------------------------------------------------------------------------# 
#--------------------------------------------------------------------------------------# 
#--------------------------------------------------------------------------------------# 
# All red book species ####
(moran_result <- Moran.I(HEADER$red_sp, dists_auto))
write.table(moran_result) 

qqnorm(HEADER$red_sp)
qqline(HEADER$red_sp)
skewness(HEADER$red_sp)

mod.red_sp <- lmer(red_sp ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova_results <- anova(mod.red_sp, ddf = "Kenward-Roger", type = "III")
anova_results
plot(mod.red_sp)
ggResidpanel::resid_panel(mod.red_sp, plots = c("resid", "qq", "hist"), nrow = 1)

# Calculating the effects object
eff_RS_without_lichens <- effect("plot_age * class", mod.red_sp)
eff_RS_without_lichens

# Calculating marginal means
marginal_means.red_sp <- as.data.frame(effect("plot_age * class", mod.red_sp))
marginal_means.red_sp$plot_age <- factor(marginal_means.red_sp$plot_age, levels = c("Old", "New"))
marginal_means.red_sp <- marginal_means.red_sp[order(marginal_means.red_sp$class, marginal_means.red_sp$plot_age), ]
marginal_means.red_sp$class_age <- paste(marginal_means.red_sp$class, marginal_means.red_sp$plot_age, sep=".")
marginal_means.red_sp$class_age <- factor(marginal_means.red_sp$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

print(marginal_means.red_sp)
round(marginal_means.red_sp$fit, digits=1)
round(marginal_means.red_sp$se, digits=1)

round(marginal_means.red_sp$fit, digits= 1)
round((marginal_means.red_sp$fit-marginal_means.red_sp$se),digits=1)
round((marginal_means.red_sp$fit+marginal_means.red_sp$se),digits=1)

marginal_meansalred <- cbind(marginal_means.red_sp, parameter = "red_sp")
means_model <- rbind(means_model, marginal_meansalred)

# # Checking model with PERM
# perm_mod.red_sp <- mixed(
#   formula = red_sp ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   per_parameter = 10000,
#   progress = interactive(),
#   return = "mixed"
# )
# 
# anova(perm_mod.red_sp, ddf = "Kenward-Roger", type = "III")
#--------------------------------------------------------------------------------------#

# Creating a new data frame
results_df <- data.frame(
  `F (1,155)` = anova_results$`F value`,
  `P` = anova_results$`Pr(>F)`,
  # 'significance' = symnum(anova_results$`Pr(>F)`, corr = FALSE, na = FALSE,
  #                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
  #                         symbols = c("***", "**", "*", ".", " ")),
  row.names = rownames(anova_results), check.names = F 
)

results_df <- as.data.frame(t(results_df))
results_df$'F, P' <- rownames(results_df)

# Rounding fit and se
marginal_means.red_sp$fit <- round(marginal_means.red_sp$fit, 2)
marginal_means.red_sp$se <- round(marginal_means.red_sp$se, 2)

# Creating the "fit ± se" column
marginal_means.red_sp$mean_se <- paste0(marginal_means.red_sp$fit, " ± ", marginal_means.red_sp$se)

# Creating a new frame
result_table <- reshape(
  marginal_means.red_sp[, c("plot_age", "class", "mean_se")],
  idvar = "plot_age",
  timevar = "class",
  direction = "wide"
)

# Giving new names to some columns
colnames(result_table) <- c(" ", "FB", "KC")
result_table$` ` <- as.character(result_table$` `)
result_table$` `[which(result_table$` `=="New")] <- "Red species_New"
result_table$` `[result_table$` `=="Old"] <- "Red species_Old"
results_df_red <- cbind(result_table, results_df[c('F, P','plot_age', 'class', 'plot_age:class')])

#
results_df_all <- rbind(results_df_all, results_df_red)


#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------# 
# red book cryptogam species ####

# creating a red cryptogam column
HEADER$red_crypt <- NA
for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  sum_values <- 0  # start value of red sp per plot
  for (j in 1:nrow(TABLO)) {
    if (!is.na(TABLO[j, col]) && !is.na(SPP_info[j, "red_cat"]) && TABLO[j, col] != "0" && SPP_info[j,]$life_form == "cryptogam") {
      sum_values <- sum_values + 1  # plus 1 if value is not "0" and do not NA
    }
  }
  HEADER$red_crypt[match(col, HEADER$New_plot_ID)] <- sum_values
}

# modelling
qqnorm(HEADER$red_crypt)
qqline(HEADER$red_crypt)
skewness(HEADER$red_crypt)

mod.red_cr <- lmer(red_crypt ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova_results <- anova(mod.red_cr, ddf = "Kenward-Roger", type = "III")
anova_results
plot(mod.red_cr)
ggResidpanel::resid_panel(mod.red_cr, plots = c("resid", "qq", "hist"), nrow = 1)

# Calculating the effects object
eff_RCrS_without_lich <- effect("plot_age * class", mod.red_cr)

# Calculating marginal means
marginal_means.red_spcr <- as.data.frame(effect("plot_age * class", mod.red_cr))
marginal_means.red_spcr$plot_age <- factor(marginal_means.red_spcr$plot_age, levels = c("Old", "New"))
marginal_means.red_spcr <- marginal_means.red_spcr[order(marginal_means.red_spcr$class, marginal_means.red_spcr$plot_age), ]
marginal_means.red_spcr$class_age <- paste(marginal_means.red_spcr$class, marginal_means.red_spcr$plot_age, sep=".")
marginal_means.red_spcr$class_age <- factor(marginal_means.red_spcr$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

print(marginal_means.red_spcr)
round(marginal_means.red_spcr$fit, digits=2)
round(marginal_means.red_spcr$se, digits=2)

round(marginal_means.red_spcr$fit, digits=1)
round(marginal_means.red_spcr$fit-marginal_means.red_spcr$se, digits=1)
round(marginal_means.red_spcr$fit+marginal_means.red_spcr$se, digits=1)

marginal_meansalredd <- cbind(marginal_means.red_spcr, parameter = "red_sp_cr")
means_model <- rbind(means_model, marginal_meansalredd)

# # Checking model with PERM
# set.seed(111)
# p.red.cry <- perm.lmer(red_crypt ~ plot_age * class + PCNM1 + (1 |Old_plot_ID),
#                    buildmerControl = list(direction = "order", crit = "LRT", quiet = TRUE, ddf = "lme4"),
#                    data = HEADER, nperm = 1000, type = "anova")
# p.red.cry
# 
# perm_mod.red.cr <- mixed(
#   formula = red_crypt ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   per_parameter = 10000,
#   progress = interactive(),
#   return = "mixed"
# )
# 
# anova(perm_mod.red.cr, ddf = "Kenward-Roger", type = "III")



#--------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------# 
# red book phanerogam species ####

# creating a red phanerogam column
HEADER$red_phanerogam <- NA
for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  sum_values <- 0  # start value of red sp per plot
  for (j in 1:nrow(TABLO)) {
    if (!is.na(TABLO[j, col]) && !is.na(SPP_info[j, "red_cat"]) && TABLO[j, col] != "0" && SPP_info[j,]$life_form !="cryptogam") {
      sum_values <- sum_values + 1  # plus 1 if value is not "0" and do not NA
    }
  }
  HEADER$red_phanerogam[match(col, HEADER$New_plot_ID)] <- sum_values
}

# modeling
qqnorm(HEADER$red_phanerogam)
qqline(HEADER$red_phanerogam)
skewness(HEADER$red_phanerogam)

mod.red_ph <- lmer(red_phanerogam ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova(mod.red_ph, ddf = "Kenward-Roger", type = "III")
plot(mod.red_ph)
ggResidpanel::resid_panel(mod.red_ph, plots = c("resid", "qq", "hist"), nrow = 1)

# Calculating the effects object
eff <- effect("plot_age * class", mod.red_ph)

# Calculating marginal means
marginal_means.red_spph <- as.data.frame(effect("plot_age * class", mod.red_ph))
marginal_means.red_spph$plot_age <- factor(marginal_means.red_spph$plot_age, levels = c("Old", "New"))
marginal_means.red_spph <- marginal_means.red_spph[order(marginal_means.red_spph$class, marginal_means.red_spph$plot_age), ]
marginal_means.red_spph$class_age <- paste(marginal_means.red_spph$class, marginal_means.red_spph$plot_age, sep=".")
marginal_means.red_spph$class_age <- factor(marginal_means.red_spph$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

print(marginal_means.red_spph)
round(marginal_means.red_spph$fit, digits=1)
round(marginal_means.red_spph$fit-marginal_means.red_spph$se, digits=1)
round(marginal_means.red_spph$fit+marginal_means.red_spph$se, digits=1)

marginal_meansalred <- cbind(marginal_means.red_spph, parameter = "red_sp_ph")
means_model <- rbind(means_model, marginal_meansalred)

# # Checking model with PERM
# set.seed(111)
# p.red_phanerogam <- perm.lmer(red_phanerogam ~ plot_age * class + PCNM1 + (1 |Old_plot_ID),
#                        buildmerControl = list(direction = "order", crit = "LRT", quiet = TRUE, ddf = "lme4"),
#                        data = HEADER, nperm = 1000, type = "anova")
# p.red_phanerogam
# 
# perm_mod.red_phanerogam <- mixed(
#   formula = red_phanerogam ~ plot_age * class + PCNM1 + (1|Old_plot_ID),
#   data = HEADER,
#   per_parameter = 10000,
#   progress = interactive(),
#   return = "mixed"
# )
# 
# anova(perm_mod.red_phanerogam, ddf = "Kenward-Roger", type = "III")

#--------------------------------------------------------------------------------------# 
#-------------------------------------------------------------------------------------# 
# disturb_sev_herb_mean ####
(moran_result <- Moran.I(HEADER$disturb_sev_herb_mean, dists_auto))
write.table(moran_result) 
HEADER$disturb_sev_herb_mean

mod.disturb_sev_herb_mean <- lmer(disturb_sev_herb_mean ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova_results_disturb_sev <- anova(mod.disturb_sev_herb_mean, ddf = "Kenward-Roger", type = "III")
anova_results_disturb_sev
plot(mod.disturb_sev_herb_mean)
ggResidpanel::resid_panel(mod.disturb_sev_herb_mean, plots = c("resid", "qq", "hist"), nrow = 1)

# Calculating marginal means
marginal_disturb_sev_herb_mean <- as.data.frame(effect("plot_age * class", mod.disturb_sev_herb_mean))
marginal_disturb_sev_herb_mean$plot_age <- factor(marginal_disturb_sev_herb_mean$plot_age, levels = c("Old", "New"))
marginal_disturb_sev_herb_mean <- marginal_disturb_sev_herb_mean[order(marginal_disturb_sev_herb_mean$class, marginal_disturb_sev_herb_mean$plot_age), ]
marginal_disturb_sev_herb_mean$class_age <- paste(marginal_disturb_sev_herb_mean$class, marginal_disturb_sev_herb_mean$plot_age, sep=".")
marginal_disturb_sev_herb_mean$class_age <- factor(marginal_disturb_sev_herb_mean$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

round(marginal_disturb_sev_herb_mean$fit,2)
round((marginal_disturb_sev_herb_mean$fit-marginal_disturb_sev_herb_mean$se),2)
round((marginal_disturb_sev_herb_mean$fit+marginal_disturb_sev_herb_mean$se),2)

print(marginal_disturb_sev_herb_mean)
round(marginal_disturb_sev_herb_mean$fit, digits=2)
round(marginal_disturb_sev_herb_mean$se, digits=2)

marginal_disturb_sev_herb_meanA <- cbind(marginal_disturb_sev_herb_mean, parameter = "disturb_sev_herb_mean")
means_model <- rbind(means_model, marginal_disturb_sev_herb_meanA)

mod.disturb_sev_herb_meanFB <- lmer(disturb_sev_herb_mean ~ plot_age + PCNM1 + (1|Old_plot_ID), data = only_FB)
anova(mod.disturb_sev_herb_meanFB, ddf = "Kenward-Roger", type = "III")

mod.disturb_sev_herb_meanKC <- lmer(disturb_sev_herb_mean ~ plot_age + PCNM1 + (1|Old_plot_ID), data = only_KC)
anova(mod.disturb_sev_herb_meanKC, ddf = "Kenward-Roger", type = "III")

# saveRDS(means_model, "means_model.rds")

# readRDS("means_model.rds")
# means_model_without <- readRDS("means_model_without_weissia.rds")

#----------------------------------------------------------------#

# SLA correct ####
(moran_result <- Moran.I(HEADER$sla_cwm, dists_auto))
write.table(moran_result) 

mod_sla_cwm <- lmer(sla_cwm ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova_results_sla_cwm <- anova(mod_sla_cwm, ddf = "Kenward-Roger", type = "III")
anova_results_sla_cwm
plot(mod_sla_cwm)
check_collinearity(mod_sla_cwm)
plot(resid(mod_sla_cwm))
plot(allEffects(mod_sla_cwm))
ggResidpanel::resid_panel(mod_sla_cwm, plots = c("resid", "qq", "hist"), nrow = 1)

mod.SLA_meanK <- lmer(sla_cwm ~ plot_age  + PCNM1 + (1|Old_plot_ID), data = only_KC)
anova(mod.SLA_meanK, ddf = "Kenward-Roger", type = "III")
plot(mod.SLA_meanK)

mod.SLA_meanF <- lmer(sla_cwm ~ plot_age  + PCNM1 + (1|Old_plot_ID), data = only_FB)
anova(mod.SLA_meanF, ddf = "Kenward-Roger", type = "III")
plot(mod.SLA_meanF)


# Calculating marginal means
marginal_sla_cwm <- as.data.frame(effect("plot_age * class", mod_sla_cwm))
marginal_sla_cwm$plot_age <- factor(marginal_sla_cwm$plot_age, levels = c("Old", "New"))
marginal_sla_cwm <- marginal_sla_cwm[order(marginal_sla_cwm$class, 
                                           marginal_sla_cwm$plot_age), ]
marginal_sla_cwm$class_age <- paste(marginal_sla_cwm$class, 
                                    marginal_sla_cwm$plot_age, sep=".")
marginal_sla_cwm$class_age <- factor(marginal_sla_cwm$class_age, 
                                     levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

print(marginal_sla_cwm)
round(marginal_sla_cwm$fit, digits=1)
round(marginal_sla_cwm$se, digits=1)

round(marginal_sla_cwm$fit, digits=1)
round((marginal_sla_cwm$fit-marginal_sla_cwm$se),1)
round((marginal_sla_cwm$fit+marginal_sla_cwm$se),1)

marginal_sla_cwmA <- cbind(marginal_sla_cwm, parameter = "marginal_sla_cwm")
means_model <- rbind(means_model, marginal_sla_cwmA)

#-------------------------------------------------------------------------------# 

# seed mass correct ####
# back_log10_seed_mass_cwm BACK 

(moran_result <- Moran.I(HEADER$log_back_log10_seed_mass_cwm, dists_auto))
write.table(moran_result) 

qqnorm(HEADER$log10_seed_mass_cwm)
qqline(HEADER$log10_seed_mass_cwm)

HEADER$back_log10_seed_mass_cwm <- 10^HEADER$log10_seed_mass_cwm

qqnorm(HEADER$back_log10_seed_mass_cwm)
qqline(HEADER$back_log10_seed_mass_cwm)
skewness(HEADER$back_log10_seed_mass_cwm, na.rm=T)

HEADER$back_log10_seed_mass_cwm
par(mfrow = c(2, 2))
hist(HEADER$back_log10_seed_mass_cwm, main = "Original")
hist(1 / HEADER$back_log10_seed_mass_cwm, main = "Reciprocal")
hist(sqrt(HEADER$back_log10_seed_mass_cwm), main = "Square Root")
hist(log10(HEADER$back_log10_seed_mass_cwm), main = "Log10")

HEADER$log_back_log10_seed_mass_cwm <- log10(HEADER$back_log10_seed_mass_cwm)
qqnorm(HEADER$log_back_log10_seed_mass_cwm)
qqline(HEADER$log_back_log10_seed_mass_cwm)
skewness(HEADER$log_back_log10_seed_mass_cwm, na.rm=T)

# correct anova seed mass 
mod.log_back_log10_seed_mass_cwm <- lmer(log_back_log10_seed_mass_cwm ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova_results_mass <- anova(mod.log_back_log10_seed_mass_cwm, ddf = "Kenward-Roger", type = "III")
anova_results_mass
plot(mod.log_back_log10_seed_mass_cwm)
check_collinearity(mod.log_back_log10_seed_mass_cwm)
plot(resid(mod.log_back_log10_seed_mass_cwm))
plot(allEffects(mod.log_back_log10_seed_mass_cwm))
ggResidpanel::resid_panel(mod.log_back_log10_seed_mass_cwm, plots = c("resid", "qq", "hist"), nrow = 1)

# Calculating marginal means
marginal_log_back_log10_seed_mass_cwm <- as.data.frame(effect("plot_age * class", mod.log_back_log10_seed_mass_cwm))
marginal_log_back_log10_seed_mass_cwm$plot_age <- factor(marginal_log_back_log10_seed_mass_cwm$plot_age, levels = c("Old", "New"))
marginal_log_back_log10_seed_mass_cwm <- marginal_log_back_log10_seed_mass_cwm[order(marginal_log_back_log10_seed_mass_cwm$class, marginal_log_back_log10_seed_mass_cwm$plot_age), ]
marginal_log_back_log10_seed_mass_cwm$class_age <- paste(marginal_log_back_log10_seed_mass_cwm$class, marginal_log_back_log10_seed_mass_cwm$plot_age, sep=".")
marginal_log_back_log10_seed_mass_cwm$class_age <- factor(marginal_log_back_log10_seed_mass_cwm$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

print(marginal_log_back_log10_seed_mass_cwm)
round(marginal_log_back_log10_seed_mass_cwm$fit, digits=3)
round(marginal_log_back_log10_seed_mass_cwm$se, digits=3)

marginal_log_back_log10_seed_mass_cwm <- marginal_log_back_log10_seed_mass_cwm %>% 
  mutate(lower=10^(fit-se), upper = 10^(fit+se), fit=10^(fit)) 


marginal_log_back_log10_seed_mass_cwmA <- cbind(marginal_log_back_log10_seed_mass_cwm, parameter = "marginal_log_back_log10_seed_mass_cwm")
means_model <- rbind(means_model, marginal_log_back_log10_seed_mass_cwmA)


#---------------------------------------------------------------------------------------#
# canopy height ####

#-------------------------------------------------------------------------------------# 

# back_log10_canopy_height_cwm BACK  
(moran_result <- Moran.I(HEADER$log_back_log10_canopy_height_cwm, dists_auto))
write.table(moran_result) 

qqnorm(HEADER$log10_canopy_height_cwm)
qqline(HEADER$log10_canopy_height_cwm)

HEADER$back_log10_canopy_height_cwm <- 10^HEADER$log10_canopy_height_cwm

qqnorm(HEADER$back_log10_canopy_height_cwm)
qqline(HEADER$back_log10_canopy_height_cwm)
skewness(HEADER$back_log10_canopy_height_cwm, na.rm=T)

HEADER$back_log10_canopy_height_cwm
par(mfrow = c(2, 2))
hist(HEADER$back_log10_canopy_height_cwm, main = "Original")
hist(1 / HEADER$back_log10_canopy_height_cwm, main = "Reciprocal")
hist(sqrt(HEADER$back_log10_canopy_height_cwm), main = "Square Root")
hist(log10(HEADER$back_log10_canopy_height_cwm), main = "Log10")

HEADER$log_back_log10_canopy_height_cwm <- log10(HEADER$back_log10_canopy_height_cwm)

qqnorm(HEADER$log_back_log10_canopy_height_cwm)
qqline(HEADER$log_back_log10_canopy_height_cwm)
skewness(HEADER$log_back_log10_canopy_height_cwm, na.rm=T)

# correct anova canopy height #### 
mod.log_back_log10_canopy_height_cwm <- lmer(log_back_log10_canopy_height_cwm ~ plot_age * class + PCNM1 + (1|Old_plot_ID), data = HEADER)
anova_results_log_back_log10_canopy_height_cwm <- anova(mod.log_back_log10_canopy_height_cwm, ddf = "Kenward-Roger", type = "III")
anova_results_log_back_log10_canopy_height_cwm
plot(mod.log_back_log10_canopy_height_cwm)
check_collinearity(mod.log_back_log10_canopy_height_cwm)
plot(resid(mod.log_back_log10_canopy_height_cwm))
plot(allEffects(mod.log_back_log10_canopy_height_cwm))
ggResidpanel::resid_panel(mod.log_back_log10_canopy_height_cwm, plots = c("resid", "qq", "hist"), nrow = 1)

# Calculating marginal means
marginal_mod.log_back_log10_canopy_height_cwm <- as.data.frame(effect("plot_age * class", mod.log_back_log10_canopy_height_cwm))
marginal_mod.log_back_log10_canopy_height_cwm$plot_age <- factor(marginal_mod.log_back_log10_canopy_height_cwm$plot_age, levels = c("Old", "New"))
marginal_mod.log_back_log10_canopy_height_cwm <- marginal_mod.log_back_log10_canopy_height_cwm[order(marginal_mod.log_back_log10_canopy_height_cwm$class, marginal_mod.log_back_log10_canopy_height_cwm$plot_age), ]
marginal_mod.log_back_log10_canopy_height_cwm$class_age <- paste(marginal_mod.log_back_log10_canopy_height_cwm$class, marginal_mod.log_back_log10_canopy_height_cwm$plot_age, sep=".")
marginal_mod.log_back_log10_canopy_height_cwm$class_age <- factor(marginal_mod.log_back_log10_canopy_height_cwm$class_age, levels = c("FB.Old", "FB.New", "KC.Old", "KC.New"))

round(marginal_mod.log_back_log10_canopy_height_cwm$fit,2)
round((marginal_mod.log_back_log10_canopy_height_cwm$fit-marginal_mod.log_back_log10_canopy_height_cwm$se),2)
round((marginal_mod.log_back_log10_canopy_height_cwm$fit+marginal_mod.log_back_log10_canopy_height_cwm$se),2)

print(marginal_mod.log_back_log10_canopy_height_cwm)
round(marginal_mod.log_back_log10_canopy_height_cwm$fit, digits=3)
round(marginal_mod.log_back_log10_canopy_height_cwm$se, digits=3)

marginal_mod.log_back_log10_canopy_height_cwm <- marginal_mod.log_back_log10_canopy_height_cwm %>% 
  mutate(lower=10^(fit-se), upper = 10^(fit+se), fit=10^(fit))


marginal_mod.log_back_log10_canopy_height_cwmA <- cbind(marginal_mod.log_back_log10_canopy_height_cwm, parameter = "marginal_mod.log_back_log10_canopy_height_cwm")
means_model <- rbind(means_model, marginal_mod.log_back_log10_canopy_height_cwmA)

means_model <- means_model %>%
  mutate(fit=round(fit, 2), lower=round(lower, 2), upper=round(upper, 2))
