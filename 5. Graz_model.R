
only_FB$log_GI <- log10(only_FB$GI+1)
only_FB$log_GI6 <- log10(only_FB$GI6+1)

FB_new <- only_FB[only_FB$plot_age=="New",]
FB_new$New_plot_ID <- as.factor(as.character(FB_new$New_plot_ID))
FB_new$Toponym_short <- as.factor(as.character(FB_new$Toponym_short))
FB_new$PCNM1

dists <- as.matrix(dist(cbind(FB_new$Longitude, FB_new$Latitude), method = "euclidean"))

dists_auto <- 1/dists 
diag(dists_auto) <- 0
dist <- pcnm(dists_auto)
FB_new$PCNM11 <-dist$vectors[,1]# this creates a column in the data frame and we should add this
# as predictor variable to the model if we have spatial correlation in response variable!

# Evenness ~ GI ####

#------------------# Evenness 3 - log_GI + I(log_GI^2) ####
(moran_result <- Moran.I(FB_new$Evenness, dists_auto))

graz.mod.evennessF11_new <- lmer(Evenness ~ log_GI + I(log_GI^2) + (1|Toponym_short), data = FB_new)
anova(graz.mod.evennessF11_new, ddf = "Kenward-Roger", type = "II")
ggResidpanel::resid_panel(graz.mod.evennessF11_new, plots = c("resid", "qq", "hist"), nrow = 1)
plot(allEffects(graz.mod.evennessF11_new))
plot(graz.mod.evennessF11_new)
plot(predictorEffects(graz.mod.evennessF11_new))

AIC(graz.mod.evennessF11_new)

library(MuMIn)
graz.mod.evennessF11_new3 <- lmer(Evenness ~ log_GI + I(log_GI^2) + (1|Toponym_short), data = FB_new)
anova(graz.mod.evennessF11_new3, ddf = "Kenward-Roger", type = "II")
r_squared <- r.squaredGLMM(graz.mod.evennessF11_new3)
r_squared


# Evenness ~ GI6 ####

#------------------# Evenness 6 - log_GI6 + I(log_GI6^2) ####
(moran_result <- Moran.I(FB_new$Evenness, dists_auto))

graz.mod.evennessF11_new6 <- lmer(Evenness ~ log_GI6 + I(log_GI6^2) + (1|Toponym_short), data = FB_new)
anova(graz.mod.evennessF11_new6, ddf = "Kenward-Roger", type = "II")
ggResidpanel::resid_panel(graz.mod.evennessF11_new6, plots = c("resid", "qq", "hist"), nrow = 1)
plot(allEffects(graz.mod.evennessF11_new6))
plot(graz.mod.evennessF11_new6)
plot(predictorEffects(graz.mod.evennessF11_new6))
summary(graz.mod.evennessF11_new6)

AIC(graz.mod.evennessF11_new6)

# Shannon ~ GI ####

#-------------------# Shannon 3 ~ log_GI + I(log_GI^2) ####
(moran_result <- Moran.I(FB_new$Shannon, dists_auto))

graz.mod.ShannonF11 <- lmer(Shannon ~ log_GI + I(log_GI^2) + (1|Toponym_short), data = FB_new)
anova(graz.mod.ShannonF11, ddf = "Kenward-Roger", type = "II")
ggResidpanel::resid_panel(graz.mod.ShannonF11, plots = c("resid", "qq", "hist"), nrow = 1)
plot(allEffects(graz.mod.ShannonF11))
plot(graz.mod.ShannonF11)
plot(predictorEffects(graz.mod.ShannonF11))


AIC(graz.mod.ShannonF11)


# Shannon ~ GI6 ####

#-------------------# Shannon 6 ~ log_GI6 + I(log_GI6^2) ####
(moran_result <- Moran.I(FB_new$Shannon, dists_auto))

graz.mod.ShannonF116 <- lmer(Shannon ~ log_GI6 + I(log_GI6^2) + (1|Toponym_short), data = FB_new)
anova(graz.mod.ShannonF116, ddf = "Kenward-Roger", type = "II")
ggResidpanel::resid_panel(graz.mod.ShannonF116, plots = c("resid", "qq", "hist"), nrow = 1)
plot(allEffects(graz.mod.ShannonF116))
plot(graz.mod.ShannonF116)
plot(predictorEffects(graz.mod.ShannonF116))
summary(graz.mod.ShannonF116)

AIC(graz.mod.ShannonF116)

# Richness ~ GI ####

#--------------------# Richness 3 ~ log_GI6 + I(log_GI6^2) ####
(moran_result <- Moran.I(FB_new$Specnumber, dists_auto))

graz.mod.RichnessF11 <- lmer(Specnumber ~ log_GI + I(log_GI^2) + PCNM11 + (1|Toponym_short), data = FB_new)
anova(graz.mod.RichnessF11, ddf = "Kenward-Roger", type = "II")
ggResidpanel::resid_panel(graz.mod.RichnessF11, plots = c("resid", "qq", "hist"), nrow = 1)
plot(allEffects(graz.mod.RichnessF11))
plot(graz.mod.RichnessF11)
plot(predictorEffects(graz.mod.RichnessF11))


AIC(graz.mod.RichnessF11)


# Richness ~ GI6 ####

#--------------------# Richness 6 ~ log_GI6 + I(log_GI6^2) ####
Moran.I(FB_new$Specnumber, dists_auto)

graz.mod.RichnessF116 <- lmer(Specnumber ~ log_GI6 + I(log_GI6^2) + PCNM11 + (1|Toponym_short), data = FB_new)
anova(graz.mod.RichnessF116, ddf = "Kenward-Roger", type = "II")
ggResidpanel::resid_panel(graz.mod.RichnessF116, plots = c("resid", "qq", "hist"), nrow = 1)
plot(allEffects(graz.mod.RichnessF116))
plot(graz.mod.RichnessF116)
plot(predictorEffects(graz.mod.RichnessF116))
summary(graz.mod.RichnessF116)

AIC(graz.mod.RichnessF116)

 