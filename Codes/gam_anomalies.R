library(ggplot2)
library(GGally)
library(RColorBrewer)
library(RColorBrewer)
library(tidyverse)
library(mgcv)
library(lme4)

dataaux = read.table('/Users/anavaz/Stuff/current_work/2022_red_snapper/figuresJuly/settlement_events_noanoml.txt',header = FALSE)
#dataaux2 = read.table('/Users/anavaz/Stuff/current_work/2022_red_snapper/figuresJuly/settlement_events_max.txt',header = FALSE)

# create dataframe avg
ind = as.data.frame.matrix(dataaux) 
names(ind) <- c("Event",	"SettleTot", "anomlTot",	"MS_avgLag0", "MS_avgLag30", "MS_avgLag15","MS_avgLag60","MS_avgLag60_30","MS_avgLag90","MS_avgLag90_30","LC_avgLag0","LC_avgLag30","LC_avgLag15","LC_avgLag60","LC_avgLag60_30","LC_avgLag90","LC_avgLag90_30")
head(ind)
summary(ind)

# create dataframe max
indmax = as.data.frame.matrix(dataaux2) 
names(indmax) <- c("Event",	"SettleTot","MS_maxLag0", "MS_maxLag30", "MS_maxLag15","MS_maxLag60","MS_maxLag60_30","MS_maxLag90","MS_maxLag90_30","LC_maxLag0","LC_maxLag30","LC_maxLag15","LC_maxLag60","LC_maxLag60_30","LC_maxLag90","LC_maxLag90_30")
head(indmax)
summary(indmax)

settleLevel <- quantile(ind$SettleTot, probs = c(25, 75)/100, na.rm = TRUE)

ind$groupS <- 2
ind$groupS[which(ind$SettleTot < settleLevel[1])] <- 1
ind$groupS[which(ind$SettleTot > settleLevel[2])] <- 3

ind$zone <- "Mid"
ind$zone[which(ind$SettleTot < settleLevel[1])] <- "Low"
ind$zone[which(ind$SettleTot > settleLevel[2])] <- "High"

#-------------------------------------------------------------------------------
# Plot smooth curves 
#-------------------------------------------------------------------------------

ggplot(data = ind, aes(SettleTot, LC_avgLag0)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleLC_0.png",device = "png")

ggplot(data = ind, aes(SettleTot, LC_avgLag0)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleMS_0.png",device = "png")

ggplot(data = ind, aes(SettleTot, MS_avgLag15)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleMS_15.png",device = "png")

ggplot(data = ind, aes(SettleTot, MS_avgLag30)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleMS_30.png",device = "png")

ggplot(data = ind, aes(SettleTot, MS_avgLag60)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleMS_60.png",device = "png")

ggplot(data = ind, aes(SettleTot, MS_avgLag90)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleMS_90.png",device = "png")

ggplot(data = ind, aes(SettleTot, MS_avgLag60_30)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleMS_30_60.png",device = "png")

ggplot(data = ind, aes(SettleTot, MS_avgLag90_30)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleMS_30_90.png",device = "png")

ggplot(data = ind, aes(SettleTot, LC_avgLag0)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleLC_0.png",device = "png")

ggplot(data = ind, aes(SettleTot, LC_avgLag15)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleLC_15.png",device = "png")

ggplot(data = ind, aes(SettleTot, LC_avgLag30)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleLC_30.png",device = "png")

ggplot(data = ind, aes(SettleTot, LC_avgLag60)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleLC_60.png",device = "png")

ggplot(data = ind, aes(SettleTot, LC_avgLag90)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleLC_90.png",device = "png")

ggplot(data = ind, aes(SettleTot, LC_avgLag60_30)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleLC_30_60.png",device = "png")

ggplot(data = ind, aes(SettleTot, LC_avgLag90_30)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("SettleLC_30_90.png",device = "png")

#------------------------------------------------------------------------------------------------
# plot pairs and correlation

png(file="correlation_average.png",width=600, height=350)
gg1 = ggpairs(ind[c(2,4:17)])
gg1$plots = gg1$plots[c(1:16,31,46,61,76,91,106,121,136,151,166,181,196,211)]
gg1$yAxisLabels = gg1$yAxisLabels[1:2]
gg1
dev.off()

#------------------------------------------------------------------------------------------------
# Calculate and plot GAM Lag 0 for MS discharge

mod_lm_MS = gam(SettleTot ~ MS_avgLag0, data = ind)
summary(mod_lm_MS)

mod_lm_LC = gam(SettleTot ~ LC_avgLag0, data = ind)
summary(mod_lm_LC)

mod_lm_MSLC = gam(SettleTot ~ MS_avgLag0*LC_avgLag0, data = ind)
summary(mod_lm_MSLC)

# Calculate and plot GAM Lag 15 for MS discharge

mod_lm_MS15 = gam(SettleTot ~ MS_avgLag15, data = ind)
summary(mod_lm_MS15)

mod_lm_MS15LC = gam(SettleTot ~ MS_avgLag15*LC_avgLag0, data = ind)
summary(mod_lm_MS15LC)

# Calculate and plot GAM Lag 15 for MS discharge

mod_lm_MS30 = gam(SettleTot ~ MS_avgLag30, data = ind)
summary(mod_lm_MS30)

mod_lm_MS30LC = gam(SettleTot ~ MS_avgLag30*LC_avgLag0, data = ind)
summary(mod_lm_MS30LC)

# Calculate and plot GAM Lag 90_30 for LC

mod_lm_LC30_90 = gam(SettleTot ~ LC_avgLag90_30, data = ind)
summary(mod_lm_LC30_90)

mod_lm_MS30LC30_90 = gam(SettleTot ~ MS_avgLag30*LC_avgLag90_30, data = ind)
summary(mod_lm_MS30LC30_90)


# Calculate and plot GAM smoothed Lag 0 for MS discharge

mod_gam_MS = gam(SettleTot ~ s(MS_avgLag0), data = ind)
summary(mod_gam_MS)
png("GAM_MS_Lag0.png")
plot(mod_gam_MS)
dev.off()

mod_gam_MS15 = gam(SettleTot ~ s(MS_avgLag15), data = ind)
summary(mod_gam_MS15)
png("GAM_MS_Lag15.png")
plot(mod_gam_MS15)
dev.off()

mod_gam_MS30 = gam(SettleTot ~ s(MS_avgLag30), data = ind)
summary(mod_gam_MS30)
png("GAM_MS_Lag30.png")
plot(mod_gam_MS30)
dev.off()

mod_gam_LC = gam(SettleTot ~ s(LC_avgLag0), data = ind)
summary(mod_gam_LC)
png("GAM_LC_Lag0.png")
plot(mod_gam_LC)
dev.off()

# Plot GAM for both variables together 

mod_gam_MSLC = gam(SettleTot ~ s(MS_avgLag0) + s(LC_avgLag0), data = ind)
summary(mod_gam_MSLC)
png("GAMresponse_MSLC.png")
vis.gam(mod_gam_MSLC, type = 'response', plot.type = 'contour',color = 'topo')
dev.off()

mod_gam_MS15LC = gam(SettleTot ~ s(MS_avgLag15) + s(LC_avgLag0), data = ind)
summary(mod_gam_MS15LC)
png("GAMresponse_MS15_LC.png")
vis.gam(mod_gam_MS15LC, type = 'response', plot.type = 'contour',color = 'topo')
dev.off()

mod_gam_MS30LC = gam(SettleTot ~ s(MS_avgLag30) + s(LC_avgLag0), data = ind)
summary(mod_gam_MS30LC)
png("GAMresponse_MS30_LC.png")
vis.gam(mod_gam_MS30LC, type = 'response', plot.type = 'contour',color = 'topo')
dev.off()


# check the best model
anova(mod_lm_LC, mod_lm_LC30_90, mod_lm_MS, mod_lm_MS15, mod_lm_MS15LC,
      mod_lm_MS30, mod_lm_MS30LC, mod_lm_MS30LC30_90, mod_lm_MSLC,
      mod_gam_LC, mod_gam_MS, mod_gam_MS15, mod_gam_MS15LC, mod_gam_MS30,
      mod_gam_MS30LC, mod_gam_MSLC, test="F")

AIC(mod_lm_LC)
AIC(mod_lm_LC30_90)
AIC(mod_lm_MS)
AIC(mod_lm_MS15)
AIC(mod_lm_MS15LC)
AIC(mod_lm_MS30)
AIC(mod_lm_MS30LC)
AIC(mod_lm_MS30LC30_90)
AIC(mod_lm_MSLC)
AIC(mod_gam_LC)
AIC(mod_gam_MS)
AIC(mod_gam_MS15)
AIC(mod_gam_MS15LC)
AIC(mod_gam_MS30)
AIC(mod_gam_MS30LC)
AIC(mod_gam_MSLC)


summary(mod_lm_LC)$sp.criterion
summary(mod_lm_LC30_90)$sp.criterion
summary(mod_lm_MS)$sp.criterion
summary(mod_lm_MS15)$sp.criterion
summary(mod_lm_MS15LC)$sp.criterion
summary(mod_lm_MS30)$sp.criterion
summary(mod_lm_MS30LC)$sp.criterion
summary(mod_lm_MS30LC30_90)$sp.criterion
summary(mod_lm_MSLC)$sp.criterion
summary(mod_gam_LC)$sp.criterion
summary(mod_gam_MS)$sp.criterion
summary(mod_gam_MS15)$sp.criterion
summary(mod_gam_MS15LC)$sp.criterion
summary(mod_gam_MS30)$sp.criterion
summary(mod_gam_MS30LC)$sp.criterion
summary(mod_gam_MSLC)$sp.criterion


summary(mod_lm_LC)$r.sq
summary(mod_lm_LC30_90)$r.sq
summary(mod_lm_MS)$r.sq
summary(mod_lm_MS15)$r.sq
summary(mod_lm_MS15LC)$r.sq
summary(mod_lm_MS30)$r.sq
summary(mod_lm_MS30LC)$r.sq
summary(mod_lm_MS30LC30_90)$r.sq
summary(mod_lm_MSLC)$r.sq
summary(mod_gam_LC)$r.sq
summary(mod_gam_MS)$r.sq
summary(mod_gam_MS15)$r.sq
summary(mod_gam_MS15LC)$r.sq
summary(mod_gam_MS30)$r.sq
summary(mod_gam_MS30LC)$r.sq
summary(mod_gam_MSLC)$r.sq


################################################################################
# DO FOR MAXIMUM

#-------------------------------------------------------------------------------
# Plot smooth curves 
#-------------------------------------------------------------------------------

ggplot(data = indmax, aes(SettleTot, LC_maxLag0)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettleLC_0.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, LC_maxLag0)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettle_MS0.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, MS_maxLag15)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettle_MS15.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, MS_maxLag30)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettle_MS30.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, MS_maxLag60)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettle_MS60.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, MS_maxLag90)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettle_MS90.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, MS_maxLag60_30)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettle_MS30_60.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, MS_maxLag90_30)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("River Discharge" (m/s)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettle_MS30_90.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, LC_maxLag0)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettleLC_0.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, LC_maxLag15)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettleLC_15.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, LC_maxLag30)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettleLC_30.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, LC_maxLag60)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettleLC_60.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, LC_maxLag90)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettleLC_90.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, LC_maxLag60_30)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettleLC_30_60.png",device = "png", width=900, height=450, units = "px")

ggplot(data = indmax, aes(SettleTot, LC_maxLag90_30)) + 
  geom_point() +
  geom_smooth(method = loess) + 
  labs(y=expression("Loop Current Northern Extention" (degree*N)), x=expression("Settlement")) +
  theme_light()
ggsave("maxSettleLC_30_90.png",device = "png", width=900, height=450, units = "px")

#------------------------------------------------------------------------------------------------
# plot pairs and correlation

png(file="correlation_maximum.png",width=600, height=350)
gg1 = ggpairs(indmax[c(2:16)])
gg1$plots = gg1$plots[c(1:16,31,46,61,76,91,106,121,136,151,166,181,196,211)]
gg1$yAxisLabels = gg1$yAxisLabels[1:2]
gg1
dev.off()

#------------------------------------------------------------------------------------------------
# Calculate and plot GAM Lag 0 for MS discharge

mod_lm_MS = gam(SettleTot ~ MS_maxLag0, data = indmax)
summary(mod_lm_MS)

mod_lm_LC = gam(SettleTot ~ LC_maxLag0, data = indmax)
summary(mod_lm_LC)

mod_lm_MSLC = gam(SettleTot ~ MS_maxLag0*LC_maxLag0, data = indmax)
summary(mod_lm_MSLC)

# Calculate and plot GAM Lag 15 for MS discharge

mod_lm_MS15 = gam(SettleTot ~ MS_maxLag15, data = indmax)
summary(mod_lm_MS15)

mod_lm_MS15LC = gam(SettleTot ~ MS_maxLag15*LC_maxLag0, data = indmax)
summary(mod_lm_MS15LC)

# Calculate and plot GAM Lag 15 for MS discharge

mod_lm_MS30 = gam(SettleTot ~ MS_maxLag30, data = indmax)
summary(mod_lm_MS30)

mod_lm_MS30LC = gam(SettleTot ~ MS_maxLag30*LC_maxLag0, data = indmax)
summary(mod_lm_MS30LC)

# Calculate and plot GAM Lag 90_30 for LC

mod_lm_LC30_90 = gam(SettleTot ~ LC_maxLag90_30, data = indmax)
summary(mod_lm_LC30_90)

mod_lm_MS30LC30_90 = gam(SettleTot ~ MS_maxLag30*LC_maxLag90_30, data = indmax)
summary(mod_lm_MS30LC30_90)


# Calculate and plot GAM smoothed Lag 0 for MS discharge

mod_gam_MS = gam(SettleTot ~ s(MS_maxLag0), data = indmax)
summary(mod_gam_MS)
png("maxGAM_MS_Lag0.png")
plot(mod_gam_MS)
dev.off()

mod_gam_MS15 = gam(SettleTot ~ s(MS_maxLag15), data = indmax)
summary(mod_gam_MS15)
png("maxGAM_MS_Lag15.png")
plot(mod_gam_MS15)
dev.off()

mod_gam_MS30 = gam(SettleTot ~ s(MS_maxLag30), data = indmax)
summary(mod_gam_MS30)
png("maxGAM_MS_Lag30.png")
plot(mod_gam_MS30)
dev.off()

mod_gam_LC = gam(SettleTot ~ s(LC_maxLag0), data = indmax)
summary(mod_gam_LC)
png("maxGAM_LC_Lag0.png")
plot(mod_gam_LC)
dev.off()

# Plot GAM for both variables together 

mod_gam_MSLC = gam(SettleTot ~ s(MS_maxLag0) + s(LC_maxLag0), data = indmax)
summary(mod_gam_MSLC)
png("maxGAMresponse_MSLC.png")
vis.gam(mod_gam_MSLC, type = 'response', plot.type = 'contour',color = 'topo')
dev.off()

mod_gam_MS15LC = gam(SettleTot ~ s(MS_maxLag15) + s(LC_maxLag0), data = indmax)
summary(mod_gam_MS15LC)
png("maxGAMresponse_MS15_LC.png")
vis.gam(mod_gam_MS15LC, type = 'response', plot.type = 'contour',color = 'topo')
dev.off()

mod_gam_MS30LC = gam(SettleTot ~ s(MS_maxLag30) + s(LC_maxLag0), data = indmax)
summary(mod_gam_MS30LC)
png("maxGAMresponse_MS30_LC.png")
vis.gam(mod_gam_MS30LC, type = 'response', plot.type = 'contour',color = 'topo')
dev.off()



# check the best model
anova(mod_lm_LC, mod_lm_LC30_90, mod_lm_MS, mod_lm_MS15, mod_lm_MS15LC,
      mod_lm_MS30, mod_lm_MS30LC, mod_lm_MS30LC30_90, mod_lm_MSLC,
      mod_gam_LC, mod_gam_MS, mod_gam_MS15, mod_gam_MS15LC, mod_gam_MS30,
      mod_gam_MS30LC, mod_gam_MSLC, test="F")

AIC(mod_lm_LC)
AIC(mod_lm_LC30_90)
AIC(mod_lm_MS)
AIC(mod_lm_MS15)
AIC(mod_lm_MS15LC)
AIC(mod_lm_MS30)
AIC(mod_lm_MS30LC)
AIC(mod_lm_MS30LC30_90)
AIC(mod_lm_MSLC)
AIC(mod_gam_LC)
AIC(mod_gam_MS)
AIC(mod_gam_MS15)
AIC(mod_gam_MS15LC)
AIC(mod_gam_MS30)
AIC(mod_gam_MS30LC)
AIC(mod_gam_MSLC)


summary(mod_lm_LC)$sp.criterion
summary(mod_lm_LC30_90)$sp.criterion
summary(mod_lm_MS)$sp.criterion
summary(mod_lm_MS15)$sp.criterion
summary(mod_lm_MS15LC)$sp.criterion
summary(mod_lm_MS30)$sp.criterion
summary(mod_lm_MS30LC)$sp.criterion
summary(mod_lm_MS30LC30_90)$sp.criterion
summary(mod_lm_MSLC)$sp.criterion
summary(mod_gam_LC)$sp.criterion
summary(mod_gam_MS)$sp.criterion
summary(mod_gam_MS15)$sp.criterion
summary(mod_gam_MS15LC)$sp.criterion
summary(mod_gam_MS30)$sp.criterion
summary(mod_gam_MS30LC)$sp.criterion
summary(mod_gam_MSLC)$sp.criterion


summary(mod_lm_LC)$r.sq
summary(mod_lm_LC30_90)$r.sq
summary(mod_lm_MS)$r.sq
summary(mod_lm_MS15)$r.sq
summary(mod_lm_MS15LC)$r.sq
summary(mod_lm_MS30)$r.sq
summary(mod_lm_MS30LC)$r.sq
summary(mod_lm_MS30LC30_90)$r.sq
summary(mod_lm_MSLC)$r.sq
summary(mod_gam_LC)$r.sq
summary(mod_gam_MS)$r.sq
summary(mod_gam_MS15)$r.sq
summary(mod_gam_MS15LC)$r.sq
summary(mod_gam_MS30)$r.sq
summary(mod_gam_MS30LC)$r.sq
summary(mod_gam_MSLC)$r.sq

################################################################################
# EXTRA

#-------------------------------------------------------------------------------
# Plot scatter pairs with groups 
#-------------------------------------------------------------------------------

# png(file="maxSettle_0",width=600, height=350)
# pairs(~SettleTot + MS_avgLag0, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleLC_0",width=600, height=350)
# pairs(~SettleTot + LC_avgLag0, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleMS_30",width=600, height=350)
# pairs(~SettleTot + MS_avgLag30, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleLC_30",width=600, height=350)
# pairs(~SettleTot + LC_avgLag30, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleMS_60",width=600, height=350)
# pairs(~SettleTot + MS_avgLag60, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleLC_60",width=600, height=350)
# pairs(~SettleTot + LC_avgLag60, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleMS_15",width=600, height=350)
# pairs(~SettleTot + MS_avgLag15, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleLC_15",width=600, height=350)
# pairs(~SettleTot + LC_avgLag15, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleMS_60_30",width=600, height=350)
# pairs(~SettleTot + MS_avgLag60_30, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleLC_60_30",width=600, height=350)
# pairs(~SettleTot + LC_avgLag60_30, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleMS_90",width=600, height=350)
# pairs(~SettleTot + MS_avgLag90, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleLC_90",width=600, height=350)
# pairs(~SettleTot + LC_avgLag90, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleMS_90_30",width=600, height=350)
# pairs(~SettleTot + MS_avgLag90_30, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()
# 
# png(file="SettleLC_90_30",width=600, height=350)
# pairs(~SettleTot + LC_avgLag90_30, data=ind, col=c("cornflowerblue", "black", "red")[ind$group])
# dev.off()