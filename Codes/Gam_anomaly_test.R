# Make correlations between indexes, EOF and recdev
# calculate spectral analyses
library(gplots)
library(RColorBrewer)
library(bcp)
library(gridExtra)
library(dplyr)
library(reshape2)
library(ggplot2)
library(wesanderson)
library(mgcv)
library(glmmTMB)
library(lme4)

graphics.off()
# load the file with all covariates and rec devs
CovarSeason_RecDev <- read.csv("./csv_files/CovarSeason_RecDev.csv")

aux <- colnames(CovarSeason_RecDev)
covarNames <- aux[c(2:41)]
spNames <- aux[42:51]
# CovarSeason_RecDev <- CovarSeason_RecDev[ ,c(1:25, 30:51)]

yearsPlot <- 1993:2021
color_palette <- rainbow(length(unique(yearsPlot)))
coloraux = colorRampPalette(brewer.pal(11, "Spectral"))
color_palette = coloraux(length(unique(yearsPlot)))

#------------------------------------------------------------------
# For SEASONAL
graphics.off()

pcut <- 0.05
# number of covariates 
itot = 5*4 + 3*4 + 2*4 + 1
gam_models <- list()
gam_aic_matrix <- matrix(NA, nrow = itot-1, ncol = 10, 
                         dimnames = list(covarNames, spNames))
yearsPlot <- unique(CovarSeason_RecDev[,1])
pal <- wes_palette("Zissou1", 30, type = "continuous")

# Create directory to save plots
proposition <- "_"
dirName <- paste0("GAM_Plots_Species", proposition)
dir.create(dirName, showWarnings = FALSE)

# Loop over species to fit models and create plots
for (iSpp in 1:10) {
  species_name  <- spNames[iSpp]
  spNamePlot <- gsub(" ", "_", species_name)
  data_combined <- data.frame()
  
  # Empty lists 
  plots_model <- list()
  plots_residuals_hist   <- list()
  plots_residuals_fitted <- list()
  plots_response_fitted  <- list()
  
  for (iCovar in 2:itot) {
    covarNamePlot <- covarNames[iCovar - 1]
    
    # Prepare data for plotting
    x <- CovarSeason_RecDev[, iCovar]
    y <- CovarSeason_RecDev[, iSpp + itot]
    
    data <- data.frame(x = x, y = y, 
                       Year = CovarSeason_RecDev$X, 
                       Covariate = covarNamePlot)
    data <- na.omit(data)
    
    # GAM
    gam_model <- gam(y ~ s(x), data = data, method = "REML")
    
    r_squared <- summary(gam_model)$r.sq
    p_value <- summary(gam_model)$s.pv[1]
    correlation <- cor(data$x, data$y, use = "complete.obs")
    
    # Add statistics to data
    data$R <- round(correlation, 2)
    data$p_value <- format(p_value, digits = 2)
    data$r_squared <- round(r_squared, 2)
    data_combined <- rbind(data_combined, data)
    
    png(filename = 
        paste0(dirName, "/", spNamePlot, "_", covarNamePlot ,"_GAM_Check.png"),
        width = 800, height = 800)  # Adjust width and height as needed
    par(mfrow = c(2, 2))
    gam.check(gam_model)
    dev.off()
    
    # Calculate AIC 
    gam_aic <- AIC(gam_model)

    # Store the models and AIC values
    gam_models[[paste("Covar", iCovar, "Spp", iSpp, sep = "_")]] <- gam_model
    gam_aic_matrix[iCovar-1, iSpp] <- gam_aic
  }
  
  # Generate combined plot for the species
  species_plot <- ggplot(data_combined, aes(x = x, y = y)) +
    geom_point(aes(colour = Year)) +
    geom_smooth(method = "gam", formula = y ~ s(x), color = "blue") +
    facet_wrap(~ Covariate, scales = "free_x", ncol = 4) +
    labs(title = paste("GAM Results for", species_name),
      x = "Covariate", y = "Response" ) +
    theme_light() +
    scale_color_gradientn(colors = pal, name = "") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
    geom_text(data = data_combined, aes(x = Inf, y = Inf, 
                  label = paste0("R: ", R,"\np: ", p_value, "\nR²: ", r_squared)),
              hjust = 1.1, vjust = 1.1, size = 3, color = "black", parse = FALSE)
  ggsave(filename = 
      paste0(dirName, "/", "GAM_Results_All_", spNamePlot, ".png"),
    plot = species_plot, width = 12, height = 25)
}

# Convert matrices to data frames for plotting
gam_aic_df <- as.data.frame(gam_aic_matrix)
gam_aic_df$Mode <- rownames(gam_aic_df)
gam_aic_df_melt <- melt(gam_aic_df, id.vars = "Mode", 
                        variable.name = "Species", value.name = "AIC")

# Plot heatmap for GAM AIC values
gam_AIC <- ggplot(gam_aic_df_melt, aes(x = Species, y = Mode, fill = abs(AIC))) +
  geom_tile() +
  #scale_fill_gradient2(low = "#3B9AB2", mid = "#E3CA34", high = "#F21A00", midpoint = mean(abs(gam_aic_df_melt$AIC), na.rm = TRUE)) +
  scale_fill_gradientn(colors = pal) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Heatmap of AIC Values for GAM Models") +
  xlab("Species") +
  ylab("Mode")
print(gam_AIC)
ggsave(paste0("./images/AIC_GAM_seasons", proposition,".png"), 
       width = 6, height = 6, units = "in", dpi = 300) 

# Loop through species to create and save individual plots
for (iSpp in 1:length(spNames)) {
  spNamePlot <- spNames[iSpp]
  # get current species
  toplot <- subset(gam_aic_df_melt, Species == spNamePlot)
  # Sort by AIC
  toplot <- toplot[order(toplot$AIC), ]
  
  # Create the plot
  best_models_plot <- ggplot(toplot, aes(x = AIC, y = reorder(Mode, AIC))) +
    geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
    theme_light() +
    theme(axis.text.y = element_text(size = 10)) +
    ggtitle(paste("Best Models for", spNamePlot, "Based on AIC")) +
    xlab("AIC") +
    ylab("Mode")
  # Save the plot
  ggsave(paste0("./images/best_models_", spNamePlot, proposition, ".png"), 
         plot = best_models_plot, 
         width = 5, height = 10, units = "in", dpi = 300) 
}
  

# 3 best models for each species

# BSB
# SSH Spring, deep gulf winter, bottom T Fall
# Prepare data for plotting
x1 <- CovarSeason_RecDev[, 19]
x2 <- CovarSeason_RecDev[, 26]
x3 <- CovarSeason_RecDev[, 2]
y <- CovarSeason_RecDev[, 36]

data <- data.frame(x1 = x1, x2 = x2, x3 = x3, y = y, Year = CovarSeason_RecDev$X, Covariate = covarNamePlot)
data <- na.omit(data)

gam_model <- gam(y ~ s(x1, k=5) + s(x2, k=5) + s(x3, k=5), data = data, method = "REML")

summary(gam_model)

# Combine data for this species
data_combined <- rbind(data_combined, data)

png(filename = paste0("BSB_3best_GAM_Check.png"),
    width = 800, height = 800)  # Adjust width and height as needed
par(mfrow = c(2, 2))
gam.check(gam_model)
dev.off()

# Calculate AIC 
gam_aic <- AIC(gam_model)