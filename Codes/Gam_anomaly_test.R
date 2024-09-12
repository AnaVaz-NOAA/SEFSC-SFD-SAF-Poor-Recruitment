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
covarNames <- aux[c(2:31)]
spNames <- aux[32:41]
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
itot = 5*4 + 2 + 2*4 + 1

gam_models <- list()
gam_aic_matrix <- matrix(NA, nrow = itot-1, ncol = 10, dimnames = list(covarNames, spNames))

yearsPlot <- unique(CovarSeason_RecDev[,1])
pal <- wes_palette("Zissou1", 30, type = "continuous")

# Create directory to save plots

dir.create("GAM_Plots_Species_k5_no15Chl", showWarnings = FALSE)

# Loop over species to fit models and create plots
for (iSpp in 1:10) {
  species_name  <- spNames[iSpp]
  spNamePlot <- gsub(" ", "_", species_name) # Clean species name for filenames
  data_combined <- data.frame()
  
  # Create empty lists to store plots
  plots_model <- list()
  plots_residuals_hist   <- list()
  plots_residuals_fitted <- list()
  plots_response_fitted  <- list()
  
  for (iCovar in 2:itot) {
    covarNamePlot <- covarNames[iCovar - 1]
    
    # Prepare data for plotting
    x <- CovarSeason_RecDev[, iCovar]
    y <- CovarSeason_RecDev[, iSpp + itot]
    
    data <- data.frame(x = x, y = y, Year = CovarSeason_RecDev$X, Covariate = covarNamePlot)
    data <- na.omit(data)
    
    # GAM
    gam_model <- gam(y ~ s(x, k=5), data = data, method = "REML")
    
    r_squared <- summary(gam_model)$r.sq
    p_value <- summary(gam_model)$s.pv[1]
    correlation <- cor(data$x, data$y, use = "complete.obs")
    
    # Add statistics to data
    data$R <- round(correlation, 2)
    data$p_value <- format(p_value, digits = 2)
    data$r_squared <- round(r_squared, 2)
    
    # Combine data for this species
    data_combined <- rbind(data_combined, data)
    
    png(filename = paste0("GAM_Plots_Species_k5_no15Chl/", spNamePlot, 
                          "_", covarNamePlot ,"_GAM_Check.png"),
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
    geom_smooth(method = "gam", formula = y ~ s(x, k=5), color = "blue") +
    facet_wrap(~ Covariate, scales = "free_x", ncol = 4) +
    labs(
      title = paste("GAM Results for", species_name),
      x = "Covariate",
      y = "Response"
    ) +
    theme_light() +
    scale_color_gradientn(colors = pal, name = "") +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
    geom_text(data = data_combined, aes(x = Inf, y = Inf, 
                                        label = paste0("R: ", R, "\np: ", p_value, "\nR²: ", r_squared)),
              hjust = 1.1, vjust = 1.1, size = 3, color = "black", parse = FALSE)
  
  # Save the plot for the current species
  ggsave(
    filename = paste0("GAM_Plots_Species_k5_no15Chl/GAM_Results_All_", spNamePlot, "_.png"),
    plot = species_plot,
    width = 12,
    height = 25)
}

# Convert matrices to data frames for plotting
gam_aic_df <- as.data.frame(gam_aic_matrix)
gam_aic_df$Mode <- rownames(gam_aic_df)
gam_aic_df_melt <- melt(gam_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

# Plot heatmap for GAM AIC values
gam_AIC <- ggplot(gam_aic_df_melt, aes(x = Species, y = Mode, fill = AIC)) +
  geom_tile() +
  scale_fill_gradient(low = "royalblue", high = "tomato3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Heatmap of AIC Values for GAM Models") +
  xlab("Species") +
  ylab("Mode")
print(gam_AIC)
ggsave(paste0("./images/AIC_GAM_seasons_k5_noChl15.png"), width = 6, height = 6, units = "in", dpi = 300) 

# Find the minimum AIC values and their corresponding indices for GAM models
min_gam_aic <- min(abs(gam_aic_matrix), na.rm = TRUE)
best_gam_model <- which(abs(gam_aic_matrix) == min_gam_aic, arr.ind = TRUE)
best_gam_model_info <- data.frame(
  Mode = rownames(gam_aic_matrix)[best_gam_model[, 1]],
  Species = colnames(gam_aic_matrix)[best_gam_model[, 2]],
  AIC = min_gam_aic
)

# Print the best models information
print("Best GAM Model Season :")
print(best_gam_model_info)

# Find the best GAM model for each species
# best_gam_models <- apply(gam_aic_matrix, 2, function(column) {
#   min_index <- which.min(column)
#   list(Mode = rownames(gam_aic_matrix)[min_index], AIC = column[min_index])
# })

best_gam_models <- apply(gam_aic_matrix, 2, function(column) {
  # Calculate the absolute values of AICs
  abs_column <- abs(column)
  # Find the index of the minimum absolute AIC value
  min_index <- which.min(abs_column)
  list(Mode = rownames(gam_aic_matrix)[min_index], AIC = column[min_index])
})

# Combine the data frames with species and the mode (variable) yielding the best model
best_gam_df <- do.call(rbind, lapply(seq_along(best_gam_models), function(i) {
  data.frame(Species = names(best_gam_models)[i], 
             Mode = best_gam_models[[i]]$Mode, 
             Label = paste(names(best_gam_models)[i], best_gam_models[[i]]$Mode, sep = " - "),
             AIC = best_gam_models[[i]]$AIC)
}))

# Plot the best models with combined labels
# Create the plot with labels on the y-axis and AIC values on the x-axis
best_models_plot <- ggplot(best_gam_df, aes(x = AIC, y = Label)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  theme_light() +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Best Models for Each Species Based on AIC") +
  xlab("AIC") +
  ylab("Species - Best Mode")
ggsave(paste0("./images/best_models_seasons_k5_noChl15.png"), width = 5, height = 10, units = "in", dpi = 300) 


# Find the best 3 GAM models by minimum absolute AIC value
best_3_gam_models <- apply(gam_aic_matrix, 2, function(column) {
  # Calculate the absolute values of AICs
  abs_column <- abs(column)
  # Sort the indices by the AIC values
  sorted_indices <- order(abs_column)
  # Select the top 3 models
  top_3_indices <- sorted_indices[1:5]
  # Return the modes and AICs of the top 3 models
  list(Mode = rownames(gam_aic_matrix)[top_3_indices], AIC = column[top_3_indices])
})

# Combine the data frames with species and the modes (variables) yielding the best 3 models
best_3_gam_df <- do.call(rbind, lapply(seq_along(best_3_gam_models), function(i) {
  data.frame(Species = rep(names(best_3_gam_models)[i], 5), 
             Mode = best_3_gam_models[[i]]$Mode, 
             Label = paste(names(best_3_gam_models)[i], best_3_gam_models[[i]]$Mode, sep = " - "),
             AIC = best_3_gam_models[[i]]$AIC)
}))

# Plot the best 3 models with combined labels
# Create the plot with labels on the y-axis and AIC values on the x-axis
best_models_plot <- ggplot(best_3_gam_df, aes(x = AIC, y = Label)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  theme_light() +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Best Models for Each Species Based on AIC") +
  xlab("AIC") +
  ylab("Species - Best Mode")
ggsave(paste0("./images/best_3models_seasons_k5_noChl15.png"), width = 5, height = 10, units = "in", dpi = 300) 



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
