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
# load recdevs, indexes and EOF
# set the diretory with the assessment files
#setwd("./csv_files")
RecDev      <- read.csv("./csv_files/RecruitmentResiduals_Feb23.csv")
Season   <- read.csv("./csv_files/Anomaly_Seasonal.csv")
SpSeason <- read.csv("./csv_files/Anomaly_Spawning_Seasonal.csv")

yearsPlot <- 1993:2021
color_palette <- rainbow(length(unique(yearsPlot)))
coloraux = colorRampPalette(brewer.pal(11, "Spectral"))
color_palette = coloraux(length(unique(yearsPlot)))

# it is in alphabetical order
NamePlot <- c("Bottom T","Mixed Layer","Salinity","SSH","SST")
NameSeason <- c("Fall","Spring","Summer","Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotSeason <- paste(aux$Var2, aux$Var1)

# it is in alphabetical order
NameSeason <- c("Summer","Winter")
# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotSpawnSeason <- paste(aux$Var2, aux$Var1)

# create combinations of names and combine the names
aux <- expand.grid(NamePlot, NameModes)
NamePlotModes <- paste(aux$Var1, aux$Var2)

spNamePlot <- c("Gag Grouper","Greater Amberjack","Gray Triggerfish"," Red Porgy","Red Grouper","Black Sea Bass","Red Snapper","Scamp","Snowy Grouper ","Vermilion Snapper")

#------------------------------------------------------------------
# For SEASONAL
graphics.off()

pcut <- 0.1
# create a matrix with only dates both have data
EOFSeason_RecDev <- merge(Season, RecDev, by = "X")

# Loop over EOFs and calculate correlation with indexes
# we have 5 variables, vs 4 seasons (20) and 10 species
itot = 5*4+1

# for results of models
gam_models <- list()
gam_aic_matrix <- matrix(NA, nrow = itot-1, ncol = 10, dimnames = list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(EOFSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]
    
    # Combine x and y into a data frame and remove rows with NAs
    data <- data.frame(x = x, y = y, Year = EOFSeason_RecDev$X)
    data <- na.omit(data)
    
    # Fit a GAM model
    gam_model <- gam(y ~ s(x), data = data)
    model <- lm(y ~ x, data = data)
    # Calculate AIC for both models
    gam_aic <- AIC(gam_model)

    # Store the models and AIC values
    gam_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- gam_model
    gam_aic_matrix[iEOF-1, iSpp] <- gam_aic
  } # end iEOF, lwd 
}  # end iInd

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
ggsave(paste0("../images/AIC_GAM_seasons.png"), width = 6, height = 6, units = "in", dpi = 300) 

# Find the minimum AIC values and their corresponding indices for GAM models
min_gam_aic <- min(gam_aic_matrix, na.rm = TRUE)
best_gam_model <- which(gam_aic_matrix == min_gam_aic, arr.ind = TRUE)
best_gam_model_info <- data.frame(
  Mode = rownames(gam_aic_matrix)[best_gam_model[, 1]],
  Species = colnames(gam_aic_matrix)[best_gam_model[, 2]],
  AIC = min_gam_aic
)

# Print the best models information
print("Best GAM Model Season :")
print(best_gam_model_info)

# Find the best GAM model for each species
best_gam_models <- apply(gam_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(gam_aic_matrix)[min_index], AIC = column[min_index])
})

# Find the best GLMM model for each species
best_glmm_models <- apply(glmm_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(glmm_aic_matrix)[min_index], AIC = column[min_index])
})

# Combine the data frames with species and the mode (variable) yielding the best model
best_gam_df <- do.call(rbind, lapply(seq_along(best_gam_models), function(i) {
  data.frame(Species = names(best_gam_models)[i], 
             Mode = best_gam_models[[i]]$Mode, 
             Label = paste(names(best_gam_models)[i], best_gam_models[[i]]$Mode, sep = " - "),
             AIC = best_gam_models[[i]]$AIC)
}))

best_glmm_df <- do.call(rbind, lapply(seq_along(best_glmm_models), function(i) {
  data.frame(Species = names(best_glmm_models)[i], 
             Mode = best_glmm_models[[i]]$Mode, 
             Label = paste(names(best_glmm_models)[i], best_glmm_models[[i]]$Mode, sep = " - "),
             AIC = best_glmm_models[[i]]$AIC)
}))

# Combine the data frames for comparison
best_models_df <- bind_rows(
  best_gam_df %>% mutate(Model = "GAM"),
  best_glmm_df %>% mutate(Model = "GLMM")
)

# Plot the best models with combined labels
# Create the plot with labels on the y-axis and AIC values on the x-axis
best_models_plot <- ggplot(best_models_df, aes(x = AIC, y = Label, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  theme_light() +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Best Models for Each Species Based on AIC") +
  xlab("AIC") +
  ylab("Species - Best Mode")
ggsave(paste0("../images/best_models_seasons.png"), width = 5, height = 10, units = "in", dpi = 300) 

#------------------------------------------------------------------
# For Spawning SEASONAL
graphics.off()

# create a matrix with only dates both have data
EOFSpSeason_RecDev <- merge(SpSeason, RecDev, by = "X")

# we have 5 Anomal variables, vs 2 seasons (10) and 10 species
itot <- (5*2)+1

# for results of models
gam_models <- list()
glmm_models <- list()
gam_aic_matrix <- matrix(NA, nrow = itot-1, ncol = 10, dimnames = list(NamePlotSpawnSeason, spNamePlot))
glmm_aic_matrix <- matrix(NA, nrow = itot-1, ncol = 10, dimnames = list(NamePlotSpawnSeason, spNamePlot))

yearsPlot <- unique(EOFSpSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSpSeason_RecDev[,iEOF]
    y <- EOFSpSeason_RecDev[,iSpp+itot]
    
    # Combine x and y into a data frame and remove rows with NAs
    data <- data.frame(x = x, y = y, Year = EOFSpSeason_RecDev$X)
    data <- na.omit(data)
    
    # Fit a GAM model
    gam_model <- gam(y ~ s(x, k=5), data = data)
    
    # Fit a GLMM model
    #   glmm_model <- glmmTMB(y ~ x + (1|Year), data = data)
    glmm_model <- glmmTMB(y ~ x, data = data)
    
    # Calculate AIC for both models
    gam_aic <- AIC(gam_model)
    glmm_aic <- AIC(glmm_model)
    
    # Store the models and AIC values
    gam_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- gam_model
    glmm_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- glmm_model
    gam_aic_matrix[iEOF-1, iSpp] <- gam_aic
    glmm_aic_matrix[iEOF-1, iSpp] <- glmm_aic
  } # end iEOF, lwd 
}  # end iInd

# Convert matrices to data frames for plotting
gam_aic_df <- as.data.frame(gam_aic_matrix)
gam_aic_df$Mode <- rownames(gam_aic_df)
gam_aic_df_melt <- melt(gam_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

glmm_aic_df <- as.data.frame(glmm_aic_matrix)
glmm_aic_df$Mode <- rownames(glmm_aic_df)
glmm_aic_df_melt <- melt(glmm_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

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
ggsave(paste0("../images/AIC_GAM_seasons.png"),  width = 6, height = 6, units = "in", dpi = 300) 

# Plot heatmap for GLMM AIC values
glm_AIC <- ggplot(glmm_aic_df_melt, aes(x = Species, y = Mode, fill = AIC)) +
  geom_tile() +
  scale_fill_gradient(low = "royalblue", high = "tomato3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Heatmap of AIC Values for GLMM Models") +
  xlab("Species") +
  ylab("Mode")
print(glm_AIC)
ggsave(paste0("../images/AIC_GLM_seasons.png"), width = 6, height = 6, units = "in", dpi = 300) 

# Find the minimum AIC values and their corresponding indices for GAM models
min_gam_aic <- min(gam_aic_matrix, na.rm = TRUE)
best_gam_model <- which(gam_aic_matrix == min_gam_aic, arr.ind = TRUE)
best_gam_model_info <- data.frame(
  Mode = rownames(gam_aic_matrix)[best_gam_model[, 1]],
  Species = colnames(gam_aic_matrix)[best_gam_model[, 2]],
  AIC = min_gam_aic
)

# Find the minimum AIC values and their corresponding indices for GLMM models
min_glmm_aic <- min(glmm_aic_matrix, na.rm = TRUE)
best_glmm_model <- which(glmm_aic_matrix == min_glmm_aic, arr.ind = TRUE)
best_glmm_model_info <- data.frame(
  Mode = rownames(glmm_aic_matrix)[best_glmm_model[, 1]],
  Species = colnames(glmm_aic_matrix)[best_glmm_model[, 2]],
  AIC = min_glmm_aic
)

# Print the best models information
print("Best GAM Model Spawning Season :")
print(best_gam_model_info)
print("Best GLMM Model Spawning Season:")
print(best_glmm_model_info)

# Find the best GAM model for each species
best_gam_models <- apply(gam_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(gam_aic_matrix)[min_index], AIC = column[min_index])
})

# Find the best GLMM model for each species
best_glmm_models <- apply(glmm_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(glmm_aic_matrix)[min_index], AIC = column[min_index])
})

# Combine the data frames with species and the mode (variable) yielding the best model
best_gam_df <- do.call(rbind, lapply(seq_along(best_gam_models), function(i) {
  data.frame(Species = names(best_gam_models)[i], 
             Mode = best_gam_models[[i]]$Mode, 
             Label = paste(names(best_gam_models)[i], best_gam_models[[i]]$Mode, sep = " - "),
             AIC = best_gam_models[[i]]$AIC)
}))

best_glmm_df <- do.call(rbind, lapply(seq_along(best_glmm_models), function(i) {
  data.frame(Species = names(best_glmm_models)[i], 
             Mode = best_glmm_models[[i]]$Mode, 
             Label = paste(names(best_glmm_models)[i], best_glmm_models[[i]]$Mode, sep = " - "),
             AIC = best_glmm_models[[i]]$AIC)
}))

# Combine the data frames for comparison
best_models_df <- bind_rows(
  best_gam_df %>% mutate(Model = "GAM"),
  best_glmm_df %>% mutate(Model = "GLMM")
)

# Plot the best models with combined labels
# Create the plot with labels on the y-axis and AIC values on the x-axis
best_models_plot <- ggplot(best_models_df, aes(x = AIC, y = Label, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  theme_light() +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Best Models for Each Species Based on AIC") +
  xlab("AIC") +
  ylab("Species - Best Mode")
ggsave(paste0("../images/best_models_spawning_seasons.png"), width = 5, height = 10, units = "in", dpi = 300) 

#------------------------------------------------------------------
# Chl Globcolour 15
#------------------------------------------------------------------
RecDev      <- read.csv("RecruitmentResiduals_Feb23.csv")
Season   <- read.csv("Anomaly_Seasonal_ChlShelf15.csv")
SpSeason <- read.csv("Anomaly_Seasonal_ChlSpShelf15.csv")

yearsPlot <- 1998:2021
color_palette <- rainbow(length(unique(yearsPlot)))
coloraux <- colorRampPalette(brewer.pal(11, "Spectral"))
color_palette <- coloraux(length(unique(yearsPlot)))

# it is in alphabetical order
NamePlot <- c("Chl")
NameSeason <- c("Fall","Spring","Summer","Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotSeason <- paste(aux$Var2, aux$Var1)

# it is in alphabetical order
NameSeason <- c("Summer","Winter")
# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotSpawnSeason <- paste(aux$Var2, aux$Var1)

# create combinations of names and combine the names
aux <- expand.grid(NamePlot, NameModes)
NamePlotModes <- paste(aux$Var1, aux$Var2)

spNamePlot <- c("Gag Grouper","Greater Amberjack","Gray Triggerfish"," Red Porgy","Red Grouper","Black Sea Bass","Red Snapper","Scamp","Snowy Grouper ","Vermilion Snapper")

#------------------------------------------------------------------
# For Globcolour 15 SEASONAL
#------------------------------------------------------------------
graphics.off()

pcut <- 0.1
# create a matrix with only dates both have data
EOFSeason_RecDev <- merge(Season, RecDev, by = "X")

# Loop over seasons and calculate correlation with indexes
# we have 1 variable, vs 4 seasons (4) and 10 species
itot = 4+1

# for results of models
gam_models <- list()
glmm_models <- list()
gam_aic_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
glmm_aic_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(EOFSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]
    
    # Combine x and y into a data frame and remove rows with NAs
    data <- data.frame(x = x, y = y, Year = EOFSeason_RecDev$X)
    data <- na.omit(data)
    
    # Fit a GAM model
    gam_model <- gam(y ~ s(x, k=5), data = data)
    
    # Fit a GLMM model
    #   glmm_model <- glmmTMB(y ~ x + (1|Year), data = data)
    glmm_model <- glmmTMB(y ~ x, data = data)
    
    # Calculate AIC for both models
    gam_aic <- AIC(gam_model)
    glmm_aic <- AIC(glmm_model)
    
    # Store the models and AIC values
    gam_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- gam_model
    glmm_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- glmm_model
    gam_aic_matrix[iEOF-1, iSpp] <- gam_aic
    glmm_aic_matrix[iEOF-1, iSpp] <- glmm_aic
  } # end iEOF, lwd 
}  # end iInd

# Convert matrices to data frames for plotting
gam_aic_df <- as.data.frame(gam_aic_matrix)
gam_aic_df$Mode <- rownames(gam_aic_df)
gam_aic_df_melt <- melt(gam_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

glmm_aic_df <- as.data.frame(glmm_aic_matrix)
glmm_aic_df$Mode <- rownames(glmm_aic_df)
glmm_aic_df_melt <- melt(glmm_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

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
ggsave(paste0("../images/AIC_GAM_GlbColour_Shelf15_Season.png"),  width = 6, height = 6, units = "in", dpi = 300) 

# Plot heatmap for GLMM AIC values
glm_AIC <- ggplot(glmm_aic_df_melt, aes(x = Species, y = Mode, fill = AIC)) +
  geom_tile() +
  scale_fill_gradient(low = "royalblue", high = "tomato3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Heatmap of AIC Values for GLMM Models") +
  xlab("Species") +
  ylab("Mode")
print(glm_AIC)
ggsave(paste0("../images/AIC_GLM_GlbColour_Shelf15_Season.png"), width = 6, height = 6, units = "in", dpi = 300) 

# Find the minimum AIC values and their corresponding indices for GAM models
min_gam_aic <- min(gam_aic_matrix, na.rm = TRUE)
best_gam_model <- which(gam_aic_matrix == min_gam_aic, arr.ind = TRUE)
best_gam_model_info <- data.frame(
  Mode = rownames(gam_aic_matrix)[best_gam_model[, 1]],
  Species = colnames(gam_aic_matrix)[best_gam_model[, 2]],
  AIC = min_gam_aic
)

# Find the minimum AIC values and their corresponding indices for GLMM models
min_glmm_aic <- min(glmm_aic_matrix, na.rm = TRUE)
best_glmm_model <- which(glmm_aic_matrix == min_glmm_aic, arr.ind = TRUE)
best_glmm_model_info <- data.frame(
  Mode = rownames(glmm_aic_matrix)[best_glmm_model[, 1]],
  Species = colnames(glmm_aic_matrix)[best_glmm_model[, 2]],
  AIC = min_glmm_aic
)

# Print the best models information
print("Best GAM Model Globcolour Shelf 15 Season :")
print(best_gam_model_info)
print("Best GLMM Model Globcolour Shelf 15 Season:")
print(best_glmm_model_info)

# Find the best GAM model for each species
best_gam_models <- apply(gam_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(gam_aic_matrix)[min_index], AIC = column[min_index])
})

# Find the best GLMM model for each species
best_glmm_models <- apply(glmm_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(glmm_aic_matrix)[min_index], AIC = column[min_index])
})

# Combine the data frames with species and the mode (variable) yielding the best model
best_gam_df <- do.call(rbind, lapply(seq_along(best_gam_models), function(i) {
  data.frame(Species = names(best_gam_models)[i], 
             Mode = best_gam_models[[i]]$Mode, 
             Label = paste(names(best_gam_models)[i], best_gam_models[[i]]$Mode, sep = " - "),
             AIC = best_gam_models[[i]]$AIC)
}))

best_glmm_df <- do.call(rbind, lapply(seq_along(best_glmm_models), function(i) {
  data.frame(Species = names(best_glmm_models)[i], 
             Mode = best_glmm_models[[i]]$Mode, 
             Label = paste(names(best_glmm_models)[i], best_glmm_models[[i]]$Mode, sep = " - "),
             AIC = best_glmm_models[[i]]$AIC)
}))

# Combine the data frames for comparison
best_models_df <- bind_rows(
  best_gam_df %>% mutate(Model = "GAM"),
  best_glmm_df %>% mutate(Model = "GLMM")
)

# Plot the best models with combined labels
best_models_plot <- ggplot(best_models_df, aes(x = AIC, y = Label, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  theme_light() +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Best Models for Each Species Based on AIC") +
  xlab("AIC") +
  ylab("Species - Best Mode")

ggsave(paste0("../images/best_models_GlbColour_Shelf15_Season.png"), width = 5, height = 10, units = "in", dpi = 300) 


#------------------------------------------------------------------
# For Spawning SEASONAL Globcolour 
#------------------------------------------------------------------
graphics.off()

EOFSpSeason_RecDev <- merge(SpSeason, RecDev, by = "X")

# we have 2 seasons and 10 species
itot <- (2)+1

# for results of models
gam_models <- list()
glmm_models <- list()
gam_aic_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSpawnSeason, spNamePlot))
glmm_aic_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSpawnSeason, spNamePlot))

yearsPlot <- unique(EOFSpSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSpSeason_RecDev[,iEOF]
    y <- EOFSpSeason_RecDev[,iSpp+itot]
 
    # Combine x and y into a data frame and remove rows with NAs
    data <- data.frame(x = x, y = y, Year = EOFSpSeason_RecDev$X)
    data <- na.omit(data)
    
    # Fit a GAM model
    gam_model <- gam(y ~ s(x, k=5), data = data)
    
    # Fit a GLMM model
    #   glmm_model <- glmmTMB(y ~ x + (1|Year), data = data)
    glmm_model <- glmmTMB(y ~ x, data = data)
    
    # Calculate AIC for both models
    gam_aic <- AIC(gam_model)
    glmm_aic <- AIC(glmm_model)
    
    # Store the models and AIC values
    gam_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- gam_model
    glmm_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- glmm_model
    gam_aic_matrix[iEOF-1, iSpp] <- gam_aic
    glmm_aic_matrix[iEOF-1, iSpp] <- glmm_aic   
  } # end iEOF, lwd 
}  # end iInd

# Convert matrices to data frames for plotting
gam_aic_df <- as.data.frame(gam_aic_matrix)
gam_aic_df$Mode <- rownames(gam_aic_df)
gam_aic_df_melt <- melt(gam_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

glmm_aic_df <- as.data.frame(glmm_aic_matrix)
glmm_aic_df$Mode <- rownames(glmm_aic_df)
glmm_aic_df_melt <- melt(glmm_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

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
ggsave(paste0("../images/AIC_GAM_GlbColour_Shelf15_SpSeason.png"),  width = 6, height = 6, units = "in", dpi = 300) 

# Plot heatmap for GLMM AIC values
glm_AIC <- ggplot(glmm_aic_df_melt, aes(x = Species, y = Mode, fill = AIC)) +
  geom_tile() +
  scale_fill_gradient(low = "royalblue", high = "tomato3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Heatmap of AIC Values for GLMM Models") +
  xlab("Species") +
  ylab("Mode")
print(glm_AIC)
ggsave(paste0("../images/AIC_GLM_GlbColour_Shelf15_SpSeason.png"), width = 6, height = 6, units = "in", dpi = 300) 

# Find the minimum AIC values and their corresponding indices for GAM models
min_gam_aic <- min(gam_aic_matrix, na.rm = TRUE)
best_gam_model <- which(gam_aic_matrix == min_gam_aic, arr.ind = TRUE)
best_gam_model_info <- data.frame(
  Mode = rownames(gam_aic_matrix)[best_gam_model[, 1]],
  Species = colnames(gam_aic_matrix)[best_gam_model[, 2]],
  AIC = min_gam_aic
)

# Find the minimum AIC values and their corresponding indices for GLMM models
min_glmm_aic <- min(glmm_aic_matrix, na.rm = TRUE)
best_glmm_model <- which(glmm_aic_matrix == min_glmm_aic, arr.ind = TRUE)
best_glmm_model_info <- data.frame(
  Mode = rownames(glmm_aic_matrix)[best_glmm_model[, 1]],
  Species = colnames(glmm_aic_matrix)[best_glmm_model[, 2]],
  AIC = min_glmm_aic
)

# Print the best models information
print("Best GAM Model Globcolour Shelf 15 Spawning Season :")
print(best_gam_model_info)
print("Best GLMM Model Globcolour Shelf 15 Spawning Season:")
print(best_glmm_model_info)

# Find the best GAM model for each species
best_gam_models <- apply(gam_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(gam_aic_matrix)[min_index], AIC = column[min_index])
})

# Find the best GLMM model for each species
best_glmm_models <- apply(glmm_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(glmm_aic_matrix)[min_index], AIC = column[min_index])
})

# Combine the data frames with species and the mode (variable) yielding the best model
best_gam_df <- do.call(rbind, lapply(seq_along(best_gam_models), function(i) {
  data.frame(Species = names(best_gam_models)[i], 
             Mode = best_gam_models[[i]]$Mode, 
             Label = paste(names(best_gam_models)[i], best_gam_models[[i]]$Mode, sep = " - "),
             AIC = best_gam_models[[i]]$AIC)
}))

best_glmm_df <- do.call(rbind, lapply(seq_along(best_glmm_models), function(i) {
  data.frame(Species = names(best_glmm_models)[i], 
             Mode = best_glmm_models[[i]]$Mode, 
             Label = paste(names(best_glmm_models)[i], best_glmm_models[[i]]$Mode, sep = " - "),
             AIC = best_glmm_models[[i]]$AIC)
}))

# Combine the data frames for comparison
best_models_df <- bind_rows(
  best_gam_df %>% mutate(Model = "GAM"),
  best_glmm_df %>% mutate(Model = "GLMM")
)

# Plot the best models with combined labels
best_models_plot <- ggplot(best_models_df, aes(x = AIC, y = Label, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  theme_light() +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Best Models for Each Species Based on AIC") +
  xlab("AIC") +
  ylab("Species - Best Mode")

ggsave(paste0("../images/best_models_GlbColour_Shelf15_SpSeason.png"), width = 5, height = 10, units = "in", dpi = 300) 


#------------------------------------------------------------------
# For seasonal Globcolour 50-300m
#------------------------------------------------------------------
graphics.off()

Season   <- read.csv("Anomaly_Seasonal_ChlShelf50.csv")
EOFSeason_RecDev <- merge(Season, RecDev, by = "X")

# 4 seasons
itot <- (4)+1

# for results of models
gam_models <- list()
glmm_models <- list()
gam_aic_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
glmm_aic_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(EOFSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]

    # Combine x and y into a data frame and remove rows with NAs
    data <- data.frame(x = x, y = y, Year = EOFSeason_RecDev$X)
    data <- na.omit(data)
    
    # Fit a GAM model
    gam_model <- gam(y ~ s(x, k=5), data = data)
    
    # Fit a GLMM model
    #   glmm_model <- glmmTMB(y ~ x + (1|Year), data = data)
    glmm_model <- glmmTMB(y ~ x, data = data)
    
    # Calculate AIC for both models
    gam_aic <- AIC(gam_model)
    glmm_aic <- AIC(glmm_model)
    
    # Store the models and AIC values
    gam_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- gam_model
    glmm_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- glmm_model
    gam_aic_matrix[iEOF-1, iSpp] <- gam_aic
    glmm_aic_matrix[iEOF-1, iSpp] <- glmm_aic  
  } # end iEOF, lwd 
}  # end iInd

# Convert matrices to data frames for plotting
gam_aic_df <- as.data.frame(gam_aic_matrix)
gam_aic_df$Mode <- rownames(gam_aic_df)
gam_aic_df_melt <- melt(gam_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

glmm_aic_df <- as.data.frame(glmm_aic_matrix)
glmm_aic_df$Mode <- rownames(glmm_aic_df)
glmm_aic_df_melt <- melt(glmm_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

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
ggsave(paste0("../images/AIC_GAM_GlbColour_Shelf50_Season.png"),  width = 6, height = 6, units = "in", dpi = 300) 

# Plot heatmap for GLMM AIC values
glm_AIC <- ggplot(glmm_aic_df_melt, aes(x = Species, y = Mode, fill = AIC)) +
  geom_tile() +
  scale_fill_gradient(low = "royalblue", high = "tomato3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Heatmap of AIC Values for GLMM Models") +
  xlab("Species") +
  ylab("Mode")
print(glm_AIC)
ggsave(paste0("../images/AIC_GLM_GlbColour_Shelf50_Season.png"), width = 6, height = 6, units = "in", dpi = 300) 

# Find the minimum AIC values and their corresponding indices for GAM models
min_gam_aic <- min(gam_aic_matrix, na.rm = TRUE)
best_gam_model <- which(gam_aic_matrix == min_gam_aic, arr.ind = TRUE)
best_gam_model_info <- data.frame(
  Mode = rownames(gam_aic_matrix)[best_gam_model[, 1]],
  Species = colnames(gam_aic_matrix)[best_gam_model[, 2]],
  AIC = min_gam_aic
)

# Find the minimum AIC values and their corresponding indices for GLMM models
min_glmm_aic <- min(glmm_aic_matrix, na.rm = TRUE)
best_glmm_model <- which(glmm_aic_matrix == min_glmm_aic, arr.ind = TRUE)
best_glmm_model_info <- data.frame(
  Mode = rownames(glmm_aic_matrix)[best_glmm_model[, 1]],
  Species = colnames(glmm_aic_matrix)[best_glmm_model[, 2]],
  AIC = min_glmm_aic
)

# Print the best models information
print("Best GAM Model Globcolour Shelf 50 Season :")
print(best_gam_model_info)
print("Best GLMM Model Globcolour Shelf 50 Season:")
print(best_glmm_model_info)

# Find the best GAM model for each species
best_gam_models <- apply(gam_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(gam_aic_matrix)[min_index], AIC = column[min_index])
})

# Find the best GLMM model for each species
best_glmm_models <- apply(glmm_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(glmm_aic_matrix)[min_index], AIC = column[min_index])
})

# Combine the data frames with species and the mode (variable) yielding the best model
best_gam_df <- do.call(rbind, lapply(seq_along(best_gam_models), function(i) {
  data.frame(Species = names(best_gam_models)[i], 
             Mode = best_gam_models[[i]]$Mode, 
             Label = paste(names(best_gam_models)[i], best_gam_models[[i]]$Mode, sep = " - "),
             AIC = best_gam_models[[i]]$AIC)
}))

best_glmm_df <- do.call(rbind, lapply(seq_along(best_glmm_models), function(i) {
  data.frame(Species = names(best_glmm_models)[i], 
             Mode = best_glmm_models[[i]]$Mode, 
             Label = paste(names(best_glmm_models)[i], best_glmm_models[[i]]$Mode, sep = " - "),
             AIC = best_glmm_models[[i]]$AIC)
}))

# Combine the data frames for comparison
best_models_df <- bind_rows(
  best_gam_df %>% mutate(Model = "GAM"),
  best_glmm_df %>% mutate(Model = "GLMM")
)

# Plot the best models with combined labels
best_models_plot <- ggplot(best_models_df, aes(x = AIC, y = Label, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  theme_light() +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Best Models for Each Species Based on AIC") +
  xlab("AIC") +
  ylab("Species - Best Mode")

ggsave(paste0("../images/best_models_GlbColour_Shelf50_Season.png"), width = 5, height = 10, units = "in", dpi = 300) 


#------------------------------------------------------------------
# Shelf spawning globcolour 50-300m
#------------------------------------------------------------------
graphics.off()

SpSeason <- read.csv("Anomaly_Seasonal_ChlSpShelf50.csv")
# create a matrix with only dates both have data
EOFSpSeason_RecDev <- merge(SpSeason, RecDev, by = "X")

# we have 2 seasons and 10 species
itot <- (2)+1
# for results of models
gam_models <- list()
glmm_models <- list()
gam_aic_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSpawnSeason, spNamePlot))
glmm_aic_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSpawnSeason, spNamePlot))

yearsPlot <- unique(EOFSpSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSpSeason_RecDev[,iEOF]
    y <- EOFSpSeason_RecDev[,iSpp+itot]

    # Combine x and y into a data frame and remove rows with NAs
    data <- data.frame(x = x, y = y, Year = EOFSpSeason_RecDev$X)
    data <- na.omit(data)
    
    # Fit a GAM model
    gam_model <- gam(y ~ s(x, k=5), data = data)
    
    # Fit a GLMM model
    #   glmm_model <- glmmTMB(y ~ x + (1|Year), data = data)
    glmm_model <- glmmTMB(y ~ x, data = data)
    
    # Calculate AIC for both models
    gam_aic <- AIC(gam_model)
    glmm_aic <- AIC(glmm_model)
    
    # Store the models and AIC values
    gam_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- gam_model
    glmm_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- glmm_model
    gam_aic_matrix[iEOF-1, iSpp] <- gam_aic
    glmm_aic_matrix[iEOF-1, iSpp] <- glmm_aic   
  } # end iEOF, lwd 
}  # end iInd

# Convert matrices to data frames for plotting
gam_aic_df <- as.data.frame(gam_aic_matrix)
gam_aic_df$Mode <- rownames(gam_aic_df)
gam_aic_df_melt <- melt(gam_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

glmm_aic_df <- as.data.frame(glmm_aic_matrix)
glmm_aic_df$Mode <- rownames(glmm_aic_df)
glmm_aic_df_melt <- melt(glmm_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

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
ggsave(paste0("../images/AIC_GAM_GlbColour_Shelf50_SpSeason.png"),  width = 6, height = 6, units = "in", dpi = 300) 

# Plot heatmap for GLMM AIC values
glm_AIC <- ggplot(glmm_aic_df_melt, aes(x = Species, y = Mode, fill = AIC)) +
  geom_tile() +
  scale_fill_gradient(low = "royalblue", high = "tomato3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Heatmap of AIC Values for GLMM Models") +
  xlab("Species") +
  ylab("Mode")
print(glm_AIC)
ggsave(paste0("../images/AIC_GLM_GlbColour_Shelf50_SpSeason.png"), width = 6, height = 6, units = "in", dpi = 300) 

# Find the minimum AIC values and their corresponding indices for GAM models
min_gam_aic <- min(gam_aic_matrix, na.rm = TRUE)
best_gam_model <- which(gam_aic_matrix == min_gam_aic, arr.ind = TRUE)
best_gam_model_info <- data.frame(
  Mode = rownames(gam_aic_matrix)[best_gam_model[, 1]],
  Species = colnames(gam_aic_matrix)[best_gam_model[, 2]],
  AIC = min_gam_aic
)

# Find the minimum AIC values and their corresponding indices for GLMM models
min_glmm_aic <- min(glmm_aic_matrix, na.rm = TRUE)
best_glmm_model <- which(glmm_aic_matrix == min_glmm_aic, arr.ind = TRUE)
best_glmm_model_info <- data.frame(
  Mode = rownames(glmm_aic_matrix)[best_glmm_model[, 1]],
  Species = colnames(glmm_aic_matrix)[best_glmm_model[, 2]],
  AIC = min_glmm_aic
)

# Print the best models information
print("Best GAM Model Globcolour Shelf 50 SpSeason :")
print(best_gam_model_info)
print("Best GLMM Model Globcolour Shelf 50 SpSeason:")
print(best_glmm_model_info)

# Find the best GAM model for each species
best_gam_models <- apply(gam_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(gam_aic_matrix)[min_index], AIC = column[min_index])
})

# Find the best GLMM model for each species
best_glmm_models <- apply(glmm_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(glmm_aic_matrix)[min_index], AIC = column[min_index])
})

# Combine the data frames with species and the mode (variable) yielding the best model
best_gam_df <- do.call(rbind, lapply(seq_along(best_gam_models), function(i) {
  data.frame(Species = names(best_gam_models)[i], 
             Mode = best_gam_models[[i]]$Mode, 
             Label = paste(names(best_gam_models)[i], best_gam_models[[i]]$Mode, sep = " - "),
             AIC = best_gam_models[[i]]$AIC)
}))

best_glmm_df <- do.call(rbind, lapply(seq_along(best_glmm_models), function(i) {
  data.frame(Species = names(best_glmm_models)[i], 
             Mode = best_glmm_models[[i]]$Mode, 
             Label = paste(names(best_glmm_models)[i], best_glmm_models[[i]]$Mode, sep = " - "),
             AIC = best_glmm_models[[i]]$AIC)
}))

# Combine the data frames for comparison
best_models_df <- bind_rows(
  best_gam_df %>% mutate(Model = "GAM"),
  best_glmm_df %>% mutate(Model = "GLMM")
)

# Plot the best models with combined labels
best_models_plot <- ggplot(best_models_df, aes(x = AIC, y = Label, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  theme_light() +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Best Models for Each Species Based on AIC") +
  xlab("AIC") +
  ylab("Species - Best Mode")

ggsave(paste0("../images/best_models_GlbColour_Shelf50_SpSeason.png"), width = 5, height = 10, units = "in", dpi = 300) 










#------------------------------------------------------------------
# For seasonal MODIS 50-300m
#------------------------------------------------------------------
graphics.off()

Season   <- read.csv("Anomaly_Seasonal_ChlModis15.csv")
EOFSeason_RecDev <- merge(Season, RecDev, by = "X")

# 4 seasons
itot <- (4)+1

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(EOFSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]

    # Combine x and y into a data frame and remove rows with NAs
    data <- data.frame(x = x, y = y, Year = EOFSeason_RecDev$X)
    data <- na.omit(data)
    
    # Fit a GAM model
    gam_model <- gam(y ~ s(x, k=5), data = data)
    
    # Fit a GLMM model
    #   glmm_model <- glmmTMB(y ~ x + (1|Year), data = data)
    glmm_model <- glmmTMB(y ~ x, data = data)
    
    # Calculate AIC for both models
    gam_aic <- AIC(gam_model)
    glmm_aic <- AIC(glmm_model)
    
    # Store the models and AIC values
    gam_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- gam_model
    glmm_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- glmm_model
    gam_aic_matrix[iEOF-1, iSpp] <- gam_aic
    glmm_aic_matrix[iEOF-1, iSpp] <- glmm_aic   
  } # end iEOF, lwd 
}  # end iInd

# Convert matrices to data frames for plotting
gam_aic_df <- as.data.frame(gam_aic_matrix)
gam_aic_df$Mode <- rownames(gam_aic_df)
gam_aic_df_melt <- melt(gam_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

glmm_aic_df <- as.data.frame(glmm_aic_matrix)
glmm_aic_df$Mode <- rownames(glmm_aic_df)
glmm_aic_df_melt <- melt(glmm_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

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
ggsave(paste0("../images/AIC_GAM_Modis_Shelf15_Season.png"),  width = 6, height = 6, units = "in", dpi = 300) 

# Plot heatmap for GLMM AIC values
glm_AIC <- ggplot(glmm_aic_df_melt, aes(x = Species, y = Mode, fill = AIC)) +
  geom_tile() +
  scale_fill_gradient(low = "royalblue", high = "tomato3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Heatmap of AIC Values for GLMM Models") +
  xlab("Species") +
  ylab("Mode")
print(glm_AIC)
ggsave(paste0("../images/AIC_GLM_Modis_Shelf15_Season.png"), width = 6, height = 6, units = "in", dpi = 300) 

# Find the minimum AIC values and their corresponding indices for GAM models
min_gam_aic <- min(gam_aic_matrix, na.rm = TRUE)
best_gam_model <- which(gam_aic_matrix == min_gam_aic, arr.ind = TRUE)
best_gam_model_info <- data.frame(
  Mode = rownames(gam_aic_matrix)[best_gam_model[, 1]],
  Species = colnames(gam_aic_matrix)[best_gam_model[, 2]],
  AIC = min_gam_aic
)

# Find the minimum AIC values and their corresponding indices for GLMM models
min_glmm_aic <- min(glmm_aic_matrix, na.rm = TRUE)
best_glmm_model <- which(glmm_aic_matrix == min_glmm_aic, arr.ind = TRUE)
best_glmm_model_info <- data.frame(
  Mode = rownames(glmm_aic_matrix)[best_glmm_model[, 1]],
  Species = colnames(glmm_aic_matrix)[best_glmm_model[, 2]],
  AIC = min_glmm_aic
)

# Print the best models information
print("Best GAM Model Modis Shelf 15 Season :")
print(best_gam_model_info)
print("Best GLMM Model Modis Shelf 15 Season:")
print(best_glmm_model_info)

# Find the best GAM model for each species
best_gam_models <- apply(gam_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(gam_aic_matrix)[min_index], AIC = column[min_index])
})

# Find the best GLMM model for each species
best_glmm_models <- apply(glmm_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(glmm_aic_matrix)[min_index], AIC = column[min_index])
})

# Combine the data frames with species and the mode (variable) yielding the best model
best_gam_df <- do.call(rbind, lapply(seq_along(best_gam_models), function(i) {
  data.frame(Species = names(best_gam_models)[i], 
             Mode = best_gam_models[[i]]$Mode, 
             Label = paste(names(best_gam_models)[i], best_gam_models[[i]]$Mode, sep = " - "),
             AIC = best_gam_models[[i]]$AIC)
}))

best_glmm_df <- do.call(rbind, lapply(seq_along(best_glmm_models), function(i) {
  data.frame(Species = names(best_glmm_models)[i], 
             Mode = best_glmm_models[[i]]$Mode, 
             Label = paste(names(best_glmm_models)[i], best_glmm_models[[i]]$Mode, sep = " - "),
             AIC = best_glmm_models[[i]]$AIC)
}))

# Combine the data frames for comparison
best_models_df <- bind_rows(
  best_gam_df %>% mutate(Model = "GAM"),
  best_glmm_df %>% mutate(Model = "GLMM")
)

# Plot the best models with combined labels
best_models_plot <- ggplot(best_models_df, aes(x = AIC, y = Label, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  theme_light() +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Best Models for Each Species Based on AIC") +
  xlab("AIC") +
  ylab("Species - Best Mode")

ggsave(paste0("../images/best_models_Modis_Shelf15_Season.png"), width = 5, height = 10, units = "in", dpi = 300) 

#------------------------------------------------------------------
# For seasonal MODIS 50-300m
#------------------------------------------------------------------
graphics.off()

Season   <- read.csv("Anomaly_Seasonal_ChlModis50.csv")
EOFSeason_RecDev <- merge(Season, RecDev, by = "X")

# 4 seasons
itot <- (4)+1

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(EOFSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]

    # Combine x and y into a data frame and remove rows with NAs
    data <- data.frame(x = x, y = y, Year = EOFSeason_RecDev$X)
    data <- na.omit(data)
    
    # Fit a GAM model
    gam_model <- gam(y ~ s(x, k=5), data = data)
    
    # Fit a GLMM model
    #   glmm_model <- glmmTMB(y ~ x + (1|Year), data = data)
    glmm_model <- glmmTMB(y ~ x, data = data)
    
    # Calculate AIC for both models
    gam_aic <- AIC(gam_model)
    glmm_aic <- AIC(glmm_model)
    
    # Store the models and AIC values
    gam_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- gam_model
    glmm_models[[paste("EOF", iEOF, "Spp", iSpp, sep = "_")]] <- glmm_model
    gam_aic_matrix[iEOF-1, iSpp] <- gam_aic
    glmm_aic_matrix[iEOF-1, iSpp] <- glmm_aic 
  } # end iEOF, lwd 
}  # end iInd

# Convert matrices to data frames for plotting
gam_aic_df <- as.data.frame(gam_aic_matrix)
gam_aic_df$Mode <- rownames(gam_aic_df)
gam_aic_df_melt <- melt(gam_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

glmm_aic_df <- as.data.frame(glmm_aic_matrix)
glmm_aic_df$Mode <- rownames(glmm_aic_df)
glmm_aic_df_melt <- melt(glmm_aic_df, id.vars = "Mode", variable.name = "Species", value.name = "AIC")

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
ggsave(paste0("../images/AIC_GAM_Modis_Shelf50_Season.png"),  width = 6, height = 6, units = "in", dpi = 300) 

# Plot heatmap for GLMM AIC values
glm_AIC <- ggplot(glmm_aic_df_melt, aes(x = Species, y = Mode, fill = AIC)) +
  geom_tile() +
  scale_fill_gradient(low = "royalblue", high = "tomato3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Heatmap of AIC Values for GLMM Models") +
  xlab("Species") +
  ylab("Mode")
print(glm_AIC)
ggsave(paste0("../images/AIC_GLM_Modis_Shelf50_Season.png"), width = 6, height = 6, units = "in", dpi = 300) 

# Find the minimum AIC values and their corresponding indices for GAM models
min_gam_aic <- min(gam_aic_matrix, na.rm = TRUE)
best_gam_model <- which(gam_aic_matrix == min_gam_aic, arr.ind = TRUE)
best_gam_model_info <- data.frame(
  Mode = rownames(gam_aic_matrix)[best_gam_model[, 1]],
  Species = colnames(gam_aic_matrix)[best_gam_model[, 2]],
  AIC = min_gam_aic
)

# Find the minimum AIC values and their corresponding indices for GLMM models
min_glmm_aic <- min(glmm_aic_matrix, na.rm = TRUE)
best_glmm_model <- which(glmm_aic_matrix == min_glmm_aic, arr.ind = TRUE)
best_glmm_model_info <- data.frame(
  Mode = rownames(glmm_aic_matrix)[best_glmm_model[, 1]],
  Species = colnames(glmm_aic_matrix)[best_glmm_model[, 2]],
  AIC = min_glmm_aic
)

# Print the best models information
print("Best GAM Model Modis Shelf 50 Season :")
print(best_gam_model_info)
print("Best GLMM Model Modis Shelf 50 Season:")
print(best_glmm_model_info)

# Find the best GAM model for each species
best_gam_models <- apply(gam_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(gam_aic_matrix)[min_index], AIC = column[min_index])
})

# Find the best GLMM model for each species
best_glmm_models <- apply(glmm_aic_matrix, 2, function(column) {
  min_index <- which.min(column)
  list(Mode = rownames(glmm_aic_matrix)[min_index], AIC = column[min_index])
})

# Combine the data frames with species and the mode (variable) yielding the best model
best_gam_df <- do.call(rbind, lapply(seq_along(best_gam_models), function(i) {
  data.frame(Species = names(best_gam_models)[i], 
             Mode = best_gam_models[[i]]$Mode, 
             Label = paste(names(best_gam_models)[i], best_gam_models[[i]]$Mode, sep = " - "),
             AIC = best_gam_models[[i]]$AIC)
}))

best_glmm_df <- do.call(rbind, lapply(seq_along(best_glmm_models), function(i) {
  data.frame(Species = names(best_glmm_models)[i], 
             Mode = best_glmm_models[[i]]$Mode, 
             Label = paste(names(best_glmm_models)[i], best_glmm_models[[i]]$Mode, sep = " - "),
             AIC = best_glmm_models[[i]]$AIC)
}))

# Combine the data frames for comparison
best_models_df <- bind_rows(
  best_gam_df %>% mutate(Model = "GAM"),
  best_glmm_df %>% mutate(Model = "GLMM")
)

# Plot the best models with combined labels
best_models_plot <- ggplot(best_models_df, aes(x = AIC, y = Label, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(), show.legend = TRUE) +
  theme_light() +
  theme(axis.text.y = element_text(size = 10)) +
  ggtitle("Best Models for Each Species Based on AIC") +
  xlab("AIC") +
  ylab("Species - Best Mode")

ggsave(paste0("../images/best_models_Modis_Shelf50_Season.png"), width = 5, height = 10, units = "in", dpi = 300) 




