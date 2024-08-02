# Make correlations between indexes, Covar and recdev
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
library(corrr)
library(ggcorrplot)
library(factoextra)
library(Hmisc)
# ---------------------------------------------------
# Perform PCA for the covariates

read.csv("./csv_files/CovarSpSeason_RecDev.csv")

# Exclude the last 10 columns (recruitment deviations) and identifier column 'X'
# covariate_columns <- CovarSeason_RecDev[, 1:(51 - 10)]
# Exclude all Chl and species rec devs

covariate_columns <- CovarSeason_RecDev[, c(2:21, 34:41)]

# Replace infinite values with NA
covariate_columns <- covariate_columns %>% mutate_all(~ ifelse(is.infinite(.), NA, .))
covariate_columns_clean <- na.omit(covariate_columns)

# Do PCA
data.pca <- princomp(covariate_columns_clean)
summary(data.pca)

# the first 8 PCAs reflects 95% of the variability
data.pca$loadings[, 1:8]

# save the loadings so it is easier to see it
write.csv(data.pca$loadings[, 1:8], "loadings_pca.csv")

# plot scree 
fviz_eig(data.pca, addlabels = TRUE)
# plot cosine
fviz_cos2(data.pca, choice = "var", axes = 1:2)
# plot loading and cosine
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

# Extract the loadings
loadings <- data.pca$loadings[, 1:2]

# Convert to data frame and add variable names
loadings_df <- as.data.frame(loadings)
loadings_df$variable <- rownames(loadings_df)

# Find top 5 variables for PC1 and PC2 by absolute value
top_PC1 <- loadings_df[order(abs(loadings_df$Comp.1), decreasing = TRUE)[1:5], ]
top_PC2 <- loadings_df[order(abs(loadings_df$Comp.2), decreasing = TRUE)[1:5], ]

# Combine the top variables for both PCs
top_loadings <- unique(rbind(top_PC1, top_PC2))

# Plot the top loadings for PC1 and PC2
fviz_pca_var(data.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             select.var = list(name = top_loadings$variable),
             repel = TRUE)

# Define the order of variables and seasons
variables <- c("Bottom.Temp", "Mixed.Layer", "Salinity", "SSH", "SST", "deepGulf", "surfGulf")
seasons <- c("Fall", "Spring", "Summer", "Winter")

# Create a vector of labels in the correct order
labels <- c()
for (var in variables) {
  for (sea in seasons) {
    labels <- c(labels, paste(var, sea, sep = "."))
  }
}

# Create the loadings data frame
loadings <- data.frame(
  PC1 = data.pca$loadings[, 1],
  PC2 = data.pca$loadings[, 2],
  variable = rep(variables, each = length(seasons)),
  season = rep(seasons, times = length(variables))
)

# Map different variables to different shapes and seasons to different colors
variable_shapes <- c("Bottom.Temp" = 15, "SST" = 17, "SSH" = 18, "Mixed.Layer" = 19, "Salinity" = 8, "deepGulf" = 21, "surfGulf" = 22)
season_colors <- c("Summer" = "orchid", "Fall" = "orange", "Winter" = "cadetblue2", "Spring" = "seagreen3")

# Create the PCA plot
p <- ggplot(loadings, aes(x = PC1, y = PC2, shape = variable, color = season)) +
  geom_point(size = 6, alpha = 0.8) +  # Points for the loadings
  scale_shape_manual(values = variable_shapes) +  # Customize shapes
  scale_color_manual(values = season_colors) +  # Customize colors
  theme_light() +
  labs(
    title = "PCA Loadings Plot",
    x = "PC1",
    y = "PC2",
    shape = "Variable",
    color = "Season"
  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
# Print the plot
print(p)
ggsave("./images/PCAs_covariates.png", plot = p, width = 6, height = 6, units = "in", dpi = 300, bg = "white")
#---------------------------------------------------------------------------
# correlation 
cor_results <- rcorr(as.matrix(covariate_columns_clean))
cor_matrix <- cor_results$r
p_matrix <- cor_results$P

# remove p < 5%
alpha <- 0.05
cor_matrix[p_matrix > alpha] <- NA

# Melt the correlation matrix for ggplot
melted_cor_matrix <- melt(cor_matrix)

# Plot the heatmap
plotmatrix <- ggplot(data = melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name = "Correlation") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
ggsave("./images/correlation_covariates.png", plot = plotmatrix, width = 10, height = 10, units = "in", dpi = 300, bg = "white")