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

graphics.off()
# load the file with all covariates and rec devs
CovarSeason_RecDev <- read.csv("./csv_files/CovarSeason_RecDev.csv")

aux <- colnames(CovarSeason_RecDev)
covarNames <- aux[c(2:41)]
spNames <- aux[42:51]

yearsPlot <- 1993:2021
color_palette <- rainbow(length(unique(yearsPlot)))
coloraux = colorRampPalette(brewer.pal(11, "Spectral"))
color_palette = coloraux(length(unique(yearsPlot)))

pcut <- 0.05
# number of covariates 
itot = 5*4 + 3*4 + 2*4 + 1
gam_models <- list()

gam_aic_matrix <- matrix(NA, nrow = itot-1, ncol = 10, 
                         dimnames = list(covarNames, spNames))
yearsPlot <- unique(CovarSeason_RecDev[,1])
pal <- wes_palette("Zissou1", 30, type = "continuous")

# Create directory to save plots
dirName <- paste0("Spearman_Plots_Species")
dir.create(dirName, showWarnings = FALSE)

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames = list(covarNames, spNames))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames = list(covarNames, spNames))

# Loop over species to fit models and create plots
for (iSpp in 1:10) {
  species_name  <- spNames[iSpp]
  spNamePlot <- gsub(" ", "_", species_name)
  
  for (iCovar in 2:itot) {
    covarNamePlot <- covarNames[iCovar - 1]
    
    # Prepare data for plotting
    x <- CovarSeason_RecDev[, iCovar]
    y <- CovarSeason_RecDev[, iSpp + itot]
    
    # Compute Spearman rank correlation
    spearman_corr <- cor.test(x, y, method = "spearman", use = "complete.obs")
    # Save results
    corr_matrix[iCovar-1,iSpp] <- spearman_corr$estimate
    p_matrix[iCovar-1,iSpp] <- spearman_corr$p.value
  }
}

pcut <- 0.1

plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("./images/Correlation_Spearman_all.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "gray90", size = 0.5) +  
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),  
    plot.background = element_rect(fill = "white"), 
    panel.grid.major = element_line(color = "lightgray", size = 0.5),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.5)  
  )
dev.off()


# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, 
                      dimnames = list(covarNames, spNames))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, 
                      dimnames = list(covarNames, spNames))

for (iSpp in 1:10) {
  species_name  <- spNames[iSpp]
  spNamePlot <- gsub(" ", "_", species_name)
  for (iCovar in 2:itot) {
    covarNamePlot <- covarNames[iCovar - 1]
    # Prepare data for plotting
    x <- CovarSeason_RecDev[, iCovar]
    y <- CovarSeason_RecDev[, iSpp + itot]
    # Compute kendall rank correlation
    spearman_corr <- cor.test(x, y, method = "kendall", use = "complete.obs")
    # Save results
    corr_matrix[iCovar-1,iSpp] <- spearman_corr$estimate
    p_matrix[iCovar-1,iSpp] <- spearman_corr$p.value
  }
}

pcut <- 0.1

plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("./images/Correlation_Kendall_all.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color = "gray90", size = 0.5) +  
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),  
    plot.background = element_rect(fill = "white"), 
    panel.grid.major = element_line(color = "lightgray", size = 0.5),  
    panel.grid.minor = element_line(color = "lightgray", size = 0.5)  
  )
dev.off()