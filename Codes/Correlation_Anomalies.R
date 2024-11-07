# Make correlations 
library(gplots)
library(RColorBrewer)
library(bcp)
library(gridExtra)
library(dplyr)
library(reshape2)
library(ggplot2)
library(wesanderson)

graphics.off()
corr_matrix  <- read.csv("./csv_files/Correlation_Anomalies.csv")

colorPlot <- c('#00449f', '#2858a3', '#3d6ba7', '#4f7fac', '#6192b1', '#74a6b6', '#89b9bc', '#a1ccc3', '#bcdecb', '#dbefd5', '#ffffe0', '#f5f1ce', '#eae4bd', '#e0d6ac', '#d5c99b', '#cabc8a', '#c0af79', '#b5a269', '#aa9659', '#9f8949', '#947d39')

melted_cor_matrix <- melt(corr_matrix)

png(gsub(" ","",paste("./images/Cor_Anomal.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = variable, y = X, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  
  scale_fill_gradientn(colors = colorPlot, na.value = "white") +
  geom_vline(xintercept = c(6.5), color = "darkgray", size = 0.8, 
             linetype = "solid") +
  geom_hline(yintercept = c(4.5, 8.5, 12.5, 16.5, 20.5), 
             color = "darkgray", size = 0.8, linetype = "solid") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
    axis.text.y = element_text(size = 13),
    panel.grid.major = element_line(color = "lightgray", size = 0.5), 
    panel.grid.minor = element_line(color = "lightgray", size = 0.5),
    plot.margin = margin(1, 1, 1, 1, "cm")  
 ) +
  scale_y_discrete(labels = function(x) {
    # Create labels for y-axis based on the seasons
    seasons <- c("Fall", "Spring", "Summer", "Winter")
    rep(seasons, length.out = length(x))
  })

dev.off()

#-------------------------------------------------------
graphics.off()
corr_matrix  <- read.csv("./csv_files/Correlation_All.csv")

colorPlot <- c('#00449f', '#2858a3', '#3d6ba7', '#4f7fac', '#6192b1', '#74a6b6', '#89b9bc', '#a1ccc3', '#bcdecb', '#dbefd5', '#ffffe0', '#f5f1ce', '#eae4bd', '#e0d6ac', '#d5c99b', '#cabc8a', '#c0af79', '#b5a269', '#aa9659', '#9f8949', '#947d39')

melted_cor_matrix <- melt(corr_matrix)

png(gsub(" ","",paste("./images/Cor_All.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = variable, y = X, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  
  scale_fill_gradientn(colors = colorPlot, na.value = "white") +
  geom_vline(xintercept = c(6.5), color = "gray80", size = 0.8, 
             linetype = "solid") +
  geom_hline(yintercept = c(4.5, 8.5, 12.5, 16.5, 20.5, 24.5, 28.5), 
             color = "gray80", size = 0.8, linetype = "solid") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
    axis.text.y = element_text(size = 11),
    panel.grid.major = element_line(color = "lightgray", size = 0.5), 
    panel.grid.minor = element_line(color = "lightgray", size = 0.5),
    plot.margin = margin(1, 1, 1, 1, "cm")  
  ) +
  scale_y_discrete(labels = function(x) {
    # Create labels for y-axis based on the seasons
    seasons <- c("Winter", "Spring", "Summer", "Fall")
    rep(seasons, length.out = length(x))
  })
dev.off()


png(gsub(" ","",paste("./images/Cor_All_test.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = variable, y = X, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  
  scale_fill_gradientn(colors = colorPlot, na.value = "white") +
  geom_vline(xintercept = c(6.5), color = "gray80", size = 0.8, 
             linetype = "solid") +
  geom_hline(yintercept = c(4.5, 8.5, 12.5, 16.5, 20.5, 24.5, 28.5), 
             color = "gray80", size = 0.8, linetype = "solid") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
    axis.text.y = element_text(size = 11),
    panel.grid.major = element_line(color = "lightgray", size = 0.5), 
    panel.grid.minor = element_line(color = "lightgray", size = 0.5),
    plot.margin = margin(1, 1, 1, 1, "cm")  
  )
dev.off()


#-------------------------------------------------------
corr_matrix  <- read.csv("./csv_files/Correlation_UpDeep.csv")

colorPlot <- c('#00449f', '#2858a3', '#3d6ba7', '#4f7fac', '#6192b1', '#74a6b6', '#89b9bc', '#a1ccc3', '#bcdecb', '#dbefd5', '#ffffe0', '#f5f1ce', '#eae4bd', '#e0d6ac', '#d5c99b', '#cabc8a', '#c0af79', '#b5a269', '#aa9659', '#9f8949', '#947d39')

melted_cor_matrix <- melt(corr_matrix)

png(gsub(" ","",paste("./images/Corr_UpSurfrr.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = variable, y = X, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  
  scale_fill_gradientn(colors = colorPlot, na.value = "white") +
  geom_vline(xintercept = c(6.5), color = "darkgray", size = 0.8, 
             linetype = "solid") +
  geom_hline(yintercept = c(4.5), 
             color = "darkgray", size = 0.8, linetype = "solid") +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
    axis.text.y = element_text(size = 13),
    panel.grid.major = element_line(color = "lightgray", size = 0.5), 
    panel.grid.minor = element_line(color = "lightgray", size = 0.5),
    plot.margin = margin(1, 1, 1, 1, "cm")  
  ) 
dev.off()
