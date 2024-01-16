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
library(tidyr)

graphics.off()
# load recdevs, indexes and EOF
# set the diretory with the assessment files
setwd("./csv_files")
RecDev <- read.csv("RecruitmentResiduals_Feb23.csv")
Upw    <- read.csv("upwelling.csv")

yearsPlot <- 1993:2021
color_palette <- rainbow(length(unique(yearsPlot)))
coloraux = colorRampPalette(brewer.pal(11, "Spectral"))
color_palette = coloraux(length(unique(yearsPlot)))

# it is in alphabetical order
NameSeason <- c("Winter","Spring","Summer","Fall")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotSeason <- paste(aux$Var1, aux$Var2)

spNamePlot <- c("Gag Grouper","Greater Amberjack","Gray Triggerfish"," Red Porgy","Red Grouper","Black Sea Bass","Red Snapper","Scamp","Snowy Grouper ","Vermilion Snapper")

# Create a new column for seasons
Upw$season <- ifelse(Upw$MON %in% c(3, 4, 5), "Spring",
                           ifelse(Upw$MON %in% c(6, 7, 8), "Summer",
                                  ifelse(Upw$MON %in% c(9, 10, 11), "Fall", "Winter")))
seasonal_averages <- Upw %>%
  group_by(YR, season) %>%
  summarise(avg_UISurf  = mean(UISurf))

# Pivot the data to wide format
seasonal_wide <- seasonal_averages %>%
  pivot_wider(names_from = season, values_from = avg_UISurf)

# Write the result to a new CSV file
write.csv(seasonal_wide, "seasonal_averages_wide.csv", row.names = FALSE)

# Print the resulting data frame
print(seasonal_averages)
# Extract the YEAR column and other numeric columns
years <- CHL$YEAR
data <- CHL[, -1]  # Exclude the YEAR column

# Plot all columns
matplot(years, data, type = "l", lty = 1, col = 1:ncol(data), xlab = "Year", ylab = "Value", main = "CHL Averages")

# Add legend
legend("topright", legend = colnames(data), col = 1:ncol(data), lty = 1)

#------------------------------------------------------------------
# For SEASONAL
graphics.off()

pcut <- 0.1
# create a matrix with only dates both have data
CHL_RecDev <- merge(CHL, RecDev, by.x = "YEAR", by.y = "X")

# Loop over CHL and calculate correlation with indexes
# we have 5 seasons for 3 depths and 3 regions (15 + 15 = 30)
itot = 5*6+1

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(CHL_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- CHL_RecDev[,iEOF]
    y <- CHL_RecDev[,iSpp+itot]
    # correlation and save coef. and p
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iEOF-1,iSpp] <- aux$estimate
    p_matrix[iEOF-1,iSpp] <- aux$p.value
    
    if (aux$p.value < pcut){
    # TEST WITH GGPLOT
    # Create a data frame with your data
      df <- data.frame(
        yearsPlot = yearsPlot,
        x = x,
        y = y,
        estimate = aux$estimate,
        p.value = aux$p.value,
        color = as.numeric(factor(CHL_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      # Create the first panel with different axes
      plot1 <- ggplot(df, aes(x = yearsPlot)) +
        geom_line(aes(y = x, color = "CHL"), linetype = "solid", linewidth = 1.5) +
        geom_line(aes(y = y, color = "RecDev"), linetype = "solid", linewidth = 1.5) +
        geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
        geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        scale_color_manual(values = c("RecDev" = "#F21A00", "CHL" = "#3B9AB2")) +
        labs(
          title = paste(NamePlotSeason[iEOF-1], spNamePlot[iSpp]),
          y = NULL,
          x = NULL
        ) +
        theme_minimal() +
        theme(
          legend.position = "top",
          legend.title = element_blank()
        )
      pal <- wes_palette("Zissou1", 30, type = "continuous")
      # Create the second panel with a color gradient
      plot2 <- ggplot(df, aes(x = x, y = y)) +
        geom_point(aes(colour = yearsPlot),  size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dashed", se = FALSE) +
        geom_ribbon(
          aes(ymin = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "lwr"],
              ymax = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "upr"]),
          fill = "gray80", alpha = 0.5  # Adjust the alpha value here
        ) +
        scale_color_gradientn(colors = pal, name = "") +
        labs(
          x = "CHL",
          y = "RecDev",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
        ) +
        theme_minimal()
      
      # Create a grid of the two plots
      png(gsub(" ","",paste("../images/CorrelationCHLSeason/",spNamePlot[iSpp],NamePlotSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
      grid.arrange(plot1, plot2, ncol = 1)
      dev.off()
    }
  } # end iEOF, lwd 
}  # end iInd

plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("../images/Correlation_CHL_Season_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "#3B9AB2", mid = "white", high = "#F21A00", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),  # Set the background color to white
    plot.background = element_rect(fill = "white"), # Set the plot background to white
    panel.grid.major = element_line(color = "lightgray", size = 0.5),  # Add light gray grid lines
    panel.grid.minor = element_line(color = "lightgray", size = 0.5)  # Remove minor grid lines
  )
dev.off()

plot_matrix <- corr_matrix
plot_matrix[p_matrix > 0.05] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("../images/Correlation_CHL_Season_Species_heatmap05.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "#3B9AB2", mid = "white", high = "#F21A00", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),  # Set the background color to white
    plot.background = element_rect(fill = "white"), # Set the plot background to white
    panel.grid.major = element_line(color = "lightgray", size = 0.5),  # Add light gray grid lines
    panel.grid.minor = element_line(color = "lightgray", size = 0.5)  # Remove minor grid lines
  )
dev.off()

#------------------------------------------------------------------
# For Spawning SEASONAL
graphics.off()

# create a matrix with only dates both have data
EOFSpSeason_RecDev <- merge(EOFSpSeason, RecDev, by = "X")

# we have 5 Anomal variables, vs 2 seasons (10) and 10 species
itot <- (5*2)+1

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotModesSpawnSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotModesSpawnSeason, spNamePlot))

yearsPlot <- unique(EOFSpSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSpSeason_RecDev[,iEOF]
    y <- EOFSpSeason_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iEOF-1,iSpp] <- aux$estimate
    p_matrix[iEOF-1,iSpp] <- aux$p.value
    
    # correlation and save coef. and p
    x <- EOFSpSeason_RecDev[,iEOF]
    y <- EOFSpSeason_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iEOF-1,iSpp] <- aux$estimate
    p_matrix[iEOF-1,iSpp] <- aux$p.value
    # TEST WITH GGPLOT
    # Create a data frame with your data
    df <- data.frame(
      yearsPlot = yearsPlot,
      x = x,
      y = y,
      estimate = aux$estimate,
      p.value = aux$p.value,
      color = as.numeric(factor(EOFSpSeason_RecDev[,1]))
    )
    df <- df[complete.cases(df), ]
    
    # Create the first panel with different axes
    plot1 <- ggplot(df, aes(x = yearsPlot)) +
      geom_line(aes(y = x, color = "Var 1"), linetype = "solid", linewidth = 1.5) +
      geom_line(aes(y = y, color = "Var 2"), linetype = "solid", linewidth = 1.5) +
      geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
      geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      scale_color_manual(values = c("Var 1" = "#F21A00", "Var 2" = "#3B9AB2")) +
      labs(
        title = paste(NamePlotModesSpawnSeason[iEOF-1], spNamePlot[iSpp]),
        y = NULL,
        x = NULL
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.title = element_blank()
      )
    pal <- wes_palette("Zissou1", 30, type = "continuous")
    # Create the second panel with a color gradient
    plot2 <- ggplot(df, aes(x = x, y = y)) +
      geom_point(aes(colour = yearsPlot),  size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dashed", se = FALSE) +
      geom_ribbon(
        aes(ymin = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "lwr"],
            ymax = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "upr"]),
        fill = "gray80", alpha = 0.5  # Adjust the alpha value here
      ) +
      scale_color_gradientn(colors = pal, name = "") +
      labs(
        x = "PCAs",
        y = "Residuals",
        title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
      ) +
      theme_minimal()
    
    # Create a grid of the two plots
    png(gsub(" ","",paste("../images/CorrelationAnomalSeasonSp/",spNamePlot[iSpp],NamePlotModesSpawnSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
    grid.arrange(plot1, plot2, ncol = 1)
    dev.off()
  } # end iEOF, lwd 
}  # end iInd
    
plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("../images/Correlation_Anomal_SpSeason_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "#3B9AB2", mid = "white", high = "red", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),  # Set the background color to white
    plot.background = element_rect(fill = "white"), # Set the plot background to white
    panel.grid.major = element_line(color = "lightgray", size = 0.5),  # Add light gray grid lines
    panel.grid.minor = element_line(color = "lightgray", size = 0.5)  # Remove minor grid lines
  )
dev.off()
