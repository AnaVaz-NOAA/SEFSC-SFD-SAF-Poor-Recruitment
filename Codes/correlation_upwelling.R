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
#setwd("./csv_files")
RecDev <- read.csv("RecruitmentResiduals_Feb23.csv")
deepUp <- read.csv("seasonal_deep_upwelling.csv")
surfUp <- read.csv("seasonal_surface_upwelling.csv")

yearsPlot <- 1993:2021
color_palette <- rainbow(length(unique(yearsPlot)))
coloraux = colorRampPalette(brewer.pal(11, "Spectral"))
color_palette = coloraux(length(unique(yearsPlot)))

# it is in alphabetical order
NamePlotSeason <- c("Fall","Spring","Summer","Winter")
spNamePlot <- c("Gag Grouper","Greater Amberjack","Gray Triggerfish"," Red Porgy","Red Grouper","Black Sea Bass","Red Snapper","Scamp","Snowy Grouper ","Vermilion Snapper")

#------------------------------------------------------------------
# For SEASONAL Deep Upwelling
graphics.off()

pcut <- 0.1
# create a matrix with only dates both have data
Up_RecDev <- merge(deepUp, RecDev, by.x = "YR", by.y = "X")

# Loop over Upwelling and calculate correlation with indexes
# we have 4 seasons
itot = 4+1

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(Up_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- Up_RecDev[,iEOF]-mean(Up_RecDev[,iEOF])
    y <- Up_RecDev[,iSpp+itot]
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
        color = as.numeric(factor(Up_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      # Create the first panel with different axes
      plot1 <- ggplot(df, aes(x = yearsPlot)) +
        geom_line(aes(y = x, color = "Upwelling"), linetype = "solid", linewidth = 1.5) +
        geom_line(aes(y = y, color = "RecDev"), linetype = "solid", linewidth = 1.5) +
        geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
        geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        scale_color_manual(values = c("RecDev" = "#F21A00", "Upwelling" = "#3B9AB2")) +
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
          x = "Upwelling Deep",
          y = "RecDev",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
        ) +
        theme_minimal()
      
      # Create a grid of the two plots
      png(gsub(" ","",paste("../images/CorrelationUpDeepSeason/",spNamePlot[iSpp],NamePlotSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
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
png(gsub(" ","",paste("../images/Correlation_UpDeep_Season_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
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
png(gsub(" ","",paste("../images/Correlation_UpDeep_Season_Species_heatmap05.png")),res = 300, width = 2000, height = 2000)
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
# For SEASONAL Surface Upwelling
graphics.off()

pcut <- 0.1
# create a matrix with only dates both have data
Up_RecDev <- merge(surfUp, RecDev, by.x = "YR", by.y = "X")

# Loop over Upwelling and calculate correlation with indexes
# we have 4 seasons
itot = 4+1

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(Up_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- Up_RecDev[,iEOF]-mean(Up_RecDev[,iEOF])
    y <- Up_RecDev[,iSpp+itot]
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
        color = as.numeric(factor(Up_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      # Create the first panel with different axes
      plot1 <- ggplot(df, aes(x = yearsPlot)) +
        geom_line(aes(y = x, color = "Upwelling"), linetype = "solid", linewidth = 1.5) +
        geom_line(aes(y = y, color = "RecDev"), linetype = "solid", linewidth = 1.5) +
        geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
        geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        scale_color_manual(values = c("RecDev" = "#F21A00", "Upwelling" = "#3B9AB2")) +
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
          x = "Upwelling Surf",
          y = "RecDev",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
        ) +
        theme_minimal()
      
      # Create a grid of the two plots
      png(gsub(" ","",paste("../images/CorrelationUpSurfSeason/",spNamePlot[iSpp],NamePlotSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
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
png(gsub(" ","",paste("../images/Correlation_UpSurf_Season_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
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
png(gsub(" ","",paste("../images/Correlation_UpSurf_Season_Species_heatmap05.png")),res = 300, width = 2000, height = 2000)
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


#-----------------------------------------
#-----------------------------------------
# SEASONAL SPAWNING 
#-----------------------------------------
#-----------------------------------------

RecDev <- read.csv("RecruitmentResiduals_Feb23.csv")
deepUp <- read.csv("seasonalSp_deep_upwelling.csv")
surfUp <- read.csv("seasonalSp_surface_upwelling.csv")

yearsPlot <- 1993:2021
color_palette <- rainbow(length(unique(yearsPlot)))
coloraux = colorRampPalette(brewer.pal(11, "Spectral"))
color_palette = coloraux(length(unique(yearsPlot)))

# it is in alphabetical order
NamePlotSeason <- c("Spring","Winter","no")
spNamePlot <- c("Gag Grouper","Greater Amberjack","Gray Triggerfish"," Red Porgy","Red Grouper","Black Sea Bass","Red Snapper","Scamp","Snowy Grouper ","Vermilion Snapper")

#------------------------------------------------------------------
# For SEASONAL SPAWNING Deep Upwelling
graphics.off()

pcut <- 0.1
# create a matrix with only dates both have data
Up_RecDev <- merge(deepUp, RecDev, by.x = "YR", by.y = "X")

# Loop over Upwelling and calculate correlation with indexes
# we have 2 spawning seasons
itot = 3+1

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(Up_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- Up_RecDev[,iEOF]-mean(Up_RecDev[,iEOF])
    y <- Up_RecDev[,iSpp+itot]
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
        color = as.numeric(factor(Up_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      # Create the first panel with different axes
      plot1 <- ggplot(df, aes(x = yearsPlot)) +
        geom_line(aes(y = x, color = "Upwelling"), linetype = "solid", linewidth = 1.5) +
        geom_line(aes(y = y, color = "RecDev"), linetype = "solid", linewidth = 1.5) +
        geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
        geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        scale_color_manual(values = c("RecDev" = "#F21A00", "Upwelling" = "#3B9AB2")) +
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
          x = "Upwelling Deep",
          y = "RecDev",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
        ) +
        theme_minimal()
      
      # Create a grid of the two plots
      png(gsub(" ","",paste("../images/CorrelationUpDeepSeasonSp/",spNamePlot[iSpp],NamePlotSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
      grid.arrange(plot1, plot2, ncol = 1)
      dev.off()
    }
  } # end iEOF, lwd 
}  # end iInd

plot_matrix <- corr_matrix[1:2,]
p_matrix <- p_matrix[1:2,]
plot_matrix[p_matrix > pcut] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("../images/Correlation_UpDeep_SeasonSp_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
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

plot_matrix <- corr_matrix[1:2,]
p_matrix <- p_matrix[1:2,]
plot_matrix[p_matrix > 0.05] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("../images/Correlation_UpDeep_SeasonSp_Species_heatmap05.png")),res = 300, width = 2000, height = 2000)
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
# For SEASONAL SPANWING Surface Upwelling
graphics.off()

pcut <- 0.1
# create a matrix with only dates both have data
Up_RecDev <- merge(surfUp, RecDev, by.x = "YR", by.y = "X")

# Loop over Upwelling and calculate correlation with indexes
itot = 3+1

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(Up_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- Up_RecDev[,iEOF]-mean(Up_RecDev[,iEOF])
    y <- Up_RecDev[,iSpp+itot]
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
        color = as.numeric(factor(Up_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      # Create the first panel with different axes
      plot1 <- ggplot(df, aes(x = yearsPlot)) +
        geom_line(aes(y = x, color = "Upwelling"), linetype = "solid", linewidth = 1.5) +
        geom_line(aes(y = y, color = "RecDev"), linetype = "solid", linewidth = 1.5) +
        geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
        geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        scale_color_manual(values = c("RecDev" = "#F21A00", "Upwelling" = "#3B9AB2")) +
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
          x = "Upwelling Surf",
          y = "RecDev",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
        ) +
        theme_minimal()
      
      # Create a grid of the two plots
      png(gsub(" ","",paste("../images/CorrelationUpSurfSeasonSp/",spNamePlot[iSpp],NamePlotSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
      grid.arrange(plot1, plot2, ncol = 1)
      dev.off()
    }
  } # end iEOF, lwd 
}  # end iInd

plot_matrix <- corr_matrix[1:2,]
p_matrix <- p_matrix[1:2,]
plot_matrix[p_matrix > pcut] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("../images/Correlation_UpSurf_SeasonSp_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
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

plot_matrix <- corr_matrix[1:2,]
p_matrix <- p_matrix[1:2,]
plot_matrix[p_matrix > 0.05] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("../images/Correlation_UpSurf_SeasonSp_Species_heatmap05.png")),res = 300, width = 2000, height = 2000)
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
