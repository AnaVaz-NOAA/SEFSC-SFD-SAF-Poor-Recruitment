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
# load recdevs, indexes and EOF
# set the diretory with the assessment files
#setwd("./csv_files")
RecDev      <- read.csv("RecruitmentResiduals_Feb23.csv")
Season   <- read.csv("Anomaly_Seasonal.csv")
SpSeason <- read.csv("Anomaly_Spawning_Seasonal.csv")

yearsPlot <- 1993:2021
color_palette <- rainbow(length(unique(yearsPlot)))
coloraux = colorRampPalette(brewer.pal(11, "Spectral"))
color_palette = coloraux(length(unique(yearsPlot)))

# it is in alphabetical order
NamePlot <- c("Bottom T","Mixed Layer","Salinity","SSH","SST")
NameSeason <- c("Fall","Spring","Summer","Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModesSeason <- paste(aux$Var2, aux$Var1)

# it is in alphabetical order
NameSeason <- c("Summer","Winter")
# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModesSpawnSeason <- paste(aux$Var2, aux$Var1)

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

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotModesSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotModesSeason, spNamePlot))

yearsPlot <- unique(EOFSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]
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
      color = as.numeric(factor(EOFSeason_RecDev[,1]))
    )
    df <- df[complete.cases(df), ]
    
    # Create the first panel with different axes
    plot1 <- ggplot(df, aes(x = yearsPlot)) +
      geom_line(aes(y = x, color = "Anomaly"), linetype = "solid", linewidth = 1.5) +
      geom_line(aes(y = y, color = "Recruitment"), linetype = "solid", linewidth = 1.5) +
      geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
      geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      scale_color_manual(values = c("Anomaly" = "tomato3", "Recruitment" = "dodgerblue3")) +
      labs(
        title = paste(NamePlotModesSeason[iEOF-1], spNamePlot[iSpp]),
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
        x = "Anomaly",
        y = "Recruitment",
        title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
      ) +
      theme_minimal()
    
    # Create a grid of the two plots
    png(gsub(" ","",paste("../images/CorrelationAnomlSeason/",spNamePlot[iSpp],NamePlotModesSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
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
png(gsub(" ","",paste("../images/Correlation_Anomal_Season_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
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
EOFSpSeason_RecDev <- merge(SpSeason, RecDev, by = "X")

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
    
    if (aux$p.value < pcut){
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
      geom_line(aes(y = x, color = "Anomaly"), linetype = "solid", linewidth = 1.5) +
      geom_line(aes(y = y, color = "Recruitment"), linetype = "solid", linewidth = 1.5) +
      geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
      geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      scale_color_manual(values = c("Anomaly" = "tomato3", "Recruitment" = "dodgerblue3")) +
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
        x = "Anomaly",
        y = "Residuals",
        title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
      ) +
      theme_minimal()
    
    # Create a grid of the two plots
    png(gsub(" ","",paste("../images/CorrelationAnomalSeasonSp/",spNamePlot[iSpp],NamePlotModesSpawnSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
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
png(gsub(" ","",paste("../images/Correlation_Anomal_SpSeason_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
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
NamePlotModesSeason <- paste(aux$Var2, aux$Var1)

# it is in alphabetical order
NameSeason <- c("Summer","Winter")
# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotModesSpawnSeason <- paste(aux$Var2, aux$Var1)

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

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotModesSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotModesSeason, spNamePlot))

yearsPlot <- unique(EOFSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]
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
        color = as.numeric(factor(EOFSeason_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      # Create the first panel with different axes
      plot1 <- ggplot(df, aes(x = yearsPlot)) +
        geom_line(aes(y = x, color = "Anomaly Chl"), linetype = "solid", linewidth = 1.5) +
        geom_line(aes(y = y, color = "Recruitment"), linetype = "solid", linewidth = 1.5) +
        geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
        geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        scale_color_manual(values = c("Anomaly Chl" = "tomato3", "Recruitment" = "dodgerblue3")) +
        labs(
          title = paste(NamePlotModesSeason[iEOF-1], spNamePlot[iSpp]),
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
          x = "Anomaly Chl",
          y = "Recruitment",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
        ) +
        theme_minimal()
      
      # Create a grid of the two plots
      png(gsub(" ","",paste("../images/CorrelationAnomlChlSeason15/",spNamePlot[iSpp],NamePlotModesSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
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
png(gsub(" ","",paste("../images/Correlation_Anoml_ChlShelf15_Rect.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
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
# For Spawning SEASONAL Globcolour 
#------------------------------------------------------------------
graphics.off()

EOFSpSeason_RecDev <- merge(SpSeason, RecDev, by = "X")

# we have 2 seasons and 10 species
itot <- (2)+1

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
    
    if (aux$p.value < pcut){
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
        geom_line(aes(y = x, color = "Anomaly"), linetype = "solid", linewidth = 1.5) +
        geom_line(aes(y = y, color = "Recruitment"), linetype = "solid", linewidth = 1.5) +
        geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
        geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        scale_color_manual(values = c("Anomaly" = "tomato3", "Recruitment" = "dodgerblue3")) +
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
          x = "Anomaly",
          y = "Residuals",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
        ) +
        theme_minimal()
      
      # Create a grid of the two plots
      png(gsub(" ","",paste("../images/CorrelationAnomlChlSpSeason15/",spNamePlot[iSpp],NamePlotModesSpawnSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
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
png(gsub(" ","",paste("../images/Correlation_Anoml_ChlSpShelf15_Rect.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
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
# For seasonal Globcolour 50-300m
#------------------------------------------------------------------
graphics.off()

Season   <- read.csv("Anomaly_Seasonal_ChlShelf50.csv")
EOFSeason_RecDev <- merge(Season, RecDev, by = "X")

# 4 seasons
itot <- (4)+1

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotModesSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotModesSeason, spNamePlot))

yearsPlot <- unique(EOFSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iEOF-1,iSpp] <- aux$estimate
    p_matrix[iEOF-1,iSpp] <- aux$p.value
    
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]
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
        color = as.numeric(factor(EOFSeason_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      # Create the first panel with different axes
      plot1 <- ggplot(df, aes(x = yearsPlot)) +
        geom_line(aes(y = x, color = "Anomaly"), linetype = "solid", linewidth = 1.5) +
        geom_line(aes(y = y, color = "Recruitment"), linetype = "solid", linewidth = 1.5) +
        geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
        geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        scale_color_manual(values = c("Anomaly" = "tomato3", "Recruitment" = "dodgerblue3")) +
        labs(
          title = paste(NamePlotModesSeason[iEOF-1], spNamePlot[iSpp]),
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
          x = "Anomaly",
          y = "Residuals",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
        ) +
        theme_minimal()
      
      # Create a grid of the two plots
      png(gsub(" ","",paste("../images/CorrelationAnomlChlSeason50/",spNamePlot[iSpp],NamePlotModesSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
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
png(gsub(" ","",paste("../images/Correlation_Anoml_ChlShelf50_Rect.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
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
# Shelf spawning globcolour 50-300m
#------------------------------------------------------------------
graphics.off()

SpSeason <- read.csv("Anomaly_Seasonal_ChlSpShelf50.csv")
# create a matrix with only dates both have data
EOFSpSeason_RecDev <- merge(SpSeason, RecDev, by = "X")

# we have 2 seasons and 10 species
itot <- (2)+1

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
    
    if (aux$p.value < pcut){
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
        geom_line(aes(y = x, color = "Anomaly"), linetype = "solid", linewidth = 1.5) +
        geom_line(aes(y = y, color = "Recruitment"), linetype = "solid", linewidth = 1.5) +
        geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
        geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        scale_color_manual(values = c("Anomaly" = "tomato3", "Recruitment" = "dodgerblue3")) +
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
          x = "Anomaly",
          y = "Residuals",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
        ) +
        theme_minimal()
      
      # Create a grid of the two plots
      png(gsub(" ","",paste("../images/CorrelationAnomlChlSpSeason50/",spNamePlot[iSpp],NamePlotModesSpawnSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
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
png(gsub(" ","",paste("../images/Correlation_Anoml_ChlSpShelf50_Rect.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
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
# For seasonal MODIS 50-300m
#------------------------------------------------------------------
graphics.off()

Season   <- read.csv("Anomaly_Seasonal_ChlModis15.csv")
EOFSeason_RecDev <- merge(Season, RecDev, by = "X")

# 4 seasons
itot <- (4)+1

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotModesSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotModesSeason, spNamePlot))

yearsPlot <- unique(EOFSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iEOF-1,iSpp] <- aux$estimate
    p_matrix[iEOF-1,iSpp] <- aux$p.value
    
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]
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
        color = as.numeric(factor(EOFSeason_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      # Create the first panel with different axes
      plot1 <- ggplot(df, aes(x = yearsPlot)) +
        geom_line(aes(y = x, color = "Anomaly"), linetype = "solid", linewidth = 1.5) +
        geom_line(aes(y = y, color = "Recruitment"), linetype = "solid", linewidth = 1.5) +
        geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
        geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        scale_color_manual(values = c("Anomaly" = "tomato3", "Recruitment" = "dodgerblue3")) +
        labs(
          title = paste(NamePlotModesSeason[iEOF-1], spNamePlot[iSpp]),
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
          x = "Anomaly",
          y = "Residuals",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
        ) +
        theme_minimal()
      
      # Create a grid of the two plots
      png(gsub(" ","",paste("../images/CorrelationAnomlModisChlSeason15/",spNamePlot[iSpp],NamePlotModesSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
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
png(gsub(" ","",paste("../images/Correlation_Anoml_Modis15.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
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
# For seasonal MODIS 50-300m
#------------------------------------------------------------------
graphics.off()

Season   <- read.csv("Anomaly_Seasonal_ChlModis50.csv")
EOFSeason_RecDev <- merge(Season, RecDev, by = "X")

# 4 seasons
itot <- (4)+1

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotModesSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotModesSeason, spNamePlot))

yearsPlot <- unique(EOFSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iEOF-1,iSpp] <- aux$estimate
    p_matrix[iEOF-1,iSpp] <- aux$p.value
    
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]
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
        color = as.numeric(factor(EOFSeason_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      # Create the first panel with different axes
      plot1 <- ggplot(df, aes(x = yearsPlot)) +
        geom_line(aes(y = x, color = "Anomaly"), linetype = "solid", linewidth = 1.5) +
        geom_line(aes(y = y, color = "Recruitment"), linetype = "solid", linewidth = 1.5) +
        geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
        geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                  x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
        scale_color_manual(values = c("Anomaly" = "tomato3", "Recruitment" = "dodgerblue3")) +
        labs(
          title = paste(NamePlotModesSeason[iEOF-1], spNamePlot[iSpp]),
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
          x = "Anomaly",
          y = "Residuals",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
        ) +
        theme_minimal()
      
      # Create a grid of the two plots
      png(gsub(" ","",paste("../images/CorrelationAnomlChlModisSeason50/",spNamePlot[iSpp],NamePlotModesSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
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
png(gsub(" ","",paste("../images/Correlation_Anoml_Modis50.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),  # Set the background color to white
    plot.background = element_rect(fill = "white"), # Set the plot background to white
    panel.grid.major = element_line(color = "lightgray", size = 0.5),  # Add light gray grid lines
    panel.grid.minor = element_line(color = "lightgray", size = 0.5)  # Remove minor grid lines
  )
dev.off()