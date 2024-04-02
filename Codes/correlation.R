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
setwd("./csv_files")
EOFs        <- read.csv("EOF_CNAPS.csv")
Indexes     <- read.csv("indexes_feb23.csv")
RecDev      <- read.csv("RecruitmentResiduals_Feb23.csv")
EOFSeason   <- read.csv("EOF_Seasonal_CNAPS.csv")
EOFSpSeason <- read.csv("EOF_Spawning_Seasonal_CNAPS.csv")

# Extract year and month fractions
Indexes <- Indexes %>%
  mutate(
    Year = floor(X),                     # Extract year
    MonthFraction = (X - floor(X)) * 12  # Extract month fractions (1/12 increments)
  )

# Group by year and calculate the mean NAO for the first 3 measurements
NAOYear <- Indexes %>%
  group_by(Year) %>%
  filter(row_number() <= 3) %>%           # Select the first 3 measurements of each year
  summarize(AvgNAO = mean(NAO, na.rm = TRUE))

yearsPlot <- 1998:2020
color_palette <- rainbow(length(unique(yearsPlot)))
coloraux = colorRampPalette(brewer.pal(11, "Spectral"))
color_palette = coloraux(length(unique(yearsPlot)))

# it is in alphabetical order
NamePlot <- c("Bottom T","Mixed Layer","Salinity","SSH","SST")
NameSeason <- c("Fall","Spring","Summer","Winter")
NameModes  <- c("Mode 1","Mode 2")

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeason,NamePlot)
NamePlotModesSeason <- paste(aux$Var3, aux$Var2, aux$Var1)

# it is in alphabetical order
NameSeason <- c("Summer","Winter")
# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeason,NamePlot)
NamePlotModesSpawnSeason <- paste(aux$Var3, aux$Var2, aux$Var1)

# create combinations of names and combine the names
aux <- expand.grid(NamePlot, NameModes)
NamePlotModes <- paste(aux$Var1, aux$Var2)

spNamePlot <- c("Gag Grouper","Greater Amberjack","Gray Triggerfish"," Red Porgy","Red Grouper","Black Sea Bass","Red Snapper","Scamp","Snowy Grouper ","Vermilion Snapper")

# create a matrix with only dates both have data
EOF_Indexes <- merge(EOFs, Indexes, by = "X")

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=10, ncol=3, dimnames=list(NamePlotModes, c("AMO","MEI","NAO")))
p_matrix    <- matrix(NA, nrow=10, ncol=3, dimnames=list(NamePlotModes, c("AMO","MEI","NAO")))

# Loop over EOFs and calculate correlation with indexes
# we have 5 EOF variables, vs 2 modes (8) and 3 indexes
for (iInd in 1:3) {
  for (iEOF in 2:10) {
      # correlation and save coef. and p
      aux <- cor.test(EOF_Indexes[,iEOF], EOF_Indexes[,iInd+10])
      corr_matrix[iEOF,iInd] <- aux$estimate
      p_matrix[iEOF,iInd] <- aux$p.value
  } # end iEOF
}  # end iInd

plot_matrix <- corr_matrix
plot_matrix[p_matrix > 0.05] <- NaN

# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

png("../images/Correlation_index_EOF_heatmap.png",res = 300,width=2000,height=2000)
heatmap.2(plot_matrix, 
          col=colorPlot, 
          Rowv=NA, 
          Colv=NA, 
          dendrogram="none", 
          scale="column", 
          trace="none", 
          key=TRUE,
          keysize=1.5, 
          margins=c(10,10), 
          xlab="Climate Indexes", 
          ylab="")
dev.off()


#------------------------------------------------------------------
# For SEASONAL
graphics.off()

pcut <- 0.05
# create a matrix with only dates both have data
EOFSeason_RecDev <- merge(EOFSeason, EOFSeason, by = "X")

# Loop over EOFs and calculate correlation with indexes
# we have 5 EOF variables, vs 2 modes, vs 4 seasons (40) and 40 EOFs
itot = 5*2*4+1

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=40, dimnames=list(NamePlotModesSeason, NamePlotModesSeason))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=40, dimnames=list(NamePlotModesSeason, NamePlotModesSeason))

yearsPlot <- unique(EOFSeason_RecDev[,1])

for (iSpp in 1:itot) {
  for (iEOF in 2:itot) {
    # correlation and save coef. and p
    x <- EOFSeason_RecDev[,iEOF]
    y <- EOFSeason_RecDev[,iSpp+itot]
    # correlation and save coef. and p
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
      color = as.numeric(factor(EOFSeason_RecDev[,1]))
    )
    df <- df[complete.cases(df), ]
    
    # Create the first panel with different axes
    plot1 <- ggplot(df, aes(x = yearsPlot)) +
      geom_line(aes(y = x, color = "PCA 1"), linetype = "solid", linewidth = 1.5) +
      geom_line(aes(y = y, color = "PCA 2"), linetype = "solid", linewidth = 1.5) +
      geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
      geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      scale_color_manual(values = c("PCA 1" = "#F21A00", "PCA 2" = "#3B9AB2")) +
      labs(
        title = paste(NamePlotModesSeason[iEOF-1], NamePlotModesSeason[iSpp]),
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
        x = "PCAs 1",
        y = "PCAs 2",
        title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
      ) +
      theme_minimal()
    
    # Create a grid of the two plots
    png(gsub(" ","",paste("../images/CorrelationModesSeason/",NamePlotModesSeason[iSpp],NamePlotModesSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
    grid.arrange(plot1, plot2, ncol = 1)
    dev.off()
  } # end iEOF, lwd 
}  # end iInd

plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

png(gsub(" ","",paste("../images/Correlation_EOF_Season.png")),res = 300, width = 2000, height = 2000)
heatmap.2(plot_matrix,
          col  = colorPlot, 
          Rowv = NA, 
          Colv = NA, 
          margins = c(10,10), 
          xlab= "", 
          ylab = "")
dev.off()

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("../images/Correlation_EOF_Season2.png")),res = 300, width = 2000, height = 2000)
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
# For ANNUAL NAO
graphics.off()

pcut <- 0.05
itot = 10

# for results of EOF vs indexes
corr_matrix <- matrix(NA, nrow=itot, ncol=1, dimnames=list(spNamePlot))
p_matrix    <- matrix(NA, nrow=itot, ncol=1, dimnames=list(spNamePlot))

yearsPlot <- NAOYear$Year

for (iSpp in 2:11) {
  # correlation and save coef. and p
  x <- NAOYear$AvgNAO
  y <- RecDev[,iSpp]
  # correlation and save coef. and p
  aux <- cor.test(x, y, use = "complete.obs")
  corr_matrix[iSpp-1] <- aux$estimate
  p_matrix[iSpp-1] <- aux$p.value
    
  # TEST WITH GGPLOT
  # Create a data frame with your data
    df <- data.frame(
      yearsPlot = yearsPlot,
      x = x,
      y = y,
      estimate = aux$estimate,
      p.value = aux$p.value,
      color = as.numeric(factor(NAOYear$Year))
    )
    df <- df[complete.cases(df), ]
    
    # Create the first panel with different axes
    plot1 <- ggplot(df, aes(x = yearsPlot)) +
      geom_line(aes(y = x, color = "NAO Year"), linetype = "solid", linewidth = 1.5) +
      geom_line(aes(y = y, color = "Residuals"), linetype = "solid", linewidth = 1.5) +
      geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
      geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      scale_color_manual(values = c("NAO Year" = "#F21A00", "Residuals" = "#3B9AB2")) +
      labs(
        title = paste("NAO ", spNamePlot[iSpp-1]),
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
        x = "NAO Year",
        y = "Residuals",
        title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
      ) +
      theme_minimal()
    
    # Create a grid of the two plots
    png(gsub(" ","",paste("../images/Correlations/NAOYear_",spNamePlot[iSpp-1],".png")),res = 300, width = 2000, height = 2000)
    grid.arrange(plot1, plot2, ncol = 1)
    dev.off()
}  # end iInd


#------------------------------------------------------------------
# For SEASONAL
graphics.off()

pcut <- 0.05
# create a matrix with only dates both have data
EOFSeason_RecDev <- merge(EOFSeason, RecDev, by = "X")

# Loop over EOFs and calculate correlation with indexes
# we have 5 EOF variables, vs 2 modes, vs 4 seasons (32) and 10 species
itot = 5*2*4+1

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
      geom_line(aes(y = x, color = "PCA"), linetype = "solid", linewidth = 1.5) +
      geom_line(aes(y = y, color = "Residuals"), linetype = "solid", linewidth = 1.5) +
      geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
      geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      scale_color_manual(values = c("PCA" = "#F21A00", "Residuals" = "#3B9AB2")) +
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
        x = "PCAs",
        y = "Residuals",
        title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
      ) +
      theme_minimal()
    
    # Create a grid of the two plots
    png(gsub(" ","",paste("../images/CorrelationEOFSeason/",spNamePlot[iSpp],NamePlotModesSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
    grid.arrange(plot1, plot2, ncol = 1)
    dev.off()
  } # end iEOF, lwd 
}  # end iInd

plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

png(gsub(" ","",paste("../images/Correlation_EOF_Season_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
heatmap.2(plot_matrix, 
          col  = colorPlot, 
          Rowv = NA, 
          Colv = NA, 
          margins = c(10,10), 
          xlab= "", 
          ylab = "")
dev.off()

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("../images/Correlation_EOF_Season_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
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

# Loop over EOFs and calculate correlation with indexes
# we have 4 EOF variables, vs 2 modes, vs 2 seasons (16) and 10 species
itot <- (5*2*2)+1

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
      geom_line(aes(y = x, color = "PCA"), linetype = "solid", linewidth = 1.5) +
      geom_line(aes(y = y, color = "Residuals"), linetype = "solid", linewidth = 1.5) +
      geom_hline(yintercept = 0, color = "gray70", linetype = "dashed", linewidth = 1) +
      geom_text(aes(label = paste("R=", format(estimate, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      geom_text(aes(label = paste("p=", format(p.value, digits = 2))), 
                x = Inf, y = Inf, hjust = 1, vjust = 0, size = 5) +
      scale_color_manual(values = c("PCA" = "#F21A00", "Residuals" = "#3B9AB2")) +
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
    png(gsub(" ","",paste("../images/CorrelationEOFSeasonSp/",spNamePlot[iSpp],NamePlotModesSpawnSeason[iEOF-1],".png")),res = 300, width = 2000, height = 2000)
    grid.arrange(plot1, plot2, ncol = 1)
    dev.off()
  } # end iEOF, lwd 
}  # end iInd
    
plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

png(gsub(" ","",paste("../images/Correlation_EOF_SpSeason_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
heatmap.2(plot_matrix, 
          col  = colorPlot, 
          Rowv = NA, 
          Colv = NA, 
          margins = c(15,15), 
          xlab= "", 
          ylab = "")
dev.off()

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("../images/Correlation_EOF_SpSeason_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
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
