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
# load recdevs, indexes and anomalies
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
aux <- expand.grid(NameSeason, NamePlot)
NamePlotSeason <- paste(aux$Var2, aux$Var1)

spNamePlot <- c("Gag Grouper","Greater Amberjack","Gray Triggerfish"," Red Porgy","Red Grouper","Black Sea Bass","Red Snapper","Scamp","Snowy Grouper ","Vermilion Snapper")

#------------------------------------------------------------------
# For SEASONAL
graphics.off()

pcut <- 0.1
# create a matrix with only dates both have data
Season_RecDev <- merge(Season, RecDev, by = "X")

# Loop over anomalies and calculate correlation with indexes
# we have 5 variables, vs 4 seasons (20) and 10 species
itot = 5*4+1

# for results of anomaly vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(Season_RecDev[,1])

WSp <- c(itot+1,itot+4,itot+5,itot+6,itot+8,itot+9)
SSp <-  c(itot+2,itot+3,itot+7,itot+10)
# Loop seasons for winter spawners
for (iseason in NameSeason) {
  #  columns with season name
  season_vars <- grep(iseason, colnames(Season_RecDev), value = TRUE)
  season_data <- cbind(Season_RecDev[, WSp], Season_RecDev[, season_vars])
  
  # check if worked
  print(paste("Variables for", iseason, ":"))
  print(season_vars)

  #pairs(season_data)
  plot <- ggpairs(as.data.frame(season_data))
    theme_bw() + theme(text = element_text(size = 8))  

  ggsave( filename = gsub(" ", "", paste("./images/Pairs_WSp_", iseason, ".png")),
    plot = plot,
    width = 20,  
    height = 20, 
    dpi = 300)}

# Loop seasons for winter spawners
for (iseason in NameSeason) {
  #  columns with season name
  season_vars <- grep(iseason, colnames(Season_RecDev), value = TRUE)
  season_data <- cbind(Season_RecDev[, WSp], Season_RecDev[, season_vars])
  
  # check if worked
  print(paste("Variables for", iseason, ":"))
  print(season_vars)
  
  ggsave( filename = gsub(" ", "", paste("./images/Pairs_WSp_", iseason, ".png")),
          plot = plot,
          width = 20,  
          height = 20, 
          dpi = 300)}

SSp <-  c(itot+2,itot+3,itot+7,itot+10)
# Loop seasons for winter spawners
for (iseason in NameSeason) {
  #  columns with season name
  season_vars <- grep(iseason, colnames(Season_RecDev), value = TRUE)
  season_data <- cbind(Season_RecDev[, SSp], Season_RecDev[, season_vars])
  
  # check if worked
  print(paste("Variables for", iseason, ":"))
  print(season_vars)
  
  #pairs(season_data)
  plot <- ggpairs(as.data.frame(season_data))
  theme_bw() + theme(text = element_text(size = 8))  
  
  ggsave( filename = gsub(" ", "", paste("./images/Pairs_SSp_", iseason, ".png")),
          plot = plot,
          width = 20,  
          height = 20, 
          dpi = 300)
}

# Define a custom panel function for the upper triangle
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor = 0.8, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "complete.obs")
  txt <- formatC(r, digits = digits, format = "f")
  text(0.5, 0.5, paste0(prefix, txt), cex = cex.cor * abs(r))
}

# Define a custom panel function for the lower triangle
panel.smooth <- function(x, y, ...) {
  points(x, y, pch = 19, col = "blue", cex = 0.6)
  lines(lowess(x, y), col = "red", lwd = 2, ...)
}

# Generate pairwise plots for seasonal data
for (iseason in NameSeason) {
  # Identify the columns for the specific season
  season_vars <- grep(iseason, colnames(Season_RecDev), value = TRUE)
  season_data <- cbind(Season_RecDev[, WSp], Season_RecDev[, season_vars])
  
  # Filter the columns for complete cases to avoid NA issues
  season_data <- season_data[complete.cases(season_data), ]
  
  # Set up file for saving the plot
  output_file <- gsub(" ", "", paste0("./images/Pairs_New_WSp_", iseason, ".png"))
  
  # Save the plot as a PNG
  png(filename = output_file, width = 1500, height = 1500, res = 300)
  
  # Use `pairs` with custom panels
  pairs(
    season_data,
    upper.panel = panel.cor,    # Show correlations in the upper panel
    lower.panel = panel.smooth, # Show scatterplots and smooth fits in the lower panel
    diag.panel = function(x) {  # Add histograms to the diagonal
      par(new = TRUE)
      hist(x, col = "lightgray", probability = TRUE, main = "", axes = FALSE)
      lines(density(x), col = "darkblue", lwd = 2)
    },
    main = paste("Pairwise Plot for", iseason, "Seasonal Data")
  )
  
  # Close the PNG device
  dev.off()
}

# Repeat for SSp data
for (iseason in NameSeason) {
  # Identify the columns for the specific season
  season_vars <- grep(iseason, colnames(Season_RecDev), value = TRUE)
  season_data <- cbind(Season_RecDev[, SSp], Season_RecDev[, season_vars])
  
  # Filter the columns for complete cases to avoid NA issues
  season_data <- season_data[complete.cases(season_data), ]
  
  # Set up file for saving the plot
  output_file <- gsub(" ", "", paste0("./images/Pairs_New_SSp_", iseason, ".png"))
  
  # Save the plot as a PNG
  png(filename = output_file, width = 1500, height = 1500, res = 300)
  
  # Use `pairs` with custom panels
  pairs(
    season_data,
    upper.panel = panel.cor,    # Show correlations in the upper panel
    lower.panel = panel.smooth, # Show scatterplots and smooth fits in the lower panel
    diag.panel = function(x) {  # Add histograms to the diagonal
      par(new = TRUE)
      hist(x, col = "lightgray", probability = TRUE, main = "", axes = FALSE)
      lines(density(x), col = "darkblue", lwd = 2)
    },
    main = paste("Pairwise Plot for", iseason, "Seasonal Data")
  )
  
  # Close the PNG device
  dev.off()
}

# it is in alphabetical order
NameSeason <- c("Summer", "Winter")
# create combinations of names and combine the names
aux <- expand.grid(NameSeason, NamePlot)
NamePlotSpawnSeason <- paste(aux$Var2, aux$Var1)

for (iSpp in 1:10) {
  for (iVar in 2:itot) {
    # correlation and save coef. and p
    x <- Season_RecDev[,iVar]
    y <- Season_RecDev[,iSpp+itot]
    # correlation and save coef. and p
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    if (aux$p.value < pcut){
    # TEST WITH GGPLOT
    # Create a data frame with your data
    df <- data.frame(
      yearsPlot = yearsPlot,
      x = x,
      y = y,
      estimate = aux$estimate,
      p.value = aux$p.value,
      color = as.numeric(factor(Season_RecDev[,1])))
    df <- df[complete.cases(df), ]
    pal <- wes_palette("Zissou1", 30, type = "continuous")
    # Scatterplot with color gradient
    plot2 <- ggplot(df, aes(x = x, y = y)) +
      geom_point(aes(colour = yearsPlot),  size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dashed", se = FALSE) +
      geom_ribbon(
        aes(ymin = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "lwr"],
            ymax = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "upr"]),
        fill = "gray80", alpha = 0.5) +
      # use polynomial regression
      geom_smooth(formula = y ~ poly(x, 2), color = "blue", se = FALSE) +
      scale_color_gradientn(colors = pal, name = "") +
      labs(
        x = "Anomaly",
        y = "Recruitment",
        title = paste("R=",format(aux$estimate, digits = 2), " p=",format(aux$p.value , digits = 2))) +
      theme_minimal()
      png(gsub(" ","",paste("./images/CorrelationAnoml/", spNamePlot[iSpp], NamePlotSeason[iVar-1],".png")),
          res = 300, width = 2000, height = 2000)
    dev.off()
    }
  } # end iVar, lwd 
}  # end iInd

plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
write.csv(plot_matrix, file="./csv_files/Correlation_Anomal_Season_Species.csv", quote=F)
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("./images/Correlation_Anomal_Season_Species_heatmap.png")),
    res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"),  
    panel.grid.major = element_line(color = "lightgray", size = 0.5), 
    panel.grid.minor = element_line(color = "lightgray", size = 0.5))
dev.off()

#------------------------------------------------------------------
# For Spawning SEASONAL
graphics.off()

# create a matrix with only dates both have data
SpSeason_RecDev <- merge(SpSeason, RecDev, by = "X")

# we have 5 Anomal variables, vs 2 seasons (10) and 10 species
itot <- (5*2)+1

# for results of variables vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSpawnSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSpawnSeason, spNamePlot))

yearsPlot <- unique(SpSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iVar in 2:itot) {
    # correlation and save coef. and p
    x <- SpSeason_RecDev[,iVar]
    y <- SpSeason_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    # correlation and save coef. and p
    x <- SpSeason_RecDev[,iVar]
    y <- SpSeason_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    if (aux$p.value < pcut){
    # TEST WITH GGPLOT
    # Create a data frame with your data
    df <- data.frame(
      yearsPlot = yearsPlot,
      x = x,
      y = y,
      estimate = aux$estimate,
      p.value = aux$p.value,
      color = as.numeric(factor(SpSeason_RecDev[,1])))
    df <- df[complete.cases(df), ]
    
    pal <- wes_palette("Zissou1", 30, type = "continuous")
    # Create the second panel with a color gradient
    plot2 <- ggplot(df, aes(x = x, y = y)) +
      geom_point(aes(colour = yearsPlot),  size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dashed", se = FALSE) +
      geom_ribbon(
        aes(ymin = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "lwr"],
            ymax = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "upr"]),
        fill = "gray80", alpha = 0.5) +
      scale_color_gradientn(colors = pal, name = "") +
      labs(
        x = "Anomaly",
        y = "Residuals",
        title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))) +
      theme_minimal()
    png(gsub(" ","",paste("./images/CorrelationAnomalSeasonSp/",spNamePlot[iSpp],NamePlotSpawnSeason[iVar-1],".png")),res = 300, width = 2000, height = 2000)
    dev.off()
    }
  } # end iVar, lwd 
}  # end iInd
    
plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("./images/Correlation_Anomal_SpSeason_Species_heatmap.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),  
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "lightgray", size = 0.5), 
    panel.grid.minor = element_line(color = "lightgray", size = 0.5))
dev.off()

#------------------------------------------------------------------
# Chl Globcolour 15
#------------------------------------------------------------------
RecDev      <- read.csv("./csv_files/RecruitmentResiduals_Feb23.csv")
Season   <- read.csv("./csv_files/Anomaly_Seasonal_ChlShelf15.csv")
SpSeason <- read.csv("./csv_files/Anomaly_Seasonal_ChlSpShelf15.csv")

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

spNamePlot <- c("Gag Grouper","Greater Amberjack","Gray Triggerfish"," Red Porgy","Red Grouper","Black Sea Bass","Red Snapper","Scamp","Snowy Grouper ","Vermilion Snapper")

#------------------------------------------------------------------
# For Globcolour 15 SEASONAL
#------------------------------------------------------------------
graphics.off()

pcut <- 0.1
# create a matrix with only dates both have data
Season_RecDev <- merge(Season, RecDev, by = "X")

# Loop over seasons and calculate correlation with indexes
# we have 1 variable, vs 4 seasons (4) and 10 species
itot = 4+1

# for results of variables vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(Season_RecDev[,1])

for (iSpp in 1:10) {
  for (iVar in 2:itot) {
    # correlation and save coef. and p
    x <- Season_RecDev[,iVar]
    y <- Season_RecDev[,iSpp+itot]
    # correlation and save coef. and p
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    if (aux$p.value < pcut){
      # TEST WITH GGPLOT
      # Create a data frame with your data
      df <- data.frame(
        yearsPlot = yearsPlot,
        x = x,
        y = y,
        estimate = aux$estimate,
        p.value = aux$p.value,
        color = as.numeric(factor(Season_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      pal <- wes_palette("Zissou1", 30, type = "continuous")
      # Scatterplot with color gradient
      plot2 <- ggplot(df, aes(x = x, y = y)) +
        geom_point(aes(colour = yearsPlot),  size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, color = "black", 
                    linetype = "dashed", se = FALSE) +
        geom_ribbon(
          aes(ymin = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "lwr"],
              ymax = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "upr"]),
          fill = "gray80", alpha = 0.5) +
        scale_color_gradientn(colors = pal, name = "") +
        labs(
          x = "Anomaly Chl",
          y = "Recruitment",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))) +
        theme_minimal()
      png(gsub(" ","",paste("./images/CorrelationAnomlChlSeason15/",spNamePlot[iSpp],NamePlotSeason[iVar-1],".png")),res = 300, width = 2000, height = 2000)
      dev.off()
    }
  } # end iVar, lwd 
}  # end iInd

plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
write.csv(plot_matrix, file="./csv_files/Correlation_Anoml_ChlShelf15.csv", quote=F)
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("./images/Correlation_Anoml_ChlShelf15_Rect.png")),
    res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"), 
    panel.grid.major = element_line(color = "lightgray", size = 0.5),
    panel.grid.minor = element_line(color = "lightgray", size = 0.5))
dev.off()

#------------------------------------------------------------------
# For Spawning SEASONAL Globcolour 
#------------------------------------------------------------------
graphics.off()

SpSeason_RecDev <- merge(SpSeason, RecDev, by = "X")

# we have 2 seasons and 10 species
itot <- (2)+1

# for results of variables vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSpawnSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSpawnSeason, spNamePlot))

yearsPlot <- unique(SpSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iVar in 2:itot) {
    # correlation and save coef. and p
    x <- SpSeason_RecDev[,iVar]
    y <- SpSeason_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    # correlation and save coef. and p
    x <- SpSeason_RecDev[,iVar]
    y <- SpSeason_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    if (aux$p.value < pcut){
      # TEST WITH GGPLOT
      # Create a data frame with your data
      df <- data.frame(
        yearsPlot = yearsPlot,
        x = x,
        y = y,
        estimate = aux$estimate,
        p.value = aux$p.value,
        color = as.numeric(factor(SpSeason_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      pal <- wes_palette("Zissou1", 30, type = "continuous")
      
      # Scatterplot with color gradient
      plot2 <- ggplot(df, aes(x = x, y = y)) +
        geom_point(aes(colour = yearsPlot),  size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dashed", se = FALSE) +
        geom_ribbon(
          aes(ymin = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "lwr"],
              ymax = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "upr"]),
          fill = "gray80", alpha = 0.5) +
        scale_color_gradientn(colors = pal, name = "") +
        labs(
          x = "Anomaly",
          y = "Residuals",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))) +
        theme_minimal()
      png(gsub(" ","",paste("./images/CorrelationAnomlChlSpSeason15/",spNamePlot[iSpp],NamePlotSpawnSeason[iVar-1],".png")),res = 300, width = 2000, height = 2000)
      dev.off()
    }
  } # end iVar, lwd 
}  # end iInd

plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("./images/Correlation_Anoml_ChlSpShelf15_Rect.png")),res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "lightgray", size = 0.5),
    panel.grid.minor = element_line(color = "lightgray", size = 0.5))
dev.off()

#------------------------------------------------------------------
# For seasonal Globcolour 50-300m
#------------------------------------------------------------------
graphics.off()

Season   <- read.csv("./csv_files/Anomaly_Seasonal_ChlShelf50.csv")
Season_RecDev <- merge(Season, RecDev, by = "X")

# 4 seasons
itot <- (4)+1

# for results of variables vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(Season_RecDev[,1])

for (iSpp in 1:10) {
  for (iVar in 2:itot) {
    # correlation and save coef. and p
    x <- Season_RecDev[,iVar]
    y <- Season_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    # correlation and save coef. and p
    x <- Season_RecDev[,iVar]
    y <- Season_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    if (aux$p.value < pcut){
      # TEST WITH GGPLOT
      # Create a data frame with your data
      df <- data.frame(
        yearsPlot = yearsPlot,
        x = x,
        y = y,
        estimate = aux$estimate,
        p.value = aux$p.value,
        color = as.numeric(factor(Season_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      pal <- wes_palette("Zissou1", 30, type = "continuous")
      # Scatterplot with color gradient
      plot2 <- ggplot(df, aes(x = x, y = y)) +
        geom_point(aes(colour = yearsPlot),  size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dashed", se = FALSE) +
        geom_ribbon(
          aes(ymin = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "lwr"],
              ymax = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "upr"]),
          fill = "gray80", alpha = 0.5) +
        scale_color_gradientn(colors = pal, name = "") +
        labs(
          x = "Anomaly",
          y = "Residuals",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))) +
        theme_minimal()
      png(gsub(" ","",paste("./images/CorrelationAnomlChlSeason50/",spNamePlot[iSpp],NamePlotSeason[iVar-1],".png")),res = 300, width = 2000, height = 2000)
      dev.off()
    }
  } # end iVar, lwd 
}  # end iInd

plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
write.csv(plot_matrix, file="./csv_files/Correlation_Anoml_ChlShelf50.csv", quote=F)

# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("./images/Correlation_Anoml_ChlShelf50_Rect.png")),
    res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "lightgray", size = 0.5),
    panel.grid.minor = element_line(color = "lightgray", size = 0.5))
dev.off()

#------------------------------------------------------------------
# Shelf spawning globcolour 50-300m
#------------------------------------------------------------------
graphics.off()

SpSeason <- read.csv("./csv_files/Anomaly_Seasonal_ChlSpShelf50.csv")
# create a matrix with only dates both have data
SpSeason_RecDev <- merge(SpSeason, RecDev, by = "X")

# we have 2 seasons and 10 species
itot <- (2)+1

# for results of variables vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSpawnSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSpawnSeason, spNamePlot))

yearsPlot <- unique(SpSeason_RecDev[,1])

for (iSpp in 1:10) {
  for (iVar in 2:itot) {
    # correlation and save coef. and p
    x <- SpSeason_RecDev[,iVar]
    y <- SpSeason_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    # correlation and save coef. and p
    x <- SpSeason_RecDev[,iVar]
    y <- SpSeason_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    if (aux$p.value < pcut){
      # TEST WITH GGPLOT
      # Create a data frame with your data
      df <- data.frame(
        yearsPlot = yearsPlot,
        x = x,
        y = y,
        estimate = aux$estimate,
        p.value = aux$p.value,
        color = as.numeric(factor(SpSeason_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      pal <- wes_palette("Zissou1", 30, type = "continuous")
      # Scatterplot with a color gradient
      plot2 <- ggplot(df, aes(x = x, y = y)) +
        geom_point(aes(colour = yearsPlot),  size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dashed", se = FALSE) +
        geom_ribbon(
          aes(ymin = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "lwr"],
              ymax = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "upr"]),
          fill = "gray80", alpha = 0.5) +
        scale_color_gradientn(colors = pal, name = "") +
        labs(
          x = "Anomaly",
          y = "Residuals",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))
        ) +
        theme_minimal()
        png(gsub(" ","",paste("./images/CorrelationAnomlChlSpSeason50/",spNamePlot[iSpp],NamePlotSpawnSeason[iVar-1],".png")),res = 300, width = 2000, height = 2000)
      dev.off()
    }
  } # end iVar, lwd 
}  # end iInd

plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("./images/Correlation_Anoml_ChlSpShelf50_Rect.png")),
    res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"), 
    panel.grid.major = element_line(color = "lightgray", size = 0.5),
    panel.grid.minor = element_line(color = "lightgray", size = 0.5) )
dev.off()

#------------------------------------------------------------------
# For seasonal MODIS 50-300m
#------------------------------------------------------------------
graphics.off()

Season   <- read.csv("./csv_files/Anomaly_Seasonal_ChlModis15.csv")
Season_RecDev <- merge(Season, RecDev, by = "X")

# 4 seasons
itot <- (4)+1

# for results of variables vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(Season_RecDev[,1])

for (iSpp in 1:10) {
  for (iVar in 2:itot) {
    # correlation and save coef. and p
    x <- Season_RecDev[,iVar]
    y <- Season_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    # correlation and save coef. and p
    x <- Season_RecDev[,iVar]
    y <- Season_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    if (aux$p.value < pcut){
      # TEST WITH GGPLOT
      # Create a data frame with your data
      df <- data.frame(
        yearsPlot = yearsPlot,
        x = x,
        y = y,
        estimate = aux$estimate,
        p.value = aux$p.value,
        color = as.numeric(factor(Season_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      
      pal <- wes_palette("Zissou1", 30, type = "continuous")
      # Scatterplot with color gradient
      plot2 <- ggplot(df, aes(x = x, y = y)) +
        geom_point(aes(colour = yearsPlot),  size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dashed", se = FALSE) +
        geom_ribbon(
          aes(ymin = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "lwr"],
              ymax = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "upr"]),
          fill = "gray80", alpha = 0.5) +
        scale_color_gradientn(colors = pal, name = "") +
        labs(
          x = "Anomaly",
          y = "Residuals",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))) +
        theme_minimal()
      
      # Create a grid of the two plots
      png(gsub(" ","",paste("./images/CorrelationAnomlModisChlSeason15/",spNamePlot[iSpp],NamePlotSeason[iVar-1],".png")),res = 300, width = 2000, height = 2000)
      dev.off()
    }
  } # end iVar, lwd 
}  # end iInd

plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
write.csv(plot_matrix, file="./csv_files/Correlation_Anoml_Modis15.csv", quote=F)
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("./images/Correlation_Anoml_Modis15.png")),
    res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +  # Add white grid lines
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "lightgray", size = 0.5),
    panel.grid.minor = element_line(color = "lightgray", size = 0.5))
dev.off()

#------------------------------------------------------------------
# For seasonal MODIS 50-300m
#------------------------------------------------------------------
graphics.off()

Season   <- read.csv("./csv_files/Anomaly_Seasonal_ChlModis50.csv")
Season_RecDev <- merge(Season, RecDev, by = "X")

# 4 seasons
itot <- (4)+1

# for results of variables vs indexes
corr_matrix <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))
p_matrix    <- matrix(NA, nrow=itot-1, ncol=10, dimnames=list(NamePlotSeason, spNamePlot))

yearsPlot <- unique(Season_RecDev[,1])

for (iSpp in 1:10) {
  for (iVar in 2:itot) {
    # correlation and save coef. and p
    x <- Season_RecDev[,iVar]
    y <- Season_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    # correlation and save coef. and p
    x <- Season_RecDev[,iVar]
    y <- Season_RecDev[,iSpp+itot]
    aux <- cor.test(x, y, use = "complete.obs")
    corr_matrix[iVar-1,iSpp] <- aux$estimate
    p_matrix[iVar-1,iSpp] <- aux$p.value
    
    if (aux$p.value < pcut){
      # TEST WITH GGPLOT
      # Create a data frame with your data
      df <- data.frame(
        yearsPlot = yearsPlot,
        x = x,
        y = y,
        estimate = aux$estimate,
        p.value = aux$p.value,
        color = as.numeric(factor(Season_RecDev[,1]))
      )
      df <- df[complete.cases(df), ]
      pal <- wes_palette("Zissou1", 30, type = "continuous")
      # Scatterplot with color gradient
      plot2 <- ggplot(df, aes(x = x, y = y)) +
        geom_point(aes(colour = yearsPlot),  size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dashed", se = FALSE) +
        geom_ribbon(
          aes(ymin = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "lwr"],
              ymax = predict(lm(y ~ x), newdata = df, interval = "confidence")[, "upr"]),
          fill = "gray80", alpha = 0.5) +
        scale_color_gradientn(colors = pal, name = "") +
        labs(
          x = "Anomaly",
          y = "Residuals",
          title = paste("R=",format(aux$estimate, digits = 2)," p=",format(aux$p.value , digits = 2))) +
        theme_minimal()
      png(gsub(" ","",paste("./images/CorrelationAnomlChlModisSeason50/",spNamePlot[iSpp],NamePlotSeason[iVar-1],".png")),res = 300, width = 2000, height = 2000)
      dev.off()
    }
  } # end iVar, lwd 
}  # end iInd

plot_matrix <- corr_matrix
plot_matrix[p_matrix > pcut] <- NaN
write.csv(plot_matrix, file="./csv_files/Correlation_Anoml_Modis50.csv", quote=F)
# plot the correlation
colorPlot <- brewer.pal(9, "RdBu")

melted_cor_matrix <- melt(plot_matrix)
png(gsub(" ","",paste("./images/Correlation_Anoml_Modis50.png")),
    res = 300, width = 2000, height = 2000)
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "dodgerblue3", mid = "white", high = "tomato3", na.value = "white", midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "lightgray", size = 0.5),
    panel.grid.minor = element_line(color = "lightgray", size = 0.5))
dev.off()