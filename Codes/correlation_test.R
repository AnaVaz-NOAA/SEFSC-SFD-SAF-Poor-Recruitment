 # Make correlations between indexes, EOF and recdev
# calculate spectral analyses
library(gplots)
library(RColorBrewer)
library(bcp)
library(gridExtra)
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