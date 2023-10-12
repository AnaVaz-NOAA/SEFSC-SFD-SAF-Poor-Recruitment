# Make correlations between indexes, EOF and recdev
# calculate spectral analyses
library(gplots)
library(RColorBrewer)
library(changepoint)
library(strucchange)

years <- 1973:2021

breaks_matrix <- matrix(nrow = 0, ncol = 3)
colnames(breaks_matrix) <- c("Variable", "Method", "Breaks")

# Recruitment deviation
for (iSpp in 2:11) {
  #look for breaks
  aux <- na.omit(RecDev[,iSpp])
  yraux <- RecDev[complete.cases(RecDev[,iSpp]),1]
  png(gsub(" ","",paste("./images/ChangePoint/",spNamePlot[iSpp-1],"_changepoint.png")), width = 2000, height = 2000, res = 300)
  par(mfrow = c(2,2), mar=c(0.5, 0.5, 0.5, 0.5), oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  for (icpt in 1:3) {
    varname <- switch(icpt,
                      'Mean',
                      'Var',
                      'MeanVar')
    if (icpt == 1) {
      cpaux <- cpt.mean(aux)
    } else {
      if (icpt == 2) {
        cpaux <- cpt.var(aux)
      } else {
        # icpt == 3
        cpaux <- cpt.meanvar(aux)
      }
    }
    for (cpt in cpaux@cpts) {
      breaks_matrix <- rbind(breaks_matrix, c(spNamePlot[iSpp-1], paste('CPT',varname), yraux[cpt]))
    }
    limit = max(abs(aux),na.rm = TRUE)
    
    plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", ylab="", xlab="", 
         main = c("Change Point ",varname," ",spNamePlot[iSpp-1]))
    axis(1, at = seq(min(years), max(years), by = 1), labels = NA, tck = -0.02)
    lines(yraux, aux,  lty=1, col=4, lwd=2)
    abline(v = yraux[cpaux@cpts], col = "lightgray", lwd = 5)
    
    for (i in 1:(length(cpaux@cpts)+1)) {
      print(i)
      if (i == 1) {
        start <- 1
        end   <- cpaux@cpts[i]
      } else if (i == length(cpaux@cpts)+1) {
        start <- cpaux@cpts[i-1] + 1 #cpaux@cpts[length(cpaux@cpts)] + 1
        end   <- length(aux)
      } else {
        start <- cpaux@cpts[i-1] + 1
        end <- cpaux@cpts[i]
      }
      if (start > length(aux)){
        start <- length(aux)
      }
      print(start)
      print(end)
      # Calculate the linear regression coefficients for the segment
      model <- lm(aux[start:end] ~ yraux[start:end])
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      segments(yraux[start], intercept + slope*yraux[start], 
               yraux[end], intercept + slope*yraux[end], col = "red", lwd = 2, lty = "dashed")
    } # end segments
  }
  #-------------------------------------------------------------------------------
  # now do with structchange
  bpaux <- breakpoints(aux ~ 1, breaks=2)
  if (!is.na(bpaux$breakpoints)[1]) {
    for (bp in bpaux$breakpoints) {
      breaks_matrix <- rbind(breaks_matrix, c(spNamePlot[iSpp-1], 'Breakpoints', yraux[bp]))
    }
    
    plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = c("Break Point ",spNamePlot[iSpp-1]))
    axis(1, at = seq(min(years), max(years), by = 1), labels = NA, tck = -0.02)
    lines(yraux, aux,  lty=1, col=4, lwd=2)
    abline(v = yraux[bpaux$breakpoints], col = "lightgray", lwd = 5)
    for (i in 1:(length(bpaux$breakpoints)+1)) {
      print(i)
      if (i == 1) {
        start <- 1
        end   <- bpaux$breakpoints[i]
      } else if (i == length(bpaux$breakpoints)+1) {
        start <- bpaux$breakpoints[i-1] + 1
        end   <- length(aux)
      } else {
        start <- bpaux$breakpoints[i-1] + 1
        end <- bpaux$breakpoints[i]
      }
      if (start > length(aux)){
        start <- length(aux)
      }
      print(start)
      print(end)
      # Calculate the linear regression coefficients for the segment
      model <- lm(aux[start:end] ~ yraux[start:end])
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      segments(yraux[start], intercept + slope*yraux[start], 
               yraux[end], intercept + slope*yraux[end], col = "red", lwd = 2, lty = "dashed")
    }
  } else { # end is nan need more segments
    bpaux <- breakpoints(aux ~ 1, h=2)
    if (!is.na(bpaux$breakpoints)[1]) {
      for (bp in bpaux$breakpoints) {
        breaks_matrix <- rbind(breaks_matrix, c(spNamePlot[iSpp-1], 'Breakpoints', yraux[bp]))
      }
      plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", ylab="", xlab="", main = c("Break Point ",spNamePlot[iSpp-1]))
      axis(1, at = seq(min(years), max(years), by = 1), labels = NA, tck = -0.02)
      lines(yraux, aux,  lty=1, col=4, lwd=2)
      abline(v = yraux[bpaux$breakpoints], col = "lightgray", lwd = 5)
      for (i in 1:(length(bpaux$breakpoints)+1)) {
        print(i)
        if (i == 1) {
          start <- 1
          end   <- bpaux$breakpoints[i]
        } else if (i == length(bpaux$breakpoints)+1) {
          start <- bpaux$breakpoints[i-1] + 1
          end   <- length(aux)
        } else {
          start <- bpaux$breakpoints[i-1] + 1
          end <- bpaux$breakpoints[i]
        }
        if (start > length(aux)){
          start <- length(aux)
        }
        print(start)
        print(end)
        # Calculate the linear regression coefficients for the segment
        model <- lm(aux[start:end] ~ yraux[start:end])
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        segments(yraux[start], intercept + slope*yraux[start], 
                 yraux[end], intercept + slope*yraux[end], col = "red", lwd = 2, lty = "dashed")
      }
    } 
  }  # end is nan need more segments 
  dev.off()
} # end species

write.csv(breaks_matrix, file="BreakMatrix_RecDev.csv", quote=F)

breaks_matrix <- matrix(nrow = 0, ncol = 3)
colnames(breaks_matrix) <- c("Variable", "Method", "Breaks")
#--------------------------------------------------------------------------------
# EOF Monthly 
for (iEOF in 2:length(EOFs)) {
  #look for breaks
  aux <- na.omit(EOFs[,iEOF])
  yraux <- EOFs[complete.cases(EOFs[,iEOF]),1]
  png(gsub(" ","",paste("../images/ChangePoint/",NamePlotModes[iEOF-1],"_changepoint.png")), 
      width = 2000, height = 2000, res = 300)
  par(mfrow = c(4,1), mar=c(0.5, 0.5, 0.5, 0.5), 
      oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  for (icpt in 1:3) {
    varname <- switch(icpt,
                      'Mean',
                      'Var',
                      'MeanVar')
    if (icpt == 1) {
      cpaux <- cpt.mean(aux)
      else {
        if (icpt == 2) {
          cpaux <- cpt.var(aux)
        } else {
          # icpt == 3
          cpaux <- cpt.meanvar(aux)
        }
      }
    }
  
    for (cpt in cpaux@cpts) {
      breaks_matrix <- rbind(breaks_matrix, c(NamePlotModes[iEOF-1], paste('CPT',varname) , yraux[cpt]))
    }
    
    limit = max(abs(aux),na.rm = TRUE)
    
    plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
         ylab="", xlab="", main = c("Change Point ",varname," ",NamePlotModes[iEOF-1]))
    axis(1, at = seq(min(years), max(years), by = 1), labels = NA, tck = -0.02)
    lines(yraux, aux,  lty=1, col=4, lwd=2)
    abline(v = yraux[cpaux@cpts], col = "lightgray", lwd = 5)
    
    for (i in 1:(length(cpaux@cpts)+1)) {
      if (i == 1) {
        start <- 1
        end   <- cpaux@cpts[i]
      } else if (i == length(cpaux@cpts)+1) {
        start <- cpaux@cpts[i-1] + 1 #cpaux@cpts[length(cpaux@cpts)] + 1
        end   <- length(aux)
      } else {
        start <- cpaux@cpts[i-1] + 1
        end <- cpaux@cpts[i]
      }
      if (start > length(aux)){
        start <- length(aux)
      }
      # Calculate the linear regression coefficients for the segment
      model <- lm(aux[start:end] ~ yraux[start:end])
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      segments(yraux[start], intercept + slope*yraux[start], 
               yraux[end], intercept + slope*yraux[end], col = "red", lwd = 2, lty = "dashed")
    }
  }
  
  #-------------------------------------------------------------------------------
  # now do with structchange
  bpaux <- breakpoints(aux ~ 1, breaks=2)
  
  if (!is.na(bpaux$breakpoints)[1]) {
    for (bp in bpaux$breakpoints) {
      breaks_matrix <- rbind(breaks_matrix, c(NamePlotModes[iEOF-1], 'Breakpoints', yraux[bp]))
    }
    
    plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
         ylab="", xlab="", main = c("Break Point ",NamePlotModes[iEOF-1]))
    axis(1, at = seq(min(years), max(years), by = 1), labels = NA, tck = -0.02)
    lines(yraux, aux,  lty=1, col=4, lwd=2)
    abline(v = yraux[bpaux$breakpoints], col = "lightgray", lwd = 5)
  
      for (i in 1:(length(bpaux$breakpoints)+1)) {
      if (i == 1) {
        start <- 1
        end   <- bpaux$breakpoints[i]
      } else if (i == length(bpaux$breakpoints)+1) {
        start <- bpaux$breakpoints[i-1] + 1
        end   <- length(aux)
      } else {
        start <- bpaux$breakpoints[i-1] + 1
        end <- bpaux$breakpoints[i]
      }
      if (start > length(aux)){
        start <- length(aux)
      }
      # Calculate the linear regression coefficients for the segment
      model <- lm(aux[start:end] ~ yraux[start:end])
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      segments(yraux[start], intercept + slope*yraux[start], 
               yraux[end], intercept + slope*yraux[end], col = "red", lwd = 2, lty = "dashed")
    }
  } else { # end is nan need more segments
    
    bpaux <- breakpoints(aux ~ 1, h=2)
    
    if (!is.na(bpaux$breakpoints)[1]) {
      for (bp in bpaux$breakpoints) {
        breaks_matrix <- rbind(breaks_matrix, c(NamePlotModes[iEOF-1], 'Breakpoints', yraux[bp]))
      }
      plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
           ylab="", xlab="", main = c("Break Point ",NamePlotModes[iEOF-1]))
      axis(1, at = seq(min(years), max(years), by = 1), labels = NA, tck = -0.02)
      lines(yraux, aux,  lty=1, col=4, lwd=2)
      abline(v = yraux[bpaux$breakpoints], col = "lightgray", lwd = 5)
      
      for (i in 1:(length(bpaux$breakpoints)+1)) {
        if (i == 1) {
          start <- 1
          end   <- bpaux$breakpoints[i]
        } else if (i == length(bpaux$breakpoints)+1) {
          start <- bpaux$breakpoints[i-1] + 1
          end   <- length(aux)
        } else {
          start <- bpaux$breakpoints[i-1] + 1
          end <- bpaux$breakpoints[i]
        }
        if (start > length(aux)){
          start <- length(aux)
        }
        # Calculate the linear regression coefficients for the segment
        model <- lm(aux[start:end] ~ yraux[start:end])
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        segments(yraux[start], intercept + slope*yraux[start], 
                 yraux[end], intercept + slope*yraux[end], col = "red", lwd = 2, lty = "dashed")
      }  
    } 
  }  
  dev.off()
}

write.csv(breaks_matrix, file="BreakMatrix_EOFMonthly.csv", quote=F)

breaks_matrix <- matrix(nrow = 0, ncol = 3)
colnames(breaks_matrix) <- c("Variable", "Method", "Breaks")
#--------------------------------------------------------------------------------
# EOF Seasonal

for (iEOF in 2:length(EOFSeason)) {
  #look for breaks
  aux <- na.omit(EOFSeason[,iEOF])
  yraux <- EOFSeason[complete.cases(EOFSeason[,iEOF]),1]
  cpaux <- cpt.meanvar(aux)
  
  for (cpt in cpaux@cpts) {
    breaks_matrix <- rbind(breaks_matrix, c(NamePlotModesSeason[iEOF-1], 'CPT', yraux[cpt]))
  }
  
  limit = max(abs(aux),na.rm = TRUE)
  
  png(gsub(" ","",paste("../images/ChangePoint/",NamePlotModesSeason[iEOF-1],"_changepoint.png")), 
      width = 2000, height = 2000, res = 300)
  par(mfrow = c(2,1), mar=c(0.5, 0.5, 0.5, 0.5), 
      oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
       ylab="", xlab="", main = c("Change Point ",NamePlotModesSeason[iEOF-1]))
  axis(1, at = seq(min(years), max(years), by = 1), labels = NA, tck = -0.02)
  lines(yraux, aux,  lty=1, col=4, lwd=2)
  abline(v = yraux[cpaux@cpts], col = "lightgray", lwd = 5)
  
  for (i in 1:(length(cpaux@cpts)+1)) {
    if (i == 1) {
      start <- 1
      end   <- cpaux@cpts[i]
    } else if (i == length(cpaux@cpts)+1) {
      start <- cpaux@cpts[i-1] + 1 #cpaux@cpts[length(cpaux@cpts)] + 1
      end   <- length(aux)
    } else {
      start <- cpaux@cpts[i-1] + 1
      end <- cpaux@cpts[i]
    }
    if (start > length(aux)){
      start <- length(aux)
    }
    # Calculate the linear regression coefficients for the segment
    model <- lm(aux[start:end] ~ yraux[start:end])
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    segments(yraux[start], intercept + slope*yraux[start], 
             yraux[end], intercept + slope*yraux[end], col = "red", lwd = 2, lty = "dashed")
  }
  #-------------------------------------------------------------------------------
  # now do with structchange
  bpaux <- breakpoints(aux ~ 1, breaks=2)
  
  if (!is.na(bpaux$breakpoints)[1]) {
    for (bp in bpaux$breakpoints) {
      breaks_matrix <- rbind(breaks_matrix, c(NamePlotModesSeason[iEOF-1], 'Breakpoints', yraux[bp]))
    }
    
    plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
         ylab="", xlab="", main = c("Break Point ",NamePlotModesSeason[iEOF-1]))
    axis(1, at = seq(min(years), max(years), by = 1), labels = NA, tck = -0.02)
    lines(yraux, aux,  lty=1, col=4, lwd=2)
    abline(v = yraux[bpaux$breakpoints], col = "lightgray", lwd = 5)
    
    for (i in 1:(length(bpaux$breakpoints)+1)) {
      if (i == 1) {
        start <- 1
        end   <- bpaux$breakpoints[i]
      } else if (i == length(bpaux$breakpoints)+1) {
        start <- bpaux$breakpoints[i-1] + 1
        end   <- length(aux)
      } else {
        start <- bpaux$breakpoints[i-1] + 1
        end <- bpaux$breakpoints[i]
      }
      if (start > length(aux)){
        start <- length(aux)
      }
      # Calculate the linear regression coefficients for the segment
      model <- lm(aux[start:end] ~ yraux[start:end])
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      segments(yraux[start], intercept + slope*yraux[start], 
               yraux[end], intercept + slope*yraux[end], col = "red", lwd = 2, lty = "dashed")
    }
  } else { # end is nan need more segments
    
    bpaux <- breakpoints(aux ~ 1, h=2)
    
    if (!is.na(bpaux$breakpoints)[1]) {
      for (bp in bpaux$breakpoints) {
        breaks_matrix <- rbind(breaks_matrix, c(NamePlotModesSeason[iEOF-1], 'Breakpoints', yraux[bp]))
      }
      
      plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
           ylab="", xlab="", main = c("Break Point ",NamePlotModesSeason[iEOF-1]))
      axis(1, at = seq(min(years), max(years), by = 1), labels = NA, tck = -0.02)
      lines(yraux, aux,  lty=1, col=4, lwd=2)
      abline(v = yraux[bpaux$breakpoints], col = "lightgray", lwd = 5)
      
      for (i in 1:(length(bpaux$breakpoints)+1)) {
        if (i == 1) {
          start <- 1
          end   <- bpaux$breakpoints[i]
        } else if (i == length(bpaux$breakpoints)+1) {
          start <- bpaux$breakpoints[i-1] + 1
          end   <- length(aux)
        } else {
          start <- bpaux$breakpoints[i-1] + 1
          end <- bpaux$breakpoints[i]
        }
        if (start > length(aux)){
          start <- length(aux)
        }
        # Calculate the linear regression coefficients for the segment
        model <- lm(aux[start:end] ~ yraux[start:end])
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        segments(yraux[start], intercept + slope*yraux[start], 
                 yraux[end], intercept + slope*yraux[end], col = "red", lwd = 2, lty = "dashed")
      }  
    } 
  }  
  dev.off()
}

write.csv(breaks_matrix, file="BreakMatrix_EOFSeason.csv", quote=F)

breaks_matrix <- matrix(nrow = 0, ncol = 3)
colnames(breaks_matrix) <- c("Variable", "Method", "Breaks")

#--------------------------------------------------------------------------------
# EOF Seasonal Spawning
for (iEOF in 2:length(EOFSpSeason)) {
  #look for breaks
  aux <- na.omit(EOFSpSeason[,iEOF])
  yraux <- EOFSpSeason[complete.cases(EOFSpSeason[,iEOF]),1]
  cpaux <- cpt.meanvar(aux)
  
  for (cpt in cpaux@cpts) {
    breaks_matrix <- rbind(breaks_matrix, c(NamePlotModesSpawnSeason[iSpp-1], 'CPT', yraux[cpt]))
  }
  
  limit = max(abs(aux),na.rm = TRUE)
  
  png(gsub(" ","",paste("../images/ChangePoint/",NamePlotModesSpawnSeason[iEOF-1],"_changepoint.png")), 
      width = 2000, height = 2000, res = 300)
  par(mfrow = c(2,1), mar=c(0.5, 0.5, 0.5, 0.5), 
      oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
       ylab="", xlab="", main = c("Change Point ",NamePlotModesSpawnSeason[iEOF-1]))
  axis(1, at = seq(min(years), max(years), by = 1), labels = NA, tck = -0.02)
  lines(yraux, aux,  lty=1, col=4, lwd=2)
  abline(v = yraux[cpaux@cpts], col = "lightgray", lwd = 5)
  
  for (i in 1:(length(cpaux@cpts)+1)) {
    if (i == 1) {
      start <- 1
      end   <- cpaux@cpts[i]
    } else if (i == length(cpaux@cpts)+1) {
      start <- cpaux@cpts[i-1] + 1 #cpaux@cpts[length(cpaux@cpts)] + 1
      end   <- length(aux)
    } else {
      start <- cpaux@cpts[i-1] + 1
      end <- cpaux@cpts[i]
    }
    if (start > length(aux)){
      start <- length(aux)
    }
    # Calculate the linear regression coefficients for the segment
    model <- lm(aux[start:end] ~ yraux[start:end])
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    segments(yraux[start], intercept + slope*yraux[start], 
             yraux[end], intercept + slope*yraux[end], col = "red", lwd = 2, lty = "dashed")
  }
  #-------------------------------------------------------------------------------
  # now do with structchange
  bpaux <- breakpoints(aux ~ 1, breaks=2)
  
  if (!is.na(bpaux$breakpoints)[1]) {
    for (bp in bpaux$breakpoints) {
      breaks_matrix <- rbind(breaks_matrix, c(NamePlotModesSpawnSeason[iEOF-1], 'Breakpoints', yraux[bp]))
    }
    
    plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
         ylab="", xlab="", main = c("Break Point ",NamePlotModesSpawnSeason[iEOF-1]))
    axis(1, at = seq(min(years), max(years), by = 1), labels = NA, tck = -0.02)
    lines(yraux, aux,  lty=1, col=4, lwd=2)
    abline(v = yraux[bpaux$breakpoints], col = "lightgray", lwd = 5)
    
    for (i in 1:(length(bpaux$breakpoints)+1)) {
      if (i == 1) {
        start <- 1
        end   <- bpaux$breakpoints[i]
      } else if (i == length(bpaux$breakpoints)+1) {
        start <- bpaux$breakpoints[i-1] + 1
        end   <- length(aux)
      } else {
        start <- bpaux$breakpoints[i-1] + 1
        end <- bpaux$breakpoints[i]
      }
      if (start > length(aux)){
        start <- length(aux)
      }
      # Calculate the linear regression coefficients for the segment
      model <- lm(aux[start:end] ~ yraux[start:end])
      intercept <- coef(model)[1]
      slope <- coef(model)[2]
      segments(yraux[start], intercept + slope*yraux[start], 
               yraux[end], intercept + slope*yraux[end], col = "red", lwd = 2, lty = "dashed")
    }
  } else { # end is nan need more segments
    
    bpaux <- breakpoints(aux ~ 1, h=2)
    
    if (!is.na(bpaux$breakpoints)[1]) {
      for (bp in bpaux$breakpoints) {
        breaks_matrix <- rbind(breaks_matrix, c(NamePlotModesSpawnSeason[iEOF-1], 'Breakpoints', yraux[bp]))
      }
      
      plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
           ylab="", xlab="", main = c("Break Point ",NamePlotModesSpawnSeason[iEOF-1]))
      axis(1, at = seq(min(years), max(years), by = 1), labels = NA, tck = -0.02)
      lines(yraux, aux,  lty=1, col=4, lwd=2)
      abline(v = yraux[bpaux$breakpoints], col = "lightgray", lwd = 5)
      
      for (i in 1:(length(bpaux$breakpoints)+1)) {
        if (i == 1) {
          start <- 1
          end   <- bpaux$breakpoints[i]
        } else if (i == length(bpaux$breakpoints)+1) {
          start <- bpaux$breakpoints[i-1] + 1
          end   <- length(aux)
        } else {
          start <- bpaux$breakpoints[i-1] + 1
          end <- bpaux$breakpoints[i]
        }
        if (start > length(aux)){
          start <- length(aux)
        }
        # Calculate the linear regression coefficients for the segment
        model <- lm(aux[start:end] ~ yraux[start:end])
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        segments(yraux[start], intercept + slope*yraux[start], 
                 yraux[end], intercept + slope*yraux[end], col = "red", lwd = 2, lty = "dashed")
      }  
    } 
  }  
  dev.off()
}

write.csv(breaks_matrix, file="BreakMatrix_EOFSpSeason.csv", quote=F)

#------------------------------------------------------------------------------
# Plot Breaks RecDev

# Load data from csv file and remove breaks of last years (2020-2021)
data <- read.csv("BreakMatrix_RecDev.csv")
data <- subset(data, Breaks < 2020)

# Create box plot for entire dataset
png("../images/ChangePoint/BreaksRecDev.png", width = 400, height = 1000, res = 300)
ggplot(data, aes(x = "", y = Breaks)) + geom_boxplot() + 
  ylab("Year Break") +
  xlab("")
dev.off()

# Create box plot by Variable 
png("../images/ChangePoint/BreaksRecDev_species.png", width = 1500, height = 600, res = 180)
ggplot(data, aes(x = Variable, y = Breaks)) + geom_boxplot() + 
  ylab("Year Break") +
  xlab("Variable/Mode") +
  theme(axis.text.x = element_text(size = rel(0.7)))
dev.off()

#------------------------------------------------------------------------------
# Plot Breaks Spawning Season

# Load data from csv file and remove breaks of last years (2020-2021)
data <- read.csv("BreakMatrix_EOFSpSeason.csv")
data <- subset(data, Breaks < 2020)

# Create box plot for entire dataset
png("../images/ChangePoint/BreaksSpSeason.png", width = 400, height = 1000, res = 300)
ggplot(data, aes(x = "", y = Breaks)) + geom_boxplot() + 
  ylab("Year Break") +
  xlab("")
dev.off()

# Create box plot by Variable 
png("../images/ChangePoint/BreaksSpSeason_Variable.png", width = 1500, height = 600, res = 180)
ggplot(data, aes(x = Variable, y = Breaks)) + geom_boxplot() + 
  ylab("Year Break") +
  xlab("Variable/Mode") +
  theme(axis.text.x = element_text(size = rel(0.6)))
dev.off()

#------------------------------------------------------------------------------
# Plot Breaks Spawning Season

# Load data from csv file and remove breaks of last years (2020-2021)
data <- read.csv("BreakMatrix_EOFSeason.csv")
data <- subset(data, Breaks < 2020)

# Create box plot for entire dataset
png("../images/ChangePoint/BreaksSeason.png", width = 400, height = 1000, res = 300)
ggplot(data, aes(x = "", y = Breaks)) + geom_boxplot() + 
  ylab("Year Break") +
  xlab("")
dev.off()

# Create box plot by Variable 
png("../images/ChangePoint/BreaksSeason_Variable.png", width = 1500, height = 600, res = 180)
ggplot(data, aes(x = Variable, y = Breaks)) + geom_boxplot() + 
  ylab("Year Break") +
  xlab("Variable/Mode") +
  theme(axis.text.x = element_text(size = rel(0.6)))
dev.off()

#------------------------------------------------------------------------------
# Plot Breaks Spawning Season

# Load data from csv file and remove breaks of last years (2020-2021)
data <- read.csv("BreakMatrix_EOFMonthly.csv")
data <- subset(data, Breaks < 2020)

# Create box plot for entire dataset
png("../images/ChangePoint/BreaksMonthly.png", width = 400, height = 1000, res = 300)
ggplot(data, aes(x = "", y = Breaks)) + geom_boxplot() + 
  ylab("Year Break") +
  xlab("")
dev.off()

# Create box plot by Variable 
png("../BreaksMonthly_Variable.png", width = 1500, height = 600, res = 180)
ggplot(data, aes(x = Variable, y = Breaks)) + geom_boxplot() + 
  ylab("Year Break") +
  xlab("Variable/Mode") +
  theme(axis.text.x = element_text(size = rel(0.6)))
dev.off()