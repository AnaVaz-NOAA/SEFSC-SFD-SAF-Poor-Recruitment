# Make correlations between indexes, EOF and recdev
# calculate spectral analyses
library(gplots)
library(RColorBrewer)
library(changepoint)
library(strucchange)

# load recdevs, indexes and EOF
# set the diretory with the assessment files
#setwd("./csv_files")
Season   <- read.csv("Anomaly_Seasonal.csv")
SpSeason <- read.csv("Anomaly_Spawning_Seasonal.csv")

# it is in alphabetical order
NamePlot <- c("Bottom T","Mixed Layer","Salinity","SSH","SST")
NameSeason <- c("Fall","Spring","Summer","Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotSeason <- paste(aux$Var2, aux$Var1)

# it is in alphabetical order
NameSeason <- c("Summer","Winter")
# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotSpawnSeason <- paste(aux$Var2, aux$Var1)

# NamePlotSeason 
# NamePlotSpawnSeason 

breaks_matrix <- matrix(nrow = 0, ncol = 3)
colnames(breaks_matrix) <- c("Variable", "Method", "Breaks")
years <- 1993:2021

#--------------------------------------------------------------------------------
# Averages Seasonal

for (iEOF in 2:length(Season)) {
  #look for breaks
  aux <- na.omit(Season[,iEOF])
  yraux <- Season[complete.cases(Season[,iEOF]),1]
  cpaux <- cpt.meanvar(aux)
  
  for (cpt in cpaux@cpts) {
    breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'CPT', yraux[cpt]))
  }
  limit = max(abs(aux),na.rm = TRUE)
  
  png(gsub(" ","",paste("../images/ChangePoint/",NamePlotSeason[iEOF-1],"_changepoint.png")), 
      width = 2000, height = 2000, res = 300)
  par(mfrow = c(2,1), mar=c(0.5, 0.5, 0.5, 0.5), 
      oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
       ylab="", xlab="", main = c("Change Point ",NamePlotSeason[iEOF-1]))
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
      breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'Breakpoints', yraux[bp]))
    }
    
    plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
         ylab="", xlab="", main = c("Break Point ",NamePlotSeason[iEOF-1]))
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
        breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'Breakpoints', yraux[bp]))
      }
      
      plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
           ylab="", xlab="", main = c("Break Point ",NamePlotSeason[iEOF-1]))
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

write.csv(breaks_matrix, file="BreakMatrix_AnomalSeason.csv", quote=F)

breaks_matrix <- matrix(nrow = 0, ncol = 3)
colnames(breaks_matrix) <- c("Variable", "Method", "Breaks")

#--------------------------------------------------------------------------------
# EOF Seasonal Spawning
for (iEOF in 2:length(SpSeason)) {
  #look for breaks
  aux <- na.omit(SpSeason[,iEOF])
  yraux <- SpSeason[complete.cases(SpSeason[,iEOF]),1]
  cpaux <- cpt.meanvar(aux)
  
  for (cpt in cpaux@cpts) {
    breaks_matrix <- rbind(breaks_matrix, c(NamePlotSpawnSeason[iEOF-1], 'CPT', yraux[cpt]))
  }
  
  limit = max(abs(aux),na.rm = TRUE)
  
  png(gsub(" ","",paste("../images/ChangePoint/",NamePlotSpawnSeason[iEOF-1],"_Sp_changepoint.png")), 
      width = 2000, height = 2000, res = 300)
  par(mfrow = c(2,1), mar=c(0.5, 0.5, 0.5, 0.5), 
      oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
       ylab="", xlab="", main = c("Change Point ",NamePlotSpawnSeason[iEOF-1]))
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
      breaks_matrix <- rbind(breaks_matrix, c(NamePlotSpawnSeason[iEOF-1], 'Breakpoints', yraux[bp]))
    }
    
    plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
         ylab="", xlab="", main = c("Break Point ",NamePlotSpawnSeason[iEOF-1]))
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
        breaks_matrix <- rbind(breaks_matrix, c(NamePlotSpawnSeason[iEOF-1], 'Breakpoints', yraux[bp]))
      }
      
      plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
           ylab="", xlab="", main = c("Break Point ",NamePlotSpawnSeason[iEOF-1]))
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
write.csv(breaks_matrix, file="BreakMatrix_AnomalSpSeason.csv", quote=F)

#----------------------------------------------------------------------------
# MODIS R

Season   <- read.csv("Anomaly_Seasonal_ChlModis15.csv")

# it is in alphabetical order
NamePlot <- c("Chl")
NameSeason <- c("Fall","Spring","Summer","Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotSeason <- paste(aux$Var2, aux$Var1)

breaks_matrix <- matrix(nrow = 0, ncol = 3)
colnames(breaks_matrix) <- c("Variable", "Method", "Breaks")
years <- seq(2003,2022)

#--------------------------------------------------------------------------------
# Averages Seasonal

for (iEOF in 2:length(Season)) {
  #look for breaks
  aux <- na.omit(Season[,iEOF])
  yraux <- Season[complete.cases(Season[,iEOF]),1]
  cpaux <- cpt.meanvar(aux)
  
  for (cpt in cpaux@cpts) {
    breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'CPT', yraux[cpt]))
  }
  limit = max(abs(aux),na.rm = TRUE)
  
  png(gsub(" ","",paste("../images/ChangePoint/",NamePlotSeason[iEOF-1],"_changepoint_Modis15.png")), 
      width = 2000, height = 2000, res = 300)
  par(mfrow = c(2,1), mar=c(0.5, 0.5, 0.5, 0.5), 
      oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
       ylab="", xlab="", main = c("Change Point ",NamePlotSeason[iEOF-1]))
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
      breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'Breakpoints', yraux[bp]))
    }
    
    plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
         ylab="", xlab="", main = c("Break Point ",NamePlotSeason[iEOF-1]))
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
        breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'Breakpoints', yraux[bp]))
      }
      
      plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
           ylab="", xlab="", main = c("Break Point ",NamePlotSeason[iEOF-1]))
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
write.csv(breaks_matrix, file="BreakMatrix_AnomalSeasonModis15.csv", quote=F)


#-------------------------------------------------------------
# MODIS 50
Season   <- read.csv("Anomaly_Seasonal_ChlModis50.csv")

# it is in alphabetical order
NamePlot <- c("Chl")
NameSeason <- c("Fall","Spring","Summer","Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotSeason <- paste(aux$Var2, aux$Var1)

breaks_matrix <- matrix(nrow = 0, ncol = 3)
colnames(breaks_matrix) <- c("Variable", "Method", "Breaks")
years <- seq(2003,2022)

#--------------------------------------------------------------------------------
# Averages Seasonal

for (iEOF in 2:length(Season)) {
  #look for breaks
  aux <- na.omit(Season[,iEOF])
  yraux <- Season[complete.cases(Season[,iEOF]),1]
  cpaux <- cpt.meanvar(aux)
  
  for (cpt in cpaux@cpts) {
    breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'CPT', yraux[cpt]))
  }
  limit = max(abs(aux),na.rm = TRUE)
  
  png(gsub(" ","",paste("../images/ChangePoint/",NamePlotSeason[iEOF-1],"_changepoint_Modis50.png")), 
      width = 2000, height = 2000, res = 300)
  par(mfrow = c(2,1), mar=c(0.5, 0.5, 0.5, 0.5), 
      oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
       ylab="", xlab="", main = c("Change Point ",NamePlotSeason[iEOF-1]))
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
      breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'Breakpoints', yraux[bp]))
    }
    
    plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
         ylab="", xlab="", main = c("Break Point ",NamePlotSeason[iEOF-1]))
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
        breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'Breakpoints', yraux[bp]))
      }
      
      plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
           ylab="", xlab="", main = c("Break Point ",NamePlotSeason[iEOF-1]))
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
write.csv(breaks_matrix, file="BreakMatrix_AnomalSeasonModis50.csv", quote=F)


#-------------------------------------------------------------
# GlobColor 15
Season   <- read.csv("Anomaly_Seasonal_ChlShelf15.csv")

# it is in alphabetical order
NamePlot <- c("Chl")
NameSeason <- c("Fall","Spring","Summer","Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotSeason <- paste(aux$Var2, aux$Var1)

breaks_matrix <- matrix(nrow = 0, ncol = 3)
colnames(breaks_matrix) <- c("Variable", "Method", "Breaks")
years <- seq(2003,2022)

#--------------------------------------------------------------------------------
# Averages Seasonal

for (iEOF in 2:length(Season)) {
  #look for breaks
  aux <- na.omit(Season[,iEOF])
  yraux <- Season[complete.cases(Season[,iEOF]),1]
  cpaux <- cpt.meanvar(aux)
  
  for (cpt in cpaux@cpts) {
    breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'CPT', yraux[cpt]))
  }
  limit = max(abs(aux),na.rm = TRUE)
  
  png(gsub(" ","",paste("../images/ChangePoint/",NamePlotSeason[iEOF-1],"_changepoint_Glob15.png")), 
      width = 2000, height = 2000, res = 300)
  par(mfrow = c(2,1), mar=c(0.5, 0.5, 0.5, 0.5), 
      oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
       ylab="", xlab="", main = c("Change Point ",NamePlotSeason[iEOF-1]))
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
      breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'Breakpoints', yraux[bp]))
    }
    
    plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
         ylab="", xlab="", main = c("Break Point ",NamePlotSeason[iEOF-1]))
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
        breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'Breakpoints', yraux[bp]))
      }
      
      plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
           ylab="", xlab="", main = c("Break Point ",NamePlotSeason[iEOF-1]))
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
write.csv(breaks_matrix, file="BreakMatrix_AnomalSeasonGlob15.csv", quote=F)



#-------------------------------------------------------------
# GlobColor 50
Season   <- read.csv("Anomaly_Seasonal_ChlShelf50.csv")

# it is in alphabetical order
NamePlot <- c("Chl")
NameSeason <- c("Fall","Spring","Summer","Winter")

# create combinations of names and combine the names
aux <- expand.grid(NameSeason,NamePlot)
NamePlotSeason <- paste(aux$Var2, aux$Var1)

breaks_matrix <- matrix(nrow = 0, ncol = 3)
colnames(breaks_matrix) <- c("Variable", "Method", "Breaks")
years <- seq(2003,2022)

#--------------------------------------------------------------------------------
# Averages Seasonal

for (iEOF in 2:length(Season)) {
  #look for breaks
  aux <- na.omit(Season[,iEOF])
  yraux <- Season[complete.cases(Season[,iEOF]),1]
  cpaux <- cpt.meanvar(aux)
  
  for (cpt in cpaux@cpts) {
    breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'CPT', yraux[cpt]))
  }
  limit = max(abs(aux),na.rm = TRUE)
  
  png(gsub(" ","",paste("../images/ChangePoint/",NamePlotSeason[iEOF-1],"_changepoint_Glob50.png")), 
      width = 2000, height = 2000, res = 300)
  par(mfrow = c(2,1), mar=c(0.5, 0.5, 0.5, 0.5), 
      oma=c(0, 0, 0, 0), mai=c(0.7, 0.7, 0.7, 0.7))
  
  plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
       ylab="", xlab="", main = c("Change Point ",NamePlotSeason[iEOF-1]))
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
      breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'Breakpoints', yraux[bp]))
    }
    
    plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
         ylab="", xlab="", main = c("Break Point ",NamePlotSeason[iEOF-1]))
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
        breaks_matrix <- rbind(breaks_matrix, c(NamePlotSeason[iEOF-1], 'Breakpoints', yraux[bp]))
      }
      
      plot(years, rep(0,length(years)), ylim=c(-limit, limit), type="n", 
           ylab="", xlab="", main = c("Break Point ",NamePlotSeason[iEOF-1]))
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
write.csv(breaks_matrix, file="BreakMatrix_AnomalSeasonGlob50.csv", quote=F)