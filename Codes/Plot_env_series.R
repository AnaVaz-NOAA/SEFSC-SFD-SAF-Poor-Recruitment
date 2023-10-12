library(Hmisc)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(cmocean)
library(maps)
library(tidyverse)
library(mapdata)
library(marmap)

setwd("/Users/anavaz/Stuff/github/Poor-Recruitment-South-Atlantic/")

# Loading of the world coastline
load("./data/world_map.RData") 
# the object is called "maps" 
head(maps)

refaux <- read.csv("./csv_files/ssh_analyses.csv")
#load bathymetry
bathy <- getNOAA.bathy(min(refaux$Lon)-1,max(refaux$Lon)+1,min(refaux$Lat)-1,max(refaux$Lat)+1,resolution=1)
rm(refaux)
bathy_df <- fortify(bathy)
rm(bathy)

col_names <- c("Lat", "Lon", "USeasonal", "BSeasonal", 
               "Colour", "Constancy", "Contigency", "Predictability",
               "InterAnnual", "Annual", "IntraAnnual", "Synoptic",
               "TrendLinear", "Trend", "Avg", "Std")
mycolors <- rev(c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58'))

# PLOT ALL PERIOD
max_values <- matrix(c(22, 1.3, .55, 20, 2.75,
                       1, .55, .4, 1, .8,
                       2.5, 2.1, 1.5, 2, 1.2,
                       .35, .6, .3, .35, .6,
                       .7, .35, .4, .7, .6,
                       .75, .8, .5, .7, .75,
                       .25, .25, .12, .25, .2,
                       .8, .65, .5, .8, .7,
                       .5, .45, .45, .08, .35,
                       .06, .085, .06, .0125, .06), nrow = 10, ncol = 5, byrow = TRUE)
# plot now in space each one
# load the file for each variable, then rotate to plot each column (analyes results)
for (ivar in 1:5) {
  varname <- switch(ivar,
                    'bottomT',
                    'Salinity',
                    'ssh',
                    'sst',
                    'mixedlayer')
  varnamePretty <- switch(ivar,
                          'Bottom Temperature',
                          'Salinity',
                          'SSH',
                          'SST',
                          'Mixed Layer')
  filename <- (gsub(" ","",paste("./csv_files/",varname,"_analyses.csv")))
  ref  <- read.csv(filename)
  
  # we have 12 different analyses  
  for (i in 3:16) {
    if (i <= 8 ){
      MaxV <- max_values[i-2,ivar]
    }
    PlotName <- col_names[i]
    # create a plot
    if (i <= 8){
      ggplot() + # Declaration of local variables for the plot
        geom_polygon(data=maps, aes(x=lon, y=lat),fill = "grey70") +
        coord_quickmap(expand=FALSE, 
                       xlim = c(min(ref$Lon)-1, max(ref$Lon)), 
                       ylim = c(min(ref$Lat), max(ref$Lat))) +
        geom_point(data=ref, aes(x = Lon, y=Lat, color=!!sym(PlotName))) +
        scale_colour_viridis_c(limits = c(0, MaxV)) +
        geom_contour(data = bathy_df, aes(x = x, y = y, z = z), 
                     breaks = c(-50, -400, -700), 
                     colour = "grey70", linewidth = 0.5) +
        labs(x = "Longitude",y = "Latitude", title = varnamePretty) +
        theme(text = element_text(size = 20)) +
        theme_light() 
    } else {
      pl <- ggplot() + # Declaration of local variables for the plot
        geom_polygon(data=maps, aes(x=lon, y=lat),fill = "grey70") +
        coord_quickmap(expand=FALSE,
                       xlim = c(min(ref$Lon)-1,max(ref$Lon)), 
                       ylim = c(min(ref$Lat),max(ref$Lat))) +
        geom_point(data=ref, aes(x = Lon, y=Lat, color=!!sym(PlotName))) +
        geom_contour(data = bathy_df, aes(x = x, y = y, z = z), 
                     breaks = c(-50, -400, -700), 
                     colour = "grey70", linewidth = 0.5) +
        labs(x = "Longitude",y = "Latitude", title = varnamePretty) +
        theme(text = element_text(size = 20)) +
        theme_light()
        if (i <= 12 | (i > 14 & ivar != 3))
          pl + scale_colour_viridis_c()
        else
          pl + scale_colour_gradient2(
            low = "dodgerblue4", mid = "white", high = "firebrick4",
            midpoint = 0 )
    }
    ggsave(gsub(" ","",paste("./images/analyses/",PlotName,"_",varname,".png")),
           device = "png")
  }
}

# PLOT SEASONAL
max_values <- matrix(c(22, 1.3, .55, 20, 2.75,
                       1, .6, .4, 1, .8,
                       2.5, 2.1, 1.5, 2, 1.2,
                       .35, .6, .3, .35, .6,
                       .7, .35, .4, .7, .6,
                       .75, .8, .5, .7, .75,
                       22, 1.3, .6, 20, 2.75,
                       1, .5, .4, 1, .8,
                       2.5, 2.1, 1.5, 2, 1.2,
                       .35, .6, .3, .15, .6,
                       .7, .35, .4, .7, .6,
                       .75, .7, .5, .7, .75
                       ), nrow = 12, ncol = 5, byrow = TRUE)

# load file for envPred
col_names <- c("Lat","Lon","USeasonal1", "BSeasonal1", 
               "Colour1", "Constancy1", "Contigency1", 
               "Predictability1","USeasonal2", "BSeasonal2", 
               "Colour2", "Constancy2", "Contigency2", "Predictability2")
for (ivar in 1:5) {
  varname <- switch(ivar,
                    'bottomT',
                    'Salinity',
                    'ssh',
                    'sst',
                    'mixedlayer')
  varnamePretty <- switch(ivar,
                          'Bottom Temperature',
                          'Salinity',
                          'SSH',
                          'SST',
                          'Mixed Layer')
  filename <- (gsub(" ","",paste("./csv_files/",varname,"_envPred.csv")))
  ref  <- read.csv(filename)
  
  # we have 12 different analyses  
  for (i in 3:14) {
    MaxV <- max_values[i-2,ivar]
    PlotName <- col_names[i]
    # create a plot
    ggplot() + # Declaration of local variables for the plot
      geom_polygon(data=maps, aes(x=lon, y=lat),fill = "grey70") +
      coord_quickmap(expand=FALSE, 
                     xlim = c(min(ref$Lon)-1, max(ref$Lon)), 
                     ylim = c(min(ref$Lat),max(ref$Lat))) +
      geom_point(data=ref, aes(x = Lon, y=Lat, color=!!sym(PlotName))) +
      scale_colour_viridis_c(limits = c(0, MaxV)) +
      geom_contour(data = bathy_df, aes(x = x, y = y, z = z), 
                   breaks = c(-50, -400, -700), 
                   colour = "grey70", linewidth = 0.5) +
      labs(x = "Longitude",y = "Latitude", title = varnamePretty) +
      theme(text = element_text(size = 20)) +
      theme_light()
    ggsave(gsub(" ","",paste("./images/analyses/",PlotName,"_",varname,".png")), device = "png")
  }
} 
# 
# Plot every 10 years 
max_values <- matrix(c(.25, .25, .12, .25, .2,
                       .8, .65, .5, .8, .7,
                       .5, .45, .45, .08, .35,
                       .06, .085, .06, .0125, .06,
                       .25, .25, .12, .25, .2,
                       .8, .65, .5, .8, .6,
                       .5, .45, .45, .08, .35,
                       .06, .085, .06, .0125, .06), nrow = 12, ncol = 5, byrow = TRUE)
# load file for spectral analyses

col_names <- c("Lat","Lon",
               "InterAnnual1", "Annual1", "IntraAnnual1", "Synoptic1",
               "InterAnnual2", "Annual2", "IntraAnnual2", "Synoptic2",
               "TrendLinear1", "Trend1", "Avg1", "Std1",
               "TrendLinear2", "Trend2", "Avg2", "Std2")

for (ivar in 1:5) {
  varname <- switch(ivar,
                    'bottomT',
                    'Salinity',
                    'ssh',
                    'sst',
                    'mixedlayer')
  varnamePretty <- switch(ivar,
                          'Bottom Temperature',
                          'Salinity',
                          'SSH',
                          'SST',
                          'Mixed Layer')
  filename <- (gsub(" ","",paste("./csv_files/",varname,"_spectral.csv")))
  ref  <- read.csv(filename)
  
  # we have 12 different analyses
  for (i in 3:18) {
    PlotName <- col_names[i]
    # create a plot
    if (i <= 10){
      MaxV <- max_values[i-2,ivar]
      ggplot() + # Declaration of local variables for the plot
        geom_polygon(data=maps, aes(x=lon, y=lat),fill = "grey70") +
        coord_quickmap(expand=FALSE, xlim = c(min(ref$Lon)-1, max(ref$Lon)), ylim = c(min(ref$Lat),max(ref$Lat))) +
        geom_point(data=ref, aes(x = Lon, y=Lat, color=!!sym(PlotName))) +
        scale_colour_viridis_c(limits = c(0, MaxV)) +
        geom_contour(data = bathy_df, aes(x = x, y = y, z = z), 
                     breaks = c(-50, -400, -700), colour = "grey70", linewidth = 0.5) +
        labs(x = "Longitude",y = "Latitude") +
        theme(text = element_text(size = 20)) +
        theme_light()
    } else {
      pl <- ggplot() + # Declaration of local variables for the plot
        geom_polygon(data=maps, aes(x=lon, y=lat),fill = "grey70") +
        coord_quickmap(expand=FALSE,
                       xlim = c(min(ref$Lon)-1,max(ref$Lon)), 
                       ylim = c(min(ref$Lat),max(ref$Lat))) +
        geom_point(data=ref, aes(x = Lon, y=Lat, color=!!sym(PlotName))) +
        geom_contour(data = bathy_df, aes(x = x, y = y, z = z), 
                     breaks = c(-50, -400, -700), 
                     colour = "grey70", linewidth = 0.5) +
        labs(x = "Longitude",y = "Latitude", title = varnamePretty) +
        theme(text = element_text(size = 20)) +
        theme_light()
        if (i == 14 | i == 18 | (i == 13 & ivar != 3) | (i == 17 & ivar != 3))
          pl + scale_colour_viridis_c()
        else
          pl + scale_colour_gradient2(
            low = "dodgerblue4", mid = "white", high = "firebrick4",
            midpoint = 0)
    }
    ggsave(gsub(" ","",paste("./images/analyses/",PlotName,"_",varname,".png")),device = "png")
  }
}
# PLOT SEASONAL
max_values <- matrix(c(.35, .6, .3, .35, .6,
                       .7, .35, .4, .7, .6,
                       .75, .8, .5, .7, .75), nrow = 3, ncol = 5, byrow = TRUE)

col_names <- c("Lat", "Lon", "Constancy", "Contigency", "Predictability")

# plot now in space each one
# load the file for each variable, then rotate to plot each column (analyes results)
for (ivar in 1:5) {
  varname <- switch(ivar,
                    'bottomT',
                    'Salinity',
                    'ssh',
                    'sst',
                    'mixedlayer')
  varnamePretty <- switch(ivar,
                          'Bottom Temperature',
                          'Salinity',
                          'SSH',
                          'SST',
                          'Mixed Layer')
  for (iQ in 1:4) {
    qnumber <- switch(iQ,
                      'Q1',
                      'Q2',
                      'Q3',
                      'Q4')
    qname <- switch(iQ,
                    'Winter',
                    'Spring',
                    'Summer',
                    'Fall')
    filename <- (gsub(" ","",paste("./csv_files/",varname,"_analyses_",qname,".csv")))
    ref  <- read.csv(filename)
    
    # we have 3 different analyses  
    for (i in 3:5) {
      PlotName <- col_names[i]
      # save the maximum value for the plot accross all years
      MaxV <- max_values[i-2,ivar]
      # create a plot
      ggplot() + # Declaration of local variables for the plot
        geom_polygon(data=maps, aes(x=lon, y=lat),fill = "grey70") +
        coord_quickmap(expand=FALSE,xlim = c(min(ref$Lon)-1, max(ref$Lon)), ylim = c(min(ref$Lat),max(ref$Lat))) +
        geom_point(data=ref, aes(x = Lon, y=Lat, color=!!sym(PlotName))) +
        scale_colour_viridis_c(limits = c(0, MaxV)) +
        geom_contour(data = bathy_df, aes(x = x, y = y, z = z), 
                     breaks = c(-50, -400, -700), colour = "grey70", linewidth = 0.5) +
        labs(x = "Longitude",y = "Latitude", title = varnamePretty) +
        theme(text = element_text(size = 20)) +
        theme_light()
      ggsave(gsub(" ","",paste("./images/analyses/",PlotName,"_",varname,"_",qname,".png")),device = "png")
    }
  }
}

# load file for envPred
col_names <- c("Lat","Lon", "Constancy1", "Contigency1", "Predictability1",
                            "Constancy2", "Contigency2", "Predictability2")
# change matrix to repeat for analyses
max_values <- matrix(c(.35, .55, .3, .15, .6,
                       .7, .35, .4, .7, .6,
                       .75, .6, .5, .7, .75,
                       .35, .55, .3, .15, .6,
                       .7, .35, .4, .7, .6,
                       .75, .6, .5, .7, .75), 
                     nrow = 6, ncol = 5, byrow = TRUE)


for (ivar in 1:5) {
  varname <- switch(ivar,
                    'bottomT',
                    'Salinity',
                    'ssh',
                    'sst',
                    'mixedlayer')
  varnamePretty <- switch(ivar,
                          'Bottom Temperature',
                          'Salinity',
                          'SSH',
                          'SST',
                          'Mixed Layer')
  for (iQ in 1:4) {
    qnumber <- switch(iQ,
                      'Q1',
                      'Q2',
                      'Q3',
                      'Q4')
    qname <- switch(iQ,
                    'Winter',
                    'Spring',
                    'Summer',
                    'Fall')
    filename <- (gsub(" ","",paste("./csv_files/",varname,"_envPred_",qname,".csv")))
    ref  <- read.csv(filename)
    
    # we have 12 different analyses  
    for (i in 3:8) {
      # save the maximum value for the plot accross all years
      MaxV <- max_values[i-2,ivar]
      PlotName <- col_names[i]
      # create a plot
      ggplot() + # Declaration of local variables for the plot
        geom_polygon(data=maps, aes(x=lon, y=lat),fill = "grey70") +
        coord_quickmap(expand=FALSE,xlim = c(min(ref$Lon)-1, max(ref$Lon)), ylim = c(min(ref$Lat),max(ref$Lat))) +
        geom_point(data=ref, aes(x = Lon, y=Lat, color=!!sym(PlotName))) +
        scale_colour_viridis_c(limits = c(0, MaxV)) +
        geom_contour(data = bathy_df, aes(x = x, y = y, z = z), 
                     breaks = c(-50, -400, -700), colour = "grey70", linewidth = 0.5) +
        labs(x = "Longitude",y = "Latitude", title = varnamePretty) +
        theme(text = element_text(size = 20)) +
        theme_light()
      ggsave(gsub(" ","",paste("./images/analyses/",PlotName,"_",varname,"_",qname,".png")),device = "png")
    }
  }
} 