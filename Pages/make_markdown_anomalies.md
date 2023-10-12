Anomalies
================

``` r
pathfigs <- ("../images/CNAPS_constant/")

# Names of Variables to plot
NamePlot   <- c("Bottom Temperature","Salinity","SSH","SST","Mixed Layer")
NameSeason <- c("Winter","Spring","Summer","Fall")
NameSeasonSp <- c("Winter","Summer")
NameModes  <- c("Mode 1","Mode 2")

myname <- c("bottomT","Salinity","ssh","sst","mixedlayer")
mynameUp <- c("BottomT","Salinity","SSH","SST","MixedLayer")

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeason,NamePlot)
NamePlotModesSeason <- paste(aux$Var3, aux$Var2, aux$Var1)

# create combinations of names and combine the names
aux <- expand.grid(NameModes,NameSeasonSp,NamePlot)
NamePlotModesSpawnSeason <- paste(aux$Var3, aux$Var2, aux$Var1)

# create combinations of names and combine the names
aux <- expand.grid(NamePlot, NameModes)
NamePlotModes <- paste(aux$Var1, aux$Var2)

spNamePlot <- c("Gag Grouper","Greater Amberjack","Gray Triggerfish"," Red Porgy","Red Grouper","Black Sea Bass","Red Snapper","Scamp","Snowy Grouper ","Vermilion Snapper")

NameIndexes <- c("AMO","MEI","NAO")

cat("# Monthly anomalies \n\n")
```

    # Monthly anomalies 

``` r
cat("::: {layout-ncol=\"2\"}\n\n")
```

    ::: {layout-ncol="2"}

``` r
# first plot anomlies series monthly 
for (iVar in seq_along(NamePlot)) {
  cat(paste0("## ", NamePlot[iVar], "\n"))
  fig_name <- paste0(pathfigs, myname[iVar],'_anomaly.png')
  cat(paste0('![](', fig_name, ')\n\n'),sep="")
}
```

    ## Bottom Temperature
    ![](../images/CNAPS_constant/bottomT_anomaly.png)

    ## Salinity
    ![](../images/CNAPS_constant/Salinity_anomaly.png)

    ## SSH
    ![](../images/CNAPS_constant/ssh_anomaly.png)

    ## SST
    ![](../images/CNAPS_constant/sst_anomaly.png)

    ## Mixed Layer
    ![](../images/CNAPS_constant/mixedlayer_anomaly.png)

``` r
cat("::: \n\n")
```

    ::: 

``` r
cat("# Spawning season anomalies \n\n")
```

    # Spawning season anomalies 

``` r
cat("::: {layout-ncol=\"2\"}\n\n")
```

    ::: {layout-ncol="2"}

``` r
# first plot anomalies series monthly 
for (iVar in seq_along(NamePlot)) {
  for (iSeason in seq_along(NameSeasonSp)) {
    cat(paste0("## ", NamePlot[iVar],' ',NameSeasonSp[iSeason], "\n"))
    fig_name <- paste0(pathfigs, myname[iVar],'_Spawning_',NameSeasonSp[iSeason],'_anomaly.png')
    cat(paste0('![](', fig_name, ')\n\n'),sep="")
  }
}
```

    ## Bottom Temperature Winter
    ![](../images/CNAPS_constant/bottomT_Spawning_Winter_anomaly.png)

    ## Bottom Temperature Summer
    ![](../images/CNAPS_constant/bottomT_Spawning_Summer_anomaly.png)

    ## Salinity Winter
    ![](../images/CNAPS_constant/Salinity_Spawning_Winter_anomaly.png)

    ## Salinity Summer
    ![](../images/CNAPS_constant/Salinity_Spawning_Summer_anomaly.png)

    ## SSH Winter
    ![](../images/CNAPS_constant/ssh_Spawning_Winter_anomaly.png)

    ## SSH Summer
    ![](../images/CNAPS_constant/ssh_Spawning_Summer_anomaly.png)

    ## SST Winter
    ![](../images/CNAPS_constant/sst_Spawning_Winter_anomaly.png)

    ## SST Summer
    ![](../images/CNAPS_constant/sst_Spawning_Summer_anomaly.png)

    ## Mixed Layer Winter
    ![](../images/CNAPS_constant/mixedlayer_Spawning_Winter_anomaly.png)

    ## Mixed Layer Summer
    ![](../images/CNAPS_constant/mixedlayer_Spawning_Summer_anomaly.png)

``` r
cat("::: \n\n")
```

    ::: 

``` r
cat("# Seasonal anomalies \n\n")
```

    # Seasonal anomalies 

``` r
cat("::: {layout-ncol=\"2\"}\n\n")
```

    ::: {layout-ncol="2"}

``` r
# first plot anomalies series monthly 
for (iVar in seq_along(NamePlot)) {
  for (iSeason in seq_along(NameSeason)) {
    cat(paste0("## ", NamePlot[iVar],' ',NameSeason[iSeason], "\n"))
    fig_name <- paste0(pathfigs, myname[iVar],'_',NameSeason[iSeason],'_anomaly_constant.png')
    cat(paste0('![](', fig_name, ')\n\n'),sep="")
  }
}
```

    ## Bottom Temperature Winter
    ![](../images/CNAPS_constant/bottomT_Winter_anomaly_constant.png)

    ## Bottom Temperature Spring
    ![](../images/CNAPS_constant/bottomT_Spring_anomaly_constant.png)

    ## Bottom Temperature Summer
    ![](../images/CNAPS_constant/bottomT_Summer_anomaly_constant.png)

    ## Bottom Temperature Fall
    ![](../images/CNAPS_constant/bottomT_Fall_anomaly_constant.png)

    ## Salinity Winter
    ![](../images/CNAPS_constant/Salinity_Winter_anomaly_constant.png)

    ## Salinity Spring
    ![](../images/CNAPS_constant/Salinity_Spring_anomaly_constant.png)

    ## Salinity Summer
    ![](../images/CNAPS_constant/Salinity_Summer_anomaly_constant.png)

    ## Salinity Fall
    ![](../images/CNAPS_constant/Salinity_Fall_anomaly_constant.png)

    ## SSH Winter
    ![](../images/CNAPS_constant/ssh_Winter_anomaly_constant.png)

    ## SSH Spring
    ![](../images/CNAPS_constant/ssh_Spring_anomaly_constant.png)

    ## SSH Summer
    ![](../images/CNAPS_constant/ssh_Summer_anomaly_constant.png)

    ## SSH Fall
    ![](../images/CNAPS_constant/ssh_Fall_anomaly_constant.png)

    ## SST Winter
    ![](../images/CNAPS_constant/sst_Winter_anomaly_constant.png)

    ## SST Spring
    ![](../images/CNAPS_constant/sst_Spring_anomaly_constant.png)

    ## SST Summer
    ![](../images/CNAPS_constant/sst_Summer_anomaly_constant.png)

    ## SST Fall
    ![](../images/CNAPS_constant/sst_Fall_anomaly_constant.png)

    ## Mixed Layer Winter
    ![](../images/CNAPS_constant/mixedlayer_Winter_anomaly_constant.png)

    ## Mixed Layer Spring
    ![](../images/CNAPS_constant/mixedlayer_Spring_anomaly_constant.png)

    ## Mixed Layer Summer
    ![](../images/CNAPS_constant/mixedlayer_Summer_anomaly_constant.png)

    ## Mixed Layer Fall
    ![](../images/CNAPS_constant/mixedlayer_Fall_anomaly_constant.png)

``` r
cat("::: \n\n")
```

    ::: 

``` r
cat("# Seasonal anomalies (all months) \n\n")
```

    # Seasonal anomalies (all months) 

``` r
cat("::: {layout-ncol=\"2\"}\n\n")
```

    ::: {layout-ncol="2"}

``` r
# first plot anomalies series monthly 
for (iVar in seq_along(NamePlot)) {
  for (iSeason in seq_along(NameSeason)) {
    cat(paste0("## ", NamePlot[iVar],' ',NameSeason[iSeason], "\n"))
    fig_name <- paste0(pathfigs, myname[iVar],'_',NameSeason[iSeason],'_AllM_anomaly_constant.png')
    cat(paste0('![](', fig_name, ')\n\n'),sep="")
  }
}
```

    ## Bottom Temperature Winter
    ![](../images/CNAPS_constant/bottomT_Winter_AllM_anomaly_constant.png)

    ## Bottom Temperature Spring
    ![](../images/CNAPS_constant/bottomT_Spring_AllM_anomaly_constant.png)

    ## Bottom Temperature Summer
    ![](../images/CNAPS_constant/bottomT_Summer_AllM_anomaly_constant.png)

    ## Bottom Temperature Fall
    ![](../images/CNAPS_constant/bottomT_Fall_AllM_anomaly_constant.png)

    ## Salinity Winter
    ![](../images/CNAPS_constant/Salinity_Winter_AllM_anomaly_constant.png)

    ## Salinity Spring
    ![](../images/CNAPS_constant/Salinity_Spring_AllM_anomaly_constant.png)

    ## Salinity Summer
    ![](../images/CNAPS_constant/Salinity_Summer_AllM_anomaly_constant.png)

    ## Salinity Fall
    ![](../images/CNAPS_constant/Salinity_Fall_AllM_anomaly_constant.png)

    ## SSH Winter
    ![](../images/CNAPS_constant/ssh_Winter_AllM_anomaly_constant.png)

    ## SSH Spring
    ![](../images/CNAPS_constant/ssh_Spring_AllM_anomaly_constant.png)

    ## SSH Summer
    ![](../images/CNAPS_constant/ssh_Summer_AllM_anomaly_constant.png)

    ## SSH Fall
    ![](../images/CNAPS_constant/ssh_Fall_AllM_anomaly_constant.png)

    ## SST Winter
    ![](../images/CNAPS_constant/sst_Winter_AllM_anomaly_constant.png)

    ## SST Spring
    ![](../images/CNAPS_constant/sst_Spring_AllM_anomaly_constant.png)

    ## SST Summer
    ![](../images/CNAPS_constant/sst_Summer_AllM_anomaly_constant.png)

    ## SST Fall
    ![](../images/CNAPS_constant/sst_Fall_AllM_anomaly_constant.png)

    ## Mixed Layer Winter
    ![](../images/CNAPS_constant/mixedlayer_Winter_AllM_anomaly_constant.png)

    ## Mixed Layer Spring
    ![](../images/CNAPS_constant/mixedlayer_Spring_AllM_anomaly_constant.png)

    ## Mixed Layer Summer
    ![](../images/CNAPS_constant/mixedlayer_Summer_AllM_anomaly_constant.png)

    ## Mixed Layer Fall
    ![](../images/CNAPS_constant/mixedlayer_Fall_AllM_anomaly_constant.png)

``` r
cat("::: \n\n")
```

    ::: 
