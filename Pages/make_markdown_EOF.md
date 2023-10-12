EOF
================

``` r
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

pathfigs <- ("../images/CNAPS_constant/")

cat("# EOF Monthly \n\n")
```

    # EOF Monthly 

``` r
for (iVar in seq_along(NamePlot)) {
  cat("::: {layout-ncol=\"2\"}\n\n")
  
  for (iMode in seq_along(NameModes)) {
    cat(paste0("## ", NamePlot[iVar], " ",NameModes[iMode], "\n\n"))
    fig_name <- paste0(pathfigs, myname[iVar],'_constant_mode',iMode,'_CNAPS.png')
    cat(paste0('![](', fig_name, ')\n'),sep="")
    fig_name <- paste0(pathfigs, myname[iVar],'_constant_temporal_mode',iMode,'_CNAPS.png')
    cat(paste0('![](', fig_name, ')\n\n'))
  }
  cat("::: \n\n")
}
```

    ::: {layout-ncol="2"}

    ## Bottom Temperature Mode 1

    ![](../images/CNAPS_constant/bottomT_constant_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_temporal_mode1_CNAPS.png)

    ## Bottom Temperature Mode 2

    ![](../images/CNAPS_constant/bottomT_constant_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_temporal_mode2_CNAPS.png)

    ::: 

    ::: {layout-ncol="2"}

    ## Salinity Mode 1

    ![](../images/CNAPS_constant/Salinity_constant_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_temporal_mode1_CNAPS.png)

    ## Salinity Mode 2

    ![](../images/CNAPS_constant/Salinity_constant_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_temporal_mode2_CNAPS.png)

    ::: 

    ::: {layout-ncol="2"}

    ## SSH Mode 1

    ![](../images/CNAPS_constant/ssh_constant_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_temporal_mode1_CNAPS.png)

    ## SSH Mode 2

    ![](../images/CNAPS_constant/ssh_constant_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_temporal_mode2_CNAPS.png)

    ::: 

    ::: {layout-ncol="2"}

    ## SST Mode 1

    ![](../images/CNAPS_constant/sst_constant_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_temporal_mode1_CNAPS.png)

    ## SST Mode 2

    ![](../images/CNAPS_constant/sst_constant_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_temporal_mode2_CNAPS.png)

    ::: 

    ::: {layout-ncol="2"}

    ## Mixed Layer Mode 1

    ![](../images/CNAPS_constant/mixedlayer_constant_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_temporal_mode1_CNAPS.png)

    ## Mixed Layer Mode 2

    ![](../images/CNAPS_constant/mixedlayer_constant_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_temporal_mode2_CNAPS.png)

    ::: 

``` r
cat("# EOF Seasonal \n\n")
```

    # EOF Seasonal 

``` r
for (iVar in seq_along(NamePlot)) {
  cat(paste0("## ", NamePlot[iVar], "\n\n"))
  cat("::: {layout-ncol=\"2\"}\n\n")
  
  for (iSeason in seq_along(NameSeason)) {
    for (iMode in seq_along(NameModes)) {
      cat(paste0("### ", NamePlot[iVar]," ",NameSeason[iSeason]," ",NameModes[iMode], "\n\n"))
      fig_name <- paste0(pathfigs, myname[iVar],'_constant_',NameSeason[iSeason],'_mode',iMode,'_CNAPS.png')
      cat(paste0('![](', fig_name, ')\n'))
      fig_name <- paste0(pathfigs, myname[iVar],'_constant_',NameSeason[iSeason],'_temporal_mode',iMode,'_CNAPS.png')
      cat(paste0('![](', fig_name, ')\n\n'))
    }  
  }
  cat("::: \n\n")
}
```

    ## Bottom Temperature

    ::: {layout-ncol="2"}

    ### Bottom Temperature Winter Mode 1

    ![](../images/CNAPS_constant/bottomT_constant_Winter_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Winter_temporal_mode1_CNAPS.png)

    ### Bottom Temperature Winter Mode 2

    ![](../images/CNAPS_constant/bottomT_constant_Winter_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Winter_temporal_mode2_CNAPS.png)

    ### Bottom Temperature Spring Mode 1

    ![](../images/CNAPS_constant/bottomT_constant_Spring_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Spring_temporal_mode1_CNAPS.png)

    ### Bottom Temperature Spring Mode 2

    ![](../images/CNAPS_constant/bottomT_constant_Spring_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Spring_temporal_mode2_CNAPS.png)

    ### Bottom Temperature Summer Mode 1

    ![](../images/CNAPS_constant/bottomT_constant_Summer_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Summer_temporal_mode1_CNAPS.png)

    ### Bottom Temperature Summer Mode 2

    ![](../images/CNAPS_constant/bottomT_constant_Summer_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Summer_temporal_mode2_CNAPS.png)

    ### Bottom Temperature Fall Mode 1

    ![](../images/CNAPS_constant/bottomT_constant_Fall_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Fall_temporal_mode1_CNAPS.png)

    ### Bottom Temperature Fall Mode 2

    ![](../images/CNAPS_constant/bottomT_constant_Fall_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Fall_temporal_mode2_CNAPS.png)

    ::: 

    ## Salinity

    ::: {layout-ncol="2"}

    ### Salinity Winter Mode 1

    ![](../images/CNAPS_constant/Salinity_constant_Winter_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Winter_temporal_mode1_CNAPS.png)

    ### Salinity Winter Mode 2

    ![](../images/CNAPS_constant/Salinity_constant_Winter_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Winter_temporal_mode2_CNAPS.png)

    ### Salinity Spring Mode 1

    ![](../images/CNAPS_constant/Salinity_constant_Spring_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Spring_temporal_mode1_CNAPS.png)

    ### Salinity Spring Mode 2

    ![](../images/CNAPS_constant/Salinity_constant_Spring_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Spring_temporal_mode2_CNAPS.png)

    ### Salinity Summer Mode 1

    ![](../images/CNAPS_constant/Salinity_constant_Summer_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Summer_temporal_mode1_CNAPS.png)

    ### Salinity Summer Mode 2

    ![](../images/CNAPS_constant/Salinity_constant_Summer_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Summer_temporal_mode2_CNAPS.png)

    ### Salinity Fall Mode 1

    ![](../images/CNAPS_constant/Salinity_constant_Fall_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Fall_temporal_mode1_CNAPS.png)

    ### Salinity Fall Mode 2

    ![](../images/CNAPS_constant/Salinity_constant_Fall_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Fall_temporal_mode2_CNAPS.png)

    ::: 

    ## SSH

    ::: {layout-ncol="2"}

    ### SSH Winter Mode 1

    ![](../images/CNAPS_constant/ssh_constant_Winter_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Winter_temporal_mode1_CNAPS.png)

    ### SSH Winter Mode 2

    ![](../images/CNAPS_constant/ssh_constant_Winter_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Winter_temporal_mode2_CNAPS.png)

    ### SSH Spring Mode 1

    ![](../images/CNAPS_constant/ssh_constant_Spring_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Spring_temporal_mode1_CNAPS.png)

    ### SSH Spring Mode 2

    ![](../images/CNAPS_constant/ssh_constant_Spring_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Spring_temporal_mode2_CNAPS.png)

    ### SSH Summer Mode 1

    ![](../images/CNAPS_constant/ssh_constant_Summer_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Summer_temporal_mode1_CNAPS.png)

    ### SSH Summer Mode 2

    ![](../images/CNAPS_constant/ssh_constant_Summer_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Summer_temporal_mode2_CNAPS.png)

    ### SSH Fall Mode 1

    ![](../images/CNAPS_constant/ssh_constant_Fall_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Fall_temporal_mode1_CNAPS.png)

    ### SSH Fall Mode 2

    ![](../images/CNAPS_constant/ssh_constant_Fall_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Fall_temporal_mode2_CNAPS.png)

    ::: 

    ## SST

    ::: {layout-ncol="2"}

    ### SST Winter Mode 1

    ![](../images/CNAPS_constant/sst_constant_Winter_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Winter_temporal_mode1_CNAPS.png)

    ### SST Winter Mode 2

    ![](../images/CNAPS_constant/sst_constant_Winter_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Winter_temporal_mode2_CNAPS.png)

    ### SST Spring Mode 1

    ![](../images/CNAPS_constant/sst_constant_Spring_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Spring_temporal_mode1_CNAPS.png)

    ### SST Spring Mode 2

    ![](../images/CNAPS_constant/sst_constant_Spring_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Spring_temporal_mode2_CNAPS.png)

    ### SST Summer Mode 1

    ![](../images/CNAPS_constant/sst_constant_Summer_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Summer_temporal_mode1_CNAPS.png)

    ### SST Summer Mode 2

    ![](../images/CNAPS_constant/sst_constant_Summer_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Summer_temporal_mode2_CNAPS.png)

    ### SST Fall Mode 1

    ![](../images/CNAPS_constant/sst_constant_Fall_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Fall_temporal_mode1_CNAPS.png)

    ### SST Fall Mode 2

    ![](../images/CNAPS_constant/sst_constant_Fall_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Fall_temporal_mode2_CNAPS.png)

    ::: 

    ## Mixed Layer

    ::: {layout-ncol="2"}

    ### Mixed Layer Winter Mode 1

    ![](../images/CNAPS_constant/mixedlayer_constant_Winter_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Winter_temporal_mode1_CNAPS.png)

    ### Mixed Layer Winter Mode 2

    ![](../images/CNAPS_constant/mixedlayer_constant_Winter_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Winter_temporal_mode2_CNAPS.png)

    ### Mixed Layer Spring Mode 1

    ![](../images/CNAPS_constant/mixedlayer_constant_Spring_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Spring_temporal_mode1_CNAPS.png)

    ### Mixed Layer Spring Mode 2

    ![](../images/CNAPS_constant/mixedlayer_constant_Spring_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Spring_temporal_mode2_CNAPS.png)

    ### Mixed Layer Summer Mode 1

    ![](../images/CNAPS_constant/mixedlayer_constant_Summer_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Summer_temporal_mode1_CNAPS.png)

    ### Mixed Layer Summer Mode 2

    ![](../images/CNAPS_constant/mixedlayer_constant_Summer_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Summer_temporal_mode2_CNAPS.png)

    ### Mixed Layer Fall Mode 1

    ![](../images/CNAPS_constant/mixedlayer_constant_Fall_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Fall_temporal_mode1_CNAPS.png)

    ### Mixed Layer Fall Mode 2

    ![](../images/CNAPS_constant/mixedlayer_constant_Fall_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Fall_temporal_mode2_CNAPS.png)

    ::: 

``` r
cat("# EOF Seasonal Spawning \n\n")
```

    # EOF Seasonal Spawning 

``` r
for (iVar in seq_along(NamePlot)) {
  cat(paste0("## ", NamePlot[iVar], "\n\n"))
  cat("::: {layout-ncol=\"2\"}\n\n")
  
  for (iSeason in seq_along(NameSeasonSp)) {
    for (iMode in seq_along(NameModes)) {
      cat(paste0("### ", NamePlot[iVar]," Spawning ",NameSeasonSp[iSeason]," ",NameModes[iMode], "\n\n"))
      fig_name <- paste0(pathfigs, myname[iVar],'_constant_Spawning_',NameSeasonSp[iSeason],'_mode',iMode,'_CNAPS.png')
      cat(paste0('![](', fig_name, ')\n'))
      fig_name <- paste0(pathfigs, myname[iVar],'_constant_Spawning_',NameSeasonSp[iSeason],'_temporal_mode',iMode,'_CNAPS.png')
      cat(paste0('![](', fig_name, ')\n\n'))
    }  
  }  
  cat("::: \n\n")
}
```

    ## Bottom Temperature

    ::: {layout-ncol="2"}

    ### Bottom Temperature Spawning Winter Mode 1

    ![](../images/CNAPS_constant/bottomT_constant_Spawning_Winter_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Spawning_Winter_temporal_mode1_CNAPS.png)

    ### Bottom Temperature Spawning Winter Mode 2

    ![](../images/CNAPS_constant/bottomT_constant_Spawning_Winter_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Spawning_Winter_temporal_mode2_CNAPS.png)

    ### Bottom Temperature Spawning Summer Mode 1

    ![](../images/CNAPS_constant/bottomT_constant_Spawning_Summer_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Spawning_Summer_temporal_mode1_CNAPS.png)

    ### Bottom Temperature Spawning Summer Mode 2

    ![](../images/CNAPS_constant/bottomT_constant_Spawning_Summer_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Spawning_Summer_temporal_mode2_CNAPS.png)

    ::: 

    ## Salinity

    ::: {layout-ncol="2"}

    ### Salinity Spawning Winter Mode 1

    ![](../images/CNAPS_constant/Salinity_constant_Spawning_Winter_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Spawning_Winter_temporal_mode1_CNAPS.png)

    ### Salinity Spawning Winter Mode 2

    ![](../images/CNAPS_constant/Salinity_constant_Spawning_Winter_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Spawning_Winter_temporal_mode2_CNAPS.png)

    ### Salinity Spawning Summer Mode 1

    ![](../images/CNAPS_constant/Salinity_constant_Spawning_Summer_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Spawning_Summer_temporal_mode1_CNAPS.png)

    ### Salinity Spawning Summer Mode 2

    ![](../images/CNAPS_constant/Salinity_constant_Spawning_Summer_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Spawning_Summer_temporal_mode2_CNAPS.png)

    ::: 

    ## SSH

    ::: {layout-ncol="2"}

    ### SSH Spawning Winter Mode 1

    ![](../images/CNAPS_constant/ssh_constant_Spawning_Winter_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Spawning_Winter_temporal_mode1_CNAPS.png)

    ### SSH Spawning Winter Mode 2

    ![](../images/CNAPS_constant/ssh_constant_Spawning_Winter_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Spawning_Winter_temporal_mode2_CNAPS.png)

    ### SSH Spawning Summer Mode 1

    ![](../images/CNAPS_constant/ssh_constant_Spawning_Summer_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Spawning_Summer_temporal_mode1_CNAPS.png)

    ### SSH Spawning Summer Mode 2

    ![](../images/CNAPS_constant/ssh_constant_Spawning_Summer_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Spawning_Summer_temporal_mode2_CNAPS.png)

    ::: 

    ## SST

    ::: {layout-ncol="2"}

    ### SST Spawning Winter Mode 1

    ![](../images/CNAPS_constant/sst_constant_Spawning_Winter_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Spawning_Winter_temporal_mode1_CNAPS.png)

    ### SST Spawning Winter Mode 2

    ![](../images/CNAPS_constant/sst_constant_Spawning_Winter_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Spawning_Winter_temporal_mode2_CNAPS.png)

    ### SST Spawning Summer Mode 1

    ![](../images/CNAPS_constant/sst_constant_Spawning_Summer_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Spawning_Summer_temporal_mode1_CNAPS.png)

    ### SST Spawning Summer Mode 2

    ![](../images/CNAPS_constant/sst_constant_Spawning_Summer_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Spawning_Summer_temporal_mode2_CNAPS.png)

    ::: 

    ## Mixed Layer

    ::: {layout-ncol="2"}

    ### Mixed Layer Spawning Winter Mode 1

    ![](../images/CNAPS_constant/mixedlayer_constant_Spawning_Winter_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Spawning_Winter_temporal_mode1_CNAPS.png)

    ### Mixed Layer Spawning Winter Mode 2

    ![](../images/CNAPS_constant/mixedlayer_constant_Spawning_Winter_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Spawning_Winter_temporal_mode2_CNAPS.png)

    ### Mixed Layer Spawning Summer Mode 1

    ![](../images/CNAPS_constant/mixedlayer_constant_Spawning_Summer_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Spawning_Summer_temporal_mode1_CNAPS.png)

    ### Mixed Layer Spawning Summer Mode 2

    ![](../images/CNAPS_constant/mixedlayer_constant_Spawning_Summer_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Spawning_Summer_temporal_mode2_CNAPS.png)

    ::: 

``` r
NameSeasonSp <- c("Winter","Spring")

cat("# EOF Seasonal Spawning All Months \n\n")
```

    # EOF Seasonal Spawning All Months 

``` r
for (iVar in seq_along(NamePlot)) {
  cat(paste0("## ", NamePlot[iVar], "\n\n"))
  cat("::: {layout-ncol=\"2\"}\n\n")
  
  for (iSeason in seq_along(NameSeasonSp)) {
    for (iMode in seq_along(NameModes)) {
      cat(paste0("### ", NamePlot[iVar]," Spawning ",NameSeasonSp[iSeason]," ",NameModes[iMode], "\n\n"))
      fig_name <- paste0(pathfigs, myname[iVar],'_constant_',NameSeasonSp[iSeason],'_Sp_AllM_mode',iMode,'_CNAPS.png')
      cat(paste0('![](', fig_name, ')\n'))
      fig_name <- paste0(pathfigs, myname[iVar],'_constant_',NameSeasonSp[iSeason],'_Sp_AllM_temporal_mode',iMode,'_CNAPS.png')
      cat(paste0('![](', fig_name, ')\n\n'))
    }  
  }  
  cat("::: \n\n")
}
```

    ## Bottom Temperature

    ::: {layout-ncol="2"}

    ### Bottom Temperature Spawning Winter Mode 1

    ![](../images/CNAPS_constant/bottomT_constant_Winter_Sp_AllM_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Winter_Sp_AllM_temporal_mode1_CNAPS.png)

    ### Bottom Temperature Spawning Winter Mode 2

    ![](../images/CNAPS_constant/bottomT_constant_Winter_Sp_AllM_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Winter_Sp_AllM_temporal_mode2_CNAPS.png)

    ### Bottom Temperature Spawning Spring Mode 1

    ![](../images/CNAPS_constant/bottomT_constant_Spring_Sp_AllM_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Spring_Sp_AllM_temporal_mode1_CNAPS.png)

    ### Bottom Temperature Spawning Spring Mode 2

    ![](../images/CNAPS_constant/bottomT_constant_Spring_Sp_AllM_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/bottomT_constant_Spring_Sp_AllM_temporal_mode2_CNAPS.png)

    ::: 

    ## Salinity

    ::: {layout-ncol="2"}

    ### Salinity Spawning Winter Mode 1

    ![](../images/CNAPS_constant/Salinity_constant_Winter_Sp_AllM_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Winter_Sp_AllM_temporal_mode1_CNAPS.png)

    ### Salinity Spawning Winter Mode 2

    ![](../images/CNAPS_constant/Salinity_constant_Winter_Sp_AllM_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Winter_Sp_AllM_temporal_mode2_CNAPS.png)

    ### Salinity Spawning Spring Mode 1

    ![](../images/CNAPS_constant/Salinity_constant_Spring_Sp_AllM_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Spring_Sp_AllM_temporal_mode1_CNAPS.png)

    ### Salinity Spawning Spring Mode 2

    ![](../images/CNAPS_constant/Salinity_constant_Spring_Sp_AllM_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/Salinity_constant_Spring_Sp_AllM_temporal_mode2_CNAPS.png)

    ::: 

    ## SSH

    ::: {layout-ncol="2"}

    ### SSH Spawning Winter Mode 1

    ![](../images/CNAPS_constant/ssh_constant_Winter_Sp_AllM_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Winter_Sp_AllM_temporal_mode1_CNAPS.png)

    ### SSH Spawning Winter Mode 2

    ![](../images/CNAPS_constant/ssh_constant_Winter_Sp_AllM_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Winter_Sp_AllM_temporal_mode2_CNAPS.png)

    ### SSH Spawning Spring Mode 1

    ![](../images/CNAPS_constant/ssh_constant_Spring_Sp_AllM_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Spring_Sp_AllM_temporal_mode1_CNAPS.png)

    ### SSH Spawning Spring Mode 2

    ![](../images/CNAPS_constant/ssh_constant_Spring_Sp_AllM_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/ssh_constant_Spring_Sp_AllM_temporal_mode2_CNAPS.png)

    ::: 

    ## SST

    ::: {layout-ncol="2"}

    ### SST Spawning Winter Mode 1

    ![](../images/CNAPS_constant/sst_constant_Winter_Sp_AllM_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Winter_Sp_AllM_temporal_mode1_CNAPS.png)

    ### SST Spawning Winter Mode 2

    ![](../images/CNAPS_constant/sst_constant_Winter_Sp_AllM_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Winter_Sp_AllM_temporal_mode2_CNAPS.png)

    ### SST Spawning Spring Mode 1

    ![](../images/CNAPS_constant/sst_constant_Spring_Sp_AllM_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Spring_Sp_AllM_temporal_mode1_CNAPS.png)

    ### SST Spawning Spring Mode 2

    ![](../images/CNAPS_constant/sst_constant_Spring_Sp_AllM_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/sst_constant_Spring_Sp_AllM_temporal_mode2_CNAPS.png)

    ::: 

    ## Mixed Layer

    ::: {layout-ncol="2"}

    ### Mixed Layer Spawning Winter Mode 1

    ![](../images/CNAPS_constant/mixedlayer_constant_Winter_Sp_AllM_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Winter_Sp_AllM_temporal_mode1_CNAPS.png)

    ### Mixed Layer Spawning Winter Mode 2

    ![](../images/CNAPS_constant/mixedlayer_constant_Winter_Sp_AllM_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Winter_Sp_AllM_temporal_mode2_CNAPS.png)

    ### Mixed Layer Spawning Spring Mode 1

    ![](../images/CNAPS_constant/mixedlayer_constant_Spring_Sp_AllM_mode1_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Spring_Sp_AllM_temporal_mode1_CNAPS.png)

    ### Mixed Layer Spawning Spring Mode 2

    ![](../images/CNAPS_constant/mixedlayer_constant_Spring_Sp_AllM_mode2_CNAPS.png)
    ![](../images/CNAPS_constant/mixedlayer_constant_Spring_Sp_AllM_temporal_mode2_CNAPS.png)

    ::: 
