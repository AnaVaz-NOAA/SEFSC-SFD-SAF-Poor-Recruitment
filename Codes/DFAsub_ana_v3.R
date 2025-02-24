graphics.off()
rm(list=ls(all=TRUE))

library(MARSS)

## Load the recruitment and environmental data
recruit.dat <- read.csv("./csv_files/CovarSeason_RecDev.csv", header=TRUE)
head(recruit.dat)
dim(recruit.dat)
str(recruit.dat)

## Extract the years and convert to factor
years <- recruit.dat[, 1]
recruit.dat$year <- as.factor(recruit.dat[, 1])

## Extract recruitment and environmental covariates
recruitment <- recruit.dat[, 42:51]  # Recruitment data (last 10 columns)
environment <- recruit.dat[, c(2:21,34:41)]   # Environmental covariates (first 40 columns)
species_names <- colnames(recruitment)

get_DFA_fits <- function(MLEobj, dd = NULL, alpha = 0.05) {
  ## empty list for results
  fits <- list()
  ## extra stuff for var() calcs
  Ey <- MARSS:::MARSShatyt(MLEobj)
  ## model params
  ZZ <- coef(MLEobj, type = "matrix")$Z
  ## number of obs ts
  nn <- dim(Ey$ytT)[1]
  ## number of time steps
  TT <- dim(Ey$ytT)[2]
  ## get the inverse of the rotation matrix
  H_inv <- varimax(ZZ)$rotmat
  ## check for covars
  if (!is.null(dd)) {
    DD <- coef(MLEobj, type = "matrix")$D
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states + DD %*% dd
  } else {
    ## model expectation
    fits$ex <- ZZ %*% H_inv %*% MLEobj$states
  }
  ## Var in model fits
  VtT <- MARSSkfss(MLEobj)$VtT
  VV <- NULL
  for (tt in 1:TT) {
    RZVZ <- coef(MLEobj, type = "matrix")$R - ZZ %*% VtT[, 
                                                         , tt] %*% t(ZZ)
    SS <- Ey$yxtT[, , tt] - Ey$ytT[, tt, drop = FALSE] %*% 
      t(MLEobj$states[, tt, drop = FALSE])
    VV <- cbind(VV, diag(RZVZ + SS %*% t(ZZ) + ZZ %*% t(SS)))
  }
  SE <- sqrt(VV)
  ## upper & lower (1-alpha)% CI
  fits$up <- qnorm(1 - alpha/2) * SE + fits$ex
  fits$lo <- qnorm(alpha/2) * SE + fits$ex
  return(fits)
}

model_results <- list()

## loop by species
for (iSp in 1:dim(recruitment)[2]) {
  ## check what years are complete
  data.dat <- cbind(recruitment[iSp], environment)
  valid_rows <- complete.cases(data.dat)    
  dat.t <- t(data.dat[valid_rows, , drop = FALSE])
  
  ## Define number of time series (N.ts) and years (TT)
  N.ts <- dim(dat.t)[1]  # Number of recruitment time series (species)
  TT <- dim(dat.t)[2]    # Number of time steps (years)
  
  ## Standardize the recruitment data (z-score)
  Sigma <- sqrt(apply(dat.t, 1, var, na.rm=TRUE))
  y.bar = apply(dat.t, 1, mean, na.rm=TRUE)
  dat = (dat.t - y.bar) * (1/Sigma)
  
  rownames(dat) <- rownames(dat.t)
  
  ## Define the loading matrix Z for the model (2 states)
  Z.vals <- paste("z", rep(1:29, each = 2), rep(1:2, times = 29), sep = "")
  Z.vals
  
  Z = matrix(Z.vals, nrow=N.ts, ncol=2, byrow=TRUE)
  head(Z)
  dim(Z)
  
  ## Define the offset factor "a" (no intercept in the process)
  a <- "zero"
  
  ## The process model for dynamic factors (2 states)
  m <- 2  # Number of states
  
  B <- "identity"  # Identity matrix for the process model
  u <- "zero"      # No external forcing
  Q <- "identity"  # Identity matrix for the process noise
  
  ## Residual covariance matrix (R) ??? 
  R <- diag(N.ts)  # Identity matrix for residual covariance
  
  ## List with model specifications
  mod_list <- list(Z = Z, A = a, R = R, B = B, U = u, Q = Q)
  
  ## List with model initial values
  init_list <- list(x0 = matrix(rep(0, m), m, 1))
  
  ## List with model control parameters
  con_list <- list(maxit = 5000, allow.degen=TRUE, trace=-1)
  
  ## fit MARSS
  dfa_1 <- MARSS(y = dat, model = mod_list, inits = init_list, 
                 control = con_list)
  
  aic_value <- AIC(dfa_1)
  
  # Save the model results in a list
  model_results[[species_names[iSp]]] <- list(
    model = dfa_1,
    aic = aic_value
  )
  print(paste("Species:", species_names[iSp], "AIC:", aic_value))
  
  #-------------------------------------------------
  ##extract results
  ##number of processes
  mm <- 2               
  
  ## get the estimated ZZ
  Z_est <- coef(dfa_1, type = "matrix")$Z
  ## get the inverse of the rotation matrix
  H_inv <- varimax(Z_est)$rotmat
  
  ## rotate factor loadings
  Z_rot = Z_est %*% H_inv
  ## rotate processes
  proc_rot = solve(H_inv) %*% dfa_1$states
  
  ylbl <- rownames(dat)
  w_ts <- seq(dim(dat)[2])
  
  get_DFA_fits(dfa_1)
  
  ## get model fits & CI's
  mod_fit <- get_DFA_fits(dfa_1)
  
  clr <- rainbow(8, start=0.4, end=0.9)
  
  loadings <- coef(dfa_1, type = "matrix")$Z
  print(loadings)
  
  plotName <- paste("species_load_states_", species_names[iSp] , ".png", sep = "")
  
  png(filename = plotName, width = 800, height = 800) 
  #nf <- layout(matrix(c(1:(mm*2)), mm, 2), widths = c(2, 1.5))
  #layout.show(nf)
  #par(mai = c(0.5, 0.5, 0.5, 0.1), omi = c(0, 0, 0, 0))
  #par(mai = c(0.5, 0.5, 0.5, 0.1), omi = c(0, 0, 0, 0), mfrow = c(mm, 1), xpd = F) 
  nf <- layout(matrix(c(1, 3, 2, 4), 2, 2, byrow = TRUE))  
  layout.show(nf) 
  par(mar = c(4, 4, 2, 1))  
  ## plot the processes
  for (i in 1:mm) {
    ylm <- c(-1, 1) * max(abs(proc_rot[i, ]))
    ## set up plot area
    plot(w_ts, proc_rot[i, ], type = "n", bty = "L", ylim = ylm, 
         xlab = "", ylab = "", xaxt = "n")
    ## draw zero-line
    abline(h = 0, col = "gray")
    ## plot trend line
    lines(w_ts, proc_rot[i, ], lwd = 2)
    lines(w_ts, proc_rot[i, ], lwd = 2)
    ## add panel labels
    mtext(paste("State", i), side = 3, line = 0.5)
    axis(1, at=w_ts, lab=years[valid_rows])
  }
  
  ## plot the loadings
  minZ <- 0
  ylm <- c(-1, 1) * max(abs(Z_rot))
  par(mar=c(7, 2, 2, 0.5))
  for (i in 1:mm) {
    plot(c(1:N.ts)[abs(Z_rot[, i]) > minZ], as.vector(Z_rot[abs(Z_rot[, i]) > minZ, i]), type = "h", lwd = 2, 
         xlab = "", ylab = "", xaxt = "n", ylim=ylm, xlim = c(0.5, N.ts + 0.5), col = clr)
    axis(1, at=1:N.ts, lab=ylbl, las=2,)
    abline(h = 0, lwd = 1.5, col = "gray")
    mtext(paste("Factor loadings on state", i), side = 3, line = 0.5) 
  }
  dev.off()
  
  ## plot the fits
  quartz(width=8, height=10)  #KC added
  par(mfrow = c(4,2), mai = c(0.5, 0.7, 0.1, 0.1), omi = c(0, 0, 0, 0))  #KC
  for (i in 1:N.ts) {
    up <- mod_fit$up[i, ]
    mn <- mod_fit$ex[i, ]
    lo <- mod_fit$lo[i, ]
    
    plotName <- paste("species_fit_",species_names[iSp] , i, ".png", sep = "")
    png(filename = plotName, width = 800, height = 600) 
    
    plot(w_ts, mn, xlab = "", ylab = ylbl[i], xaxt = "n", type = "n", 
         cex.lab = 1.2, ylim = c(min(lo), max(up)))
    axis(1, at=w_ts, years[valid_rows])
    points(w_ts, dat[i, ], pch = 16, col = clr[i])
    lines(w_ts, up, col = "darkgray")
    lines(w_ts, mn, col = "black", lwd = 2)
    lines(w_ts, lo, col = "darkgray")
    
    dev.off()
  }
}

write.csv(aic_table, "AIC_values.csv", row.names = FALSE)
