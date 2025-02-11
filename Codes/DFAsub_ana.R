#Code for Dynamic Factor Analysis (DFA) JKC August 2023
# Changed to add env. covariates ACV November 2024

graphics.off()
rm(list=ls(all=TRUE))

library(MARSS)

##recruit data

recruit.dat <- read.csv("./csv_files/CovarSeason_RecDev.csv", header=TRUE)
head(recruit.dat)
dim(recruit.dat)
str(recruit.dat)

##convert year to a factor
recruit.dat$year <- as.factor(recruit.dat$X)
str(recruit.dat)

##making a separate file to use to label years on the graph
recruit.dat1 <- read.csv("./csv_files/CovarSeason_RecDev.csv", header=TRUE)
head(recruit.dat1)
dim(recruit.dat1)
str(recruit.dat1)

##convert year to a factor
recruit.dat1$year <- as.factor(recruit.dat1$X)
str(recruit.dat1)

##Remove year from data
##When the data are transposed, it makes the structure weird
recruit.dat <- recruit.dat[,2:9]    #hardwired for the number of columns in the input file

##transpose the data so the columns are year
dat.t <- t(recruit.dat)
head(dat.t)
dim(dat.t)
str(dat.t)

##Define number of time series
N.ts <- dim(dat.t)[1]

##Define number of years
TT <- dim(dat.t)[2]

##De-mean the data
#y.bar <- apply(dat.t, 1, mean, na.rm=TRUE)
#dat <- dat.t - y.bar

##calculate the z-score
Sigma <- sqrt(apply(dat.t, 1, var, na.rm=TRUE))
y.bar = apply(dat.t, 1, mean, na.rm=TRUE)
dat = (dat.t - y.bar) * (1/Sigma)

rownames(dat) <- rownames(dat.t)

##The observation model

##Set up the loading matrix Z
##Fitting for 2 states
##Dim = 8 * 2

Z.vals = list(
  "z11", 0, 
  "z21", "z22", 
  "z31", "z32",
  "z41", "z42", 
  "z51", "z52", 
  "z61", "z62",
  "z71", "z72",
  "z81", "z82")
 
##Set up the loading matrix Z
##Fitting for 3 states
##Dim = 8 * 3

#Z.vals = list(
#  "z11", "z12", 0, 
#  "z21", "z22", "z23", 
#  "z31", "z32", "z33",
#  "z41", "z42", "z43",
#  "z51", "z52", "z53",
#  "z61", "z62", "z63",
#  "z71", "z72", "z73",
#  "z81", "z82", "z83")  

Z = matrix(Z.vals, nrow=N.ts, ncol=2, byrow=TRUE)
head(Z)
dim(Z)

##Define the offset factor "a"

a <- "zero"

##The process model

## number of processes
m <- 2                #KC change from 2 to 3 for 3 states?

## 'B' is identity: 1's along the diagonal & 0's elsewhere
B <- "identity"  # diag(m)

## 'u' is a column vector of 0's
u <- "zero"  # matrix(0,m,1)

## 'Q' is identity
Q <- "identity"  # diag(m)

##Fitting the model in MARSS

## list with specifications for model vectors/matrices
mod_list <- list(Z = Z, A = a, R = R, B = B, 
                 U = u, Q = Q)

## list with model inits
init_list <- list(x0 = matrix(rep(0, m), m, 1))

## list with model control parameters
con_list <- list(maxit = 5000, allow.degen=TRUE, trace=-1)

## fit MARSS
dfa_1 <- MARSS(y = dat, model = mod_list, inits = init_list, 
               control = con_list)

##extract results

##number of processes
mm <- 2               

mm
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
layout(matrix(c(1:(mm*2)), mm, 2), widths = c(2, 1.5))
## par(mfcol=c(mm,2), mai=c(0.5,0.5,0.5,0.1), omi=c(0,0,0,0))
par(mai = c(0.5, 0.5, 0.5, 0.1), omi = c(0, 0, 0, 0))

clr <- rainbow(8, start=0.4, end=0.9)

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
  axis(1, at=w_ts, lab=recruit.dat1$year)
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

get_DFA_fits(dfa_1)

## get model fits & CI's
mod_fit <- get_DFA_fits(dfa_1)
## plot the fits
windows(width=8, height=10, record=TRUE)  #KC added
par(mfrow = c(4,2), mai = c(0.5, 0.7, 0.1, 0.1), omi = c(0, 0, 0, 0))  #KC
for (i in 1:N.ts) {
  up <- mod_fit$up[i, ]
  mn <- mod_fit$ex[i, ]
  lo <- mod_fit$lo[i, ]
  plot(w_ts, mn, xlab = "", ylab = ylbl[i], xaxt = "n", type = "n", 
       cex.lab = 1.2, ylim = c(min(lo), max(up)))
  axis(1, at=w_ts, recruit.dat1$year)
  points(w_ts, dat[i, ], pch = 16, col = clr[i])
  lines(w_ts, up, col = "darkgray")
  lines(w_ts, mn, col = "black", lwd = 2)
  lines(w_ts, lo, col = "darkgray")
}
