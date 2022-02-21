# lorenzen.R - DESC
# /lorenzen.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

VB <- function(Linf, k, t0, ageMax){
  return(1.066 * (Linf*(1-exp(-k*(0:ageMax-t0)))) + 10.449)
}


naturalMortality<- function(Linf, k, t0, ageMax){
  L = VB(Linf=Linf, k=k, t0=t0, ageMax = ageMax)
  W = (3.815e-006 * L ^ 3.188)*1000
  M = 3.0 * W^(-0.288)
  return(data.frame(Age = 0:ageMax, Size = L, Weight=W/1000, M = M))
  }

plotLorenzen<- function(data, title){
  plot(data$M~data$Age, type="o", main=title)
  }


optima <- function(data, firstage){
  functionM <- function(a){
  (exp(mean(log(a*data$M[4:30])))-M)^2
  }
  optim(2, functionM, method="BFGS")$par
  }


### From Then et al. (2014)
M = 4.899*30^(-0.916)

par(mfrow=c(2,2))
## For female ray
b <- naturalMortality(279.6, 0.123, -2.55, 30)
b$M <- b$M * optima(b, 4)
b
plotLorenzen(b, title="Female ray")
## For male ray
b <- naturalMortality(213.8, 0.197, -2.28, 30)
b$M <- b$M * optima(b, 4)
plotLorenzen(b, title="Male ray")
## For female otolith
b <- naturalMortality(249.6, 0.157, -2.13, 30)
b$M <- b$M * optima(b, 4)
b
plotLorenzen(b, title="Female otolith")
## For male otolith
b <- naturalMortality(191.9, 0.235, -2.10, 30)
b$M <- b$M * optima(b, 4)
b
plotLorenzen(b, title="Male otolith")


# ---

library(r4ss)
# Create a temporary folder for the output and set the working directory:
temp_path <- file.path(tempdir(), "ss3sim-tv-example")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

d <- system.file("extdata", package = "ss3sim")
ctl_file <- paste0(d, "/models/cod-om/codOM.ctl")
data.old <- r4ss::SS_readdat(file.path(d, "models", "cod-om", "codOM.dat"))
change_e(ctl_file_in = ctl_file, ctl_file_out = "change_e.ctl",
         dat_list = data.old, for_file_in = "forecast.ss",
         natM_type = "n_breakpoints", natM_n_breakpoints = c(1, 4),
         natM_lorenzen = NULL, natM_val = c(.2, 3, 0.4, 5),
         par_name = c("_steep", "SizeSel_1P_1_Fishery"),
         par_int = c(0.3, 40), par_phase = c(3, 2),
         forecast_num = 0, run_change_e_full = TRUE )
# clean up
file.remove("change_e.ctl")
setwd(wd)

