# tune_cpueMP.R - DESC
# /tune_cpueMP.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# LOAD packages
library(mse)

# LOAD data
load("data/omp.RData")

# ARGS
years <- 2015:2036
pyears <- 2016:2036

mpargs <- list(it=dims(stock(om))$iter, fy=years[length(years)], y0=1950, dy=years[1],
  iy=years[1], ny=length(years), nsqy=3, vy=ac(years), management_lag=3)

# INDICATORS
indicators <- list(
  # P(SB >= SB_MSY)
  S8 = list(~iterProb(yearMeans(SB / SBMSY) >= 1),
    name = "P(SB >= SB[MSY])",  desc = "Probability of SB greater/equal than SBMSY"),
  # P(Green)
  S6 = list(~yearSums(FLQuant((SB / SBMSY) > 1 & (F / FMSY) < 1)) / dim(SB)[2],
    name = "P(Green)", desc = "Probability of being in Kobe green quadrant"))

# CONTROL
control <- mpCtrl(list(
  # HCR
  ctrl.hcr = mseCtrl(method=cpue.hcr,
    # ARGS
    args=list(k1=0.2, k2=0.2, k3=0.2, k4=0.2, target=1,
      dtaclow=0.85, dtacupp=1.15)),
  # IND
  ctrl.est = mseCtrl(method=cpue.ind, args=list(nyears=5))))

# TEST

run <- mp(om=om, oem=oem, ctrl.mp=control, genArgs=mpargs,
   tracking=c("cpue.mean", "cpue.slope"))

# TUNE
range <- round(c(0.2, 3) * mean(index(cpue)[, ac(1994:2000)]), digits=2)


# TUNE P(SB >= SB_MSY) = 0.5

cpta1 <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S8"], metrics=list(SB=function(x) ssb(x)[,,'F']),
  tol=0.01, prob=0.5, pyears=list(pyears))

cpta1b <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S8"], metrics=list(SB=function(x) ssb(x)[,,'F']),
  tol=0.01, prob=0.5, pyears=list(2026:2036))

cpta1c <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
                     tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
                     indicator=indicators["S8"], metrics=list(SB=function(x) ssb(x)[,,'F']),
                     tol=0.01, prob=0.5, pyears=list(2030:2034))

# TUNE P(Green) = 0.5

cpta2 <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S6"],
  metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
  tol=0.01, prob=0.5, pyears=list(pyears))

cpta2b <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S6"],
  metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
  tol=0.01, prob=0.5, pyears=list(2026:2036))

cpta2c <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
                     tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
                     indicator=indicators["S6"],
                     metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
                     tol=0.01, prob=0.5, pyears=list(2030:2034))

# TUNE P(Green) = 0.6

cpta3 <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S6"],
  metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
  tol=0.01, prob=0.6, pyears=list(pyears))

cpta3b <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S6"],
  metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
  tol=0.01, prob=0.6, pyears=list(2026:2036))

cpta3c <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
                     tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
                     indicator=indicators["S6"],
                     metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
                     tol=0.01, prob=0.6, pyears=list(2030:2034))

# TUNE P(Green) = 0.7

cpta4 <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S6"],
  metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
  tol=0.01, prob=0.7, pyears=list(pyears))

cpta4b <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S6"],
  metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
  tol=0.01, prob=0.7, pyears=list(2026:2036))

cpta4c <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
                     tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
                     indicator=indicators["S6"],
                     metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
                     tol=0.01, prob=0.7, pyears=list(2030:2034))

save(cpta1, cpta2, cpta3, cpta4, file="out/cpuetune.RData", compress="xz")
save(cpta1b, cpta2b, cpta3b, cpta4b, file="out/cpuebtune.RData", compress="xz")
save(cpta2c, cpta3c, cpta4c, file="runs/out/cpuectune.RData", compress="xz")
