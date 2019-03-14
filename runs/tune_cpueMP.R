# tune_cpueMP.R - DESC
# /tune_cpueMP.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# LOAD packages
library(mse)

# LOAD data
load("data/omsmallp.RData")

# ARGS
years <- 2015:2036
pyears <- 2016:2036

mpargs <- list(it=dims(stock(om))$iter, fy=years[length(years)], y0=1950, dy=years[1],
  iy=years[1], ny=length(years), nsqy=3, vy=ac(years))

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
range <- round(c(0.2, 2) * mean(index(cpue)[, ac(1994:2000)]), digits=2)


# TUNE P(SB >= SB_MSY) = 0.5

cpta1 <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S8"], metrics=list(SB=function(x) ssb(x)[,,'F']),
  tol=0.01, prob=0.5, pyears=list(pyears))

cpta1b <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S8"], metrics=list(SB=function(x) ssb(x)[,,'F']),
  tol=0.01, prob=0.5, pyears=list(2026:2036))

# TUNE P(Green) = 0.5

cpta2 <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S6"], metrics=list(SB=function(x) ssb(x)[,,'F']),
  tol=0.01, prob=0.5, pyears=list(pyears))

# TUNE P(Green) = 0.6

cpta3 <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S6"], metrics=list(SB=function(x) ssb(x)[,,'F']),
  tol=0.01, prob=0.6, pyears=list(pyears))

# TUNE P(Green) = 0.7

cpta4 <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S6"], metrics=list(SB=function(x) ssb(x)[,,'F']),
  tol=0.01, prob=0.7, pyears=list(pyears))


save(cpta1, cpta1b, cpta2, cpta3, cpta4, file="out/cpuetune.RData")
