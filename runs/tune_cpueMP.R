# tune_cpueMP.R - DESC
# /tune_cpueMP.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# LOAD packages
library(mse)

# LOAD data
load("data/omsmall.RData")

# ARGS
years <- 2015:2047
pyears <- 2016:2047
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


oem <- FLoem(method=cpue.oem,
  observations=list(stk=stock(om), idx=FLIndices(LL_NE=cpue)),
  deviances=list(idx=deviances.q))

# TEST

run <- mp(om=om, oem=oem, ctrl.mp=control, genArgs=mpargs,
   tracking=c("cpue.mean", "cpue.slope"))

# TUNE
range <- round(c(0.01, 5) * mean(index(cpue)[, ac(1994:2000)]), digits=2)

cpta1 <- tunebisect(om=om, oem=oem, control=control, mpargs=mpargs,
  tracking=c("cpue.mean", "cpue.slope"), tune=list(target=range),
  indicator=indicators["S8"], metrics=list(SB=function(x) ssb(x)[,,'F']),
  tol=0.01, prob=0.5, pyears=list(pyears))


