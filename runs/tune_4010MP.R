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
  iy=years[1], ny=length(years), nsqy=3, vy=ac(years), management_lag=3) #

# INDICATORS

indicators <- list(
  # P(SB >= SB_MSY)
  S8 = list(~iterProb(yearMeans(SB / SBMSY) >= 1),
    name = "P(SB >= SB[MSY])",  desc = "Probability of SB greater/equal than SBMSY"),
  # P(Green)
  S6 = list(~yearSums(FLQuant((SB / SBMSY) > 1 & (F / FMSY) < 1)) / dim(SB)[2],
    name = "P(Green)", desc = "Probability of being in Kobe green quadrant"))

# CONTROL

control <- mpCtrl(list(ctrl.hcr = mseCtrl(method=catchSSB.hcr,
    args=list(dtarget=0.40, dlimit=0.20, lambda=1, MSY=refpts(om)$MSY, ssb_lag=1)),
  ctrl.est = mseCtrl(method=perfect.sa),
  ctrl.is=mseCtrl(method=deltac.is, args=list(dtaclow=0.85, dtacupp=1.15))))

# TEST run

run <- mp(om=om, oem=oem, ctrl.mp=control, genArgs=mpargs)


# TUNE P(SB >= SB_MSY) = 0.5 on catchSSB.hcr@lambda []

bdta1 <- tunebisect(om, oem=oem, control, mpargs=mpargs,
  metrics=list(SB=function(x) ssb(x)[,,'F']), indicator=indicators["S8"],
  pyears=list(pyears), tune=list(lambda=c(0.25, 1.50)), prob=0.5, tol=0.01, maxit=12)

bdta1b <- tunebisect(om, oem=oem, control, mpargs=mpargs,
  metrics=list(SB=function(x) ssb(x)[,,'F']), indicator=indicators["S8"],
  pyears=list(2026:2036), tune=list(lambda=c(0.25, 1.50)), prob=0.5, tol=0.01, maxit=12)

bdta1c <- tunebisect(om, oem=oem, control, mpargs=mpargs,
                     metrics=list(SB=function(x) ssb(x)[,,'F']), indicator=indicators["S8"],
                     pyears=list(2030:2034), tune=list(lambda=c(0.25, 1.50)), prob=0.5, tol=0.01, maxit=12)


# TUNE P(Green) = 0.5

bdta2 <- tunebisect(om, oem=oem, control, mpargs=mpargs,
  metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
  indicator=indicators["S6"], pyears=list(pyears),
  tune=list(lambda=c(0.25, 1.50)), prob=0.5, tol=0.01, maxit=12)

bdta2b <- tunebisect(om, oem=oem, control, mpargs=mpargs,
  metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
  indicator=indicators["S6"], pyears=list(2026:2036),
  tune=list(lambda=c(0.25, 1.50)), prob=0.5, tol=0.01, maxit=12)

bdta2c <- tunebisect(om, oem=oem, control, mpargs=mpargs,
                     metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
                     indicator=indicators["S6"], pyears=list(2030:2034),
                     tune=list(lambda=c(0.25, 1.50)), prob=0.5, tol=0.01, maxit=12)

# TUNE P(Green) = 0.6

bdta3 <- tunebisect(om, oem=oem, control, mpargs=mpargs,
  metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
  indicator=indicators["S6"], pyears=list(pyears),
  tune=list(lambda=c(0.25, 1.50)), prob=0.6, tol=0.01, maxit=12)

bdta3b <- tunebisect(om, oem=oem, control, mpargs=mpargs,
  metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
  indicator=indicators["S6"], pyears=list(2026:2036),
  tune=list(lambda=c(0.25, 1.50)), prob=0.6, tol=0.01, maxit=12)

bdta3c <- tunebisect(om, oem=oem, control, mpargs=mpargs,
                     metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
                     indicator=indicators["S6"], pyears=list(2030:2034),
                     tune=list(lambda=c(0.25, 1.50)), prob=0.6, tol=0.01, maxit=12)


# TUNE P(Green) = 0.7

bdta4 <- tunebisect(om, oem=oem, control, mpargs=mpargs,
  metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
  indicator=indicators["S6"], pyears=list(pyears),
  tune=list(lambda=c(0.25, 1.50)), prob=0.7, tol=0.01, maxit=12)

bdta4b <- tunebisect(om, oem=oem, control, mpargs=mpargs,
  metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
  indicator=indicators["S6"], pyears=list(2026:2036),
  tune=list(lambda=c(0.25, 1.50)), prob=0.7, tol=0.01, maxit=12)

bdta4c <- tunebisect(om, oem=oem, control, mpargs=mpargs,
                    metrics=list(SB=function(x) ssb(x)[,,'F'], F=function(x) unitMeans(fbar(x))),
                    indicator=indicators["S6"], pyears=list(2030:2034),
                    tune=list(lambda=c(0.25, 1.50)), prob=0.7, tol=0.01, maxit=12)


save(bdta1, bdta2, bdta3, bdta4, file="out/bd4010tune.RData", compress="xz")
save(bdta1b, bdta2b, bdta3b, bdta4b, file="out/bd4010btune.RData", compress="xz")

