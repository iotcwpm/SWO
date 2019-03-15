# performance.R - DESC
# /performance.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


library(mse)
library(mseviz)
library(ioswomse)

# LOAD om

load("data/omsmall.RData")

# LOAD tuning results

load("out/bd4010tune.RData")
load("out/cpuetune.RData")

data(iotcindicators)

# Tuned MPs

tuns <- list(bdta1=bdta1, bdta2=bdta2, bdta3=bdta3, bdta4=bdta4,
  cpta1=cpta1, cpta2=cpta2, cpta3=cpta3, cpta4=cpta4)

pyears <- seq(2016, 2036)

# ADD derived refpts (Ftarget, SBlim)

refpts <- rbind(refpts(om),
  FLPar(Ftarget=refpts(om)$FMSY,
  SBlim=refpts(om)$SBMSY*0.40))

# METRICS

mets <- list(SB=function(x) unitSums(ssb(x)), C=function(x) unitSums(catch(x)),
  F=function(x) unitMeans(fbar(x)))

# PERFORMANCE across tuned runs

perf <- rbindlist(lapply(tuns,
  function(x) {
    performance(stock(x), refpts=refpts, indicators=indicators, years=list(pyears),
      metrics=mets)
  }), idcol="mp")


# KOBE performance time series

perfkobe <- rbindlist(lapply(tuns,
  function(x) {
    performance(stock(x), refpts=refpts, indicators=kobeindicators, years=pyears,
      metrics=mets)
  }), idcol="mp")

perfkobe <- perfkobe[, .(data=sum(data) / length(data)), by=.(mp, year, indicator, name)]
perfkobe[, indicator:=factor(indicator, levels=c("green", "orange", "yellow", "red"))]

# PERFORMANCE ts for long table

perfts <- rbindlist(lapply(tuns,
  function(x) {
    performance(stock(x), refpts=refpts, indicators=indicators,
      years=mapply(seq, from=2019, length.out=c(5,10,20)),
      metrics=mets)
  }), idcol="mp")

# EXTRACT metrics from tuns and om

mets <- c(mets, SBMSY = function(x) unitSums(ssb(x)) / refpts$SBMSY,
  FMSY = function(x) unitMeans(fbar(x)) / refpts$FMSY)

tuns <- lapply(tuns, function(x) metrics(stock(x), metrics=mets))

omm <- metrics(window(stock(om), end=2017), metrics=mets) 


save(omm, tuns, perf, perfkobe, perfts, file="out/perf_tune.RData", compress="xz")
