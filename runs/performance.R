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

load("data/om.RData")

# LOAD tuning results

load("out/bd4010tune.RData")
load("out/cpuetune.RData")

load("out/bd4010btune.RData")
load("out/cpuebtune.RData")

load("out/bd4010ctune.RData")
load("out/cpuectune.RData")


data(iotcindicators)

# Tuned MPs

tuns <- list(bdta1=bdta1, bdta2=bdta2, bdta3=bdta3, bdta4=bdta4,
  cpta1=cpta1, cpta2=cpta2, cpta3=cpta3, cpta4=cpta4)

tunsb <- list(bdta1b=bdta1b, bdta2b=bdta2b, bdta3b=bdta3b, bdta4b=bdta4b,
             cpta1b=cpta1b, cpta2b=cpta2b, cpta3b=cpta3b, cpta4b=cpta4b)

tunsc <- list(bds1=bdta2c, bds2=bdta3c, bds3=bdta4c,
              cps1=cpta2c, cps2=cpta3c, cps3=cpta4c)

pyears <- seq(2016, 2035)

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

perfb <- rbindlist(lapply(tunsb,
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



perfkobeb <- rbindlist(lapply(tunsb,
  function(x) {
    performance(stock(x), refpts=refpts, indicators=kobeindicators, years=pyears,
      metrics=mets)
  }), idcol="mp")

perfkobeb <- perfkobeb[, .(data=sum(data) / length(data)), by=.(mp, year, indicator, name)]
perfkobeb[, indicator:=factor(indicator, levels=c("green", "orange", "yellow", "red"))]

# PERFORMANCE ts for long table

perfts <- rbindlist(lapply(tuns,
  function(x) {
    performance(stock(x), refpts=refpts, indicators=indicators,
      years=mapply(seq, from=min(pyears)+1, length.out=c(5,10,20)),
      metrics=mets)
  }), idcol="mp")

perftsb <- rbindlist(lapply(tunsb,
  function(x) {
    performance(stock(x), refpts=refpts, indicators=indicators,
       years=mapply(seq, from=min(pyears)+1, length.out=c(5,10,20)),
       metrics=mets)
  }), idcol="mp")


# EXTRACT metrics from tuns and om

mets <- c(mets, SBMSY = function(x) unitSums(ssb(x)) / refpts$SBMSY,
  FMSY = function(x) unitMeans(fbar(x)) / refpts$FMSY)

tuns <- lapply(tuns, function(x) metrics(stock(x), metrics=mets))

tunsb <- lapply(tunsb, function(x) metrics(stock(x), metrics=mets))

omm <- metrics(window(stock(om), end=2017), metrics=mets) 


save(omm, tuns, perf, perfkobe, perfts, tunsb, perfb, perfkobeb, perftsb, file="out/perf_tune.RData", compress="xz")



# PERFORMANCE across tuned runs 2030:2034

perfc <- rbindlist(lapply(tunsc,
                          function(x) {
                            performance(stock(x), refpts=refpts, indicators=indicators, years=list(pyears),
                                        metrics=mets)
                          }), idcol="mp")

# KOBE performance time series
perfkobec <- rbindlist(lapply(tunsc,
                              function(x) {
                                performance(stock(x), refpts=refpts, indicators=kobeindicators, years=pyears,
                                            metrics=mets)
                              }), idcol="mp")

perfkobec <- perfkobec[, .(data=sum(data) / length(data)), by=.(mp, year, indicator, name)]
perfkobec[, indicator:=factor(indicator, levels=c("green", "orange", "yellow", "red"))]

# PERFORMANCE ts for long table

perftsc <- rbindlist(lapply(tunsc,
                            function(x) {
                              performance(stock(x), refpts=refpts, indicators=indicators,
                                          years=mapply(seq, from=min(pyears), length.out=c(5,10,20)),
                                          metrics=mets)
                            }), idcol="mp")


# EXTRACT metrics from tuns and om

mets <- c(mets, SBMSY = function(x) unitSums(ssb(x)) / refpts$SBMSY,
          FMSY = function(x) unitMeans(fbar(x)) / refpts$FMSY)

tunsc <- lapply(tunsc, function(x) metrics(stock(x), metrics=mets))

omm <- metrics(window(stock(om), end=2016), metrics=mets) 


save(omm, tunsc, perfc, perfkobec, perftsc, file="out/perf_tunec.RData", compress="xz")
