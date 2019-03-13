# setup.R - DESC
# /setup.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


library(mse)

load("data/omsmall.RData")
load("data/metrics.RData")

set.seed(2981)

# SR DEVIANCES

# SD(residuals_SR)
resd <- c(sqrt(yearVars(sr(om)@residuals)))

# AC(residuals_SR)
rerho <- c(apply(sr(om)@residuals[,ac(1950:2013)], c(1, 3:6),
  function(x) acf(x, plot=FALSE)$acf[2]))

sres <- FLQuant(0, dimnames=list(age='all', year=2016:2047,
    iter=dimnames(stock(om))$iter), units="")

for(i in seq(dim(sres)[6]))
  sres[,,,,,i] <- rlnoise(1, sres[,,,,,i], sd=resd[i], b=rerho[i])

residuals(sr(om)) <- expand(sres, unit=c('F', 'M'))

# SET LL_NE index
cpue <- cpues[["LL_NE"]]
range(cpue, c("startf", "endf")) <- c(0,1)

# EXTEND cpue
cpue <- window(cpue,  end=2047)

# sel.pattern
sel.pattern(cpue)[, ac(2016:2047)] <- sel.pattern(cpue)[,'2015']

# REGENERATE q
vbiom <- quantSums(stock.n(stock(om))[,ac(1994:2015)] *
  exp(-harvest(stock(om))[,ac(1994:2015)] * 0.5 - m(stock(om))[,ac(1994:2015)] * 0.5) *
  stock.wt(stock(om))[,ac(1994:2015)] *
  sel.pattern(cpue)[,ac(1994:2015)])

index.q(cpue)[, ac(1994:2015)] <- yearMeans(index(cpue)[, ac(1994:2015)] /
    unitSums(vbiom [, ac(1994:2015)]))

# EXTEND to 2047
index.q(cpue)[, ac(2016:2047)] <- index.q(cpue)[,'2015']

idx <- (results[results$sample == TRUE, "llq"] == "1.01")[1:50]

index.q(cpue)[, ac(2015:2047),,,,idx]  <- index.q(cpue)[, ac(2015:2047),,,,idx] *
  FLQuant(1.01 ^ seq(0, length(2016:2047)))


# index.var
index.var(cpue)[, ac(2016:2047)] <- index.var(cpue)[,'2014']

# SET FUTURE qs
loq <- log(index(cpue)[,ac(1994:2015)] / unitSums(vbiom))
qmu <- yearMeans(loq)
qsig <- sqrt(yearVars(loq))

deviances.q <- rlnorm(1, expand(qmu, year=2016:2047), expand(qsig, year=2016:2047))


# OEM
oem <- FLoem(method=cpue.oem,
  observations=list(stk=stock(om), idx=FLIndices(LL_NE=cpue)),
  deviances=list(idx=deviances.q))


