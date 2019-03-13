# setup.R - DESC
# /setup.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# XX {{{
# }}}

library(mse)

load("om/out/omsmall.RData")
load("om/out/metrics_sub.RData")

# SR DEVIANCES

resd <- c(sqrt(yearVars(sr(om)@residuals)))
rerho <- c(apply(sr(om)@residuals[,ac(1950:2013)], c(1,3:6), function(x) acf(x, plot=FALSE)$acf[2]))

sres <- FLQuant(0, dimnames=dimnames(window(stock(om), start=2014)))
for(i in seq(dim(sres)[6]))
  sres[,,,,,i] <- rlnoise(1, sres[,,,,,i], sd=resd[1], b=rerho[i])
sr(om)@.Data[[1]] <- sres

# INDEX

cpue <- cpues[["LL_NE"]]

# EXTEND cpue

cpue <- window(cpue,  end=2047)

# sel.pattern
sel.pattern(cpue)[, ac(2016:2047)] <- sel.pattern(cpue)[,'2015']

# REGENERATE q
vbiom <- quantSums(stock.n(stock(om))[,ac(1994:2015)] *
  exp(-harvest(stock(om))[,ac(1994:2015)] * 0.5 - m(stock(om))[,ac(1994:2015)] * 0.5) *
  stock.wt(stock(om))[,ac(1994:2015)] *
  sel.pattern(cpue)[,ac(1994:2015)])

index.q(cpue)[, ac(1994:2015)] <- yearMeans(index(cpue)[, ac(1994:2015)] / unitSums(vbiom [, ac(1994:2015)]))

# EXTEND to 2047
index.q(cpue)[, ac(2016:2047)] <- index.q(cpue)[,'2015']

# RECREATE index 2015-2017
# index(cpue)[, ac(2015:2017)] <- index.q(cpue)[, ac(2015:2017)] * vbiom[, ac(2015:2017)]

# index.var
index.var(cpue)[, ac(2016:2047)] <- index.var(cpue)[,'2014']

# SET FUTURE qs

loq <- log(index(cpue)[,ac(1994:2015)] / unitSums(vbiom))
qmu <- yearMeans(loq)
qsig <- sqrt(yearVars(loq))

# FUTURE qs
set.seed(2981)

deviances.q <- rlnorm(1, expand(qmu, year=2016:2047), expand(qsig, year=2016:2047))


# llq = 1.0025
idx<- results[sample==TRUE,]
idx <- idx[1:50,]

for(i in 2016:2047){
  deviances.q[,ac(i),,,,idx$llq =="1.01"]  <- deviances.q[,ac(i),,,,idx$llq =="1.01"] * 1.1
}




