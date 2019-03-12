# setup.R - DESC
# /setup.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# XX {{{
# }}}

# SR DEVIANCES


# INDEX

cpue <- indices[[3]]

sel.pattern <- unitMeans(sel.pattern(cpue))[-1,,,3]
dimnames(sel.pattern) <- list(age=1:14)

cpue <- FLIndexBiomass(name=name(cpue), desc="IO ALB LLCPUE3",
  index=index(cpue)[,,,3],
  index.var=index.var(cpue)[,,,3],
  index.q=index.q(cpue)[,,,3],
  sel.pattern=sel.pattern)
range(cpue, c("startf", "endf")) <- c(0.5, 0.5)

# EXTEND cpue

cpue <- window(cpue, end=2047)

# sel.pattern
sel.pattern(cpue)[, ac(2015:2047)] <- sel.pattern(cpue)[,'2014']

# REGENERATE q
vbiom <- quantSums(stock.n(stock)[,ac(1979:2017)] *
  exp(-harvest(stock)[,ac(1979:2017)] * 0.5 - m(stock)[,ac(1979:2017)] * 0.5) *
  stock.wt(stock)[,ac(1979:2017)] *
  sel.pattern(cpue)[,ac(1979:2017)])

index.q(cpue)[, ac(1979:2014)] <- yearMeans(index(cpue)[, ac(1979:2014)] / vbiom [, ac(1979:2014)])

# EXTEND to 2047
index.q(cpue)[, ac(2015:2047)] <- index.q(cpue)[,'2014']

# RECREATE index 2015-2017
index(cpue)[, ac(2015:2017)] <- index.q(cpue)[, ac(2015:2017)] * vbiom[, ac(2015:2017)]

# index.var
index.var(cpue)[, ac(2017:2047)] <- index.var(cpue)[,'2014']

# SET FUTURE qs

loq <- log(index(cpue)[,ac(1979:2017)] / vbiom)
qmu <- yearMeans(loq)
qsig <- sqrt(yearVars(loq))

# FUTURE qs
set.seed(2981)

deviances.q <- rlnorm(1, expand(qmu, year=2018:2047), expand(qsig, year=2018:2047))


# llq = 1.0025
for(i in 2019:2047)
  deviances.q[,ac(i),,,,results$llq =="1.0025"]  <- deviances.q[,ac(i-1),,,,results$llq =="1.0025"] * 1.1



