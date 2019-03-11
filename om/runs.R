# test.R - DESC
# /test.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


library(mse)
library(FLasher)


years <- 2016:2047
years <- 2015

load("out/omsmall.RData")

# fwd F0
proj_f0 <- window(fwd(om, control=fwdControl(year=years, quant="f",
  value=1e-12),
  deviances=FLQuant(1, dimnames=list(year=years, unit=c('F', 'M'), iter=1:100))),
  start=years[1]-1, end=years[length(years)])

# fwd FMSY
proj_fmsy <- window(fwd(om, control=fwdControl(year=years, quant="f",
  value=rep(c(refpts(om)$FMSY), each=length(years)))),
  start=years[1]-1, end=years[length(years)])

# fwd C2015
proj_c2015 <- window(fwd(om, control=fwdControl(year=years, quant="catch",
  value=rep(c(catch(stock(om))[,'2015']), each=length(years)))),
  start=years[1]-1, end=years[length(years)])

# fwd F2015
proj_f2015 <- window(fwd(om, control=fwdControl(year=years, quant="f",
  value=rep(c(fbar(stock(om))[,'2015']), each=length(years)))),
  start=years[1]-1, end=years[length(years)])

runs <- FLStocks(f0=stock(proj_f0), fmsy=stock(proj_fmsy), c2015=stock(proj_c2015),
  f2015=stock(proj_f2015), om=stock(om))

plot(runs)


# ADD future deviances
residuals(sr) <- deviances

