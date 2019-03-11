# test.R - DESC
# /test.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


library(mse)
library(FLBRP)

load("out/oms.RData")

# MERGE units
# stock <- simplify(swom$stock, c("unit"))

# DROP iters
stock <- slim(swom$stock)

# DROP age 0
stock <- stock[-1,]

# SR
sr <- swom$sr

# CONVERT bevholtSS3 to bevholt
srpars <- do.call('FLPar', abPars("bevholt", spr0=c(params(sr)$v/params(sr)$R0),
  s=c(params(sr)$s), v=c(params(sr)$v)))

# CONSTRUCT FLBRP object
brps <- FLBRP(stock, list(model="bevholt", params=srpars), fbar=seq(0, 2, length=101))

# EXTEND to endyear
stock <- fwdWindow(stock, brps, end=2047)

# CREATE FLom
om <- FLom(stock=stock, sr=swom$sr, refpts=swom$refpts)

save(om, file="out/om.RData", compress="xz")

# SMALL

om <- FLom(stock=iter(stock, 1:50), sr=iter(swom$sr, 1:50),
  refpts=iter(swom$refpts, 1:50))

save(om, file="out/omsmall.RData", compress="xz")


