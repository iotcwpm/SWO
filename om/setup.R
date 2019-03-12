# test.R - DESC
# /test.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


library(mse)
library(FLBRP)

load("out/oms.RData")
load('out/metrics_sub.RData')


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

# --- CPUEs
indices <- swom$indices
names(indices) <- c("UJPLL_NW", "UJPLL_NE", "UJPLL_SW", "UJPLL_SE", "UTWLL_NW", "UTWLL_NE",
  "UTWLL_SW", "UTWLL_SE", "UPOR_SW")

idx <- results[sample == TRUE, cpue]

# SE
cpuese <- indices[["UJPLL_SE"]]
iter(cpuese, idx == "twnpt") <- iter(indices[["UTWLL_SE"]], idx == "twnpt")
name(cpuese) <- "LL_SE"

# NE
cpuene <- indices[["UJPLL_NE"]]
iter(cpuene, idx == "twnpt") <- iter(indices[["UTWLL_NE"]], idx == "twnpt")
name(cpuene) <- "LL_NE"

# NW
cpuenw <- indices[["UJPLL_NW"]]
iter(cpuenw, idx == "twnpt") <- iter(indices[["UTWLL_NW"]], idx == "twnpt")
name(cpuenw) <- "LL_NW"

cpues <- FLIndices(LL_NW=cpuenw, LL_NE=cpuene, LL_SE=cpuese)

save(om, cpues, file="out/om.RData", compress="xz")

# SMALL

om <- FLom(stock=iter(stock(om), 1:50), sr=iter(sr(om), 1:50),
  refpts=iter(refpts(om), 1:50))

cpues <- lapply(cpues, iter, 1:50)

save(om, cpues, file="out/omsmall.RData", compress="xz")
