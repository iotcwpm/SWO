# test.R - DESC
# /test.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


library(mse)
library(FLasher)

load("data/omp.RData")

years <- 2016:2036

# fwd F0
proj_f0 <- fwd(stock(om), sr=sr(om), deviances=residuals(sr(om)),
  control=fwdControl(year=years, quant="f", value=1e-12))

# fwd FMSY
proj_fmsy <- fwd(stock(om), sr=sr(om), deviances=residuals(sr(om)),
  control=fwdControl(year=years, quant="f",
  value=rep(c(refpts(om)$FMSY), each=length(years))))

# fwd C2015
proj_c2015 <- fwd(stock(om), sr=sr(om), deviances=residuals(sr(om)),
  control=fwdControl(year=years, quant="catch",
  value=rep(c(unitSums(catch(stock(om))[, "2015"])), each=length(years))))

# fwd F2015
proj_f2015 <- fwd(stock(om), sr=sr(om), deviances=residuals(sr(om)),
  control=fwdControl(year=years, quant="f",
  value=rep(c(unitMeans(fbar(stock(om))[, "2015"])), each=length(years))))

runs <- FLStocks(c(lapply(list(f0=proj_f0, fmsy=proj_fmsy, c2015=proj_c2015,
  f2015=proj_f2015), window, start=2015, end=2036), list(om=window(stock(om), end=2015))))

# tiff(file="int_proj.tiff", bg = "white", compression="lzw",
#      width = 24, height = 24, units = "cm", res = 300)
plot(runs) + facet_grid(qname~unit, scales="free_y")
# dev.off()

save(runs, file="out/runs.RData", compress="xz")
