# model_tune.R - DESC
# /model_tune.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


source("model.R")

library(doFuture)
registerDoFuture()
plan(multisession, workers=3)

library(doParallel)
registerDoParallel(3)

# SUBSAMPLE for testing
idx <- sample(seq(500), 5)
om <- iter(om,  idx)
oem <- iter(oem, idx)


# --- jabba.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0.10, trigger=0.40, target=mean(refpts(om)$MSY) * 0.90,
      metric=relmets$BMSY, output="catch", dlow=0.85, dupp=1.15))))

system.time(jabba_hcst <- mp(om, oem=oem, ctrl=control, args=mseargs, parallel=TRUE))


# --- jabba.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=1, nyears=5, metric=relmets$SB0))))

system.time(jabba_tren <- mp(om, oem=oem, ctrl=control, args=mseargs, parallel=TRUE))

# FINAL list

jabba <- list(jabba_tren=jabba_tren, jabba_hcst=jabba_hcst)

save(jabba, file="model/jabba.Rdata", compress="xz")
