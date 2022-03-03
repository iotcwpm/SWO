# test_perfect.R - DESC
# /test_perfect.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

source("model.R")

library(doParallel)
registerDoParallel(3)

# SUBSAMPLE for testing
idx <- sample(seq(500), 25)
om <- iter(om,  idx)
oem <- iter(oem, idx)


# --- perfect.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0.10, trigger=0.40, target=mean(refpts(om)$MSY) * 2 * 0.90,
      metric=relmets$BMSY, output="catch", dlow=0.85, dupp=1.15))))

system.time(perf_hcst <- mp(om, oem=oem, ctrl=control, args=mseargs, parallel=TRUE))



