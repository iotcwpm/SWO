# model_jabba.R - DESC
# /model_jabba.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

source("utilities.R")

# DATA
load("data/om.Rdata")
# --- DEBUG
vcov(sr(om)) <- hessian(sr(om))

# SUBSAMPLE for testing
idx <- sample(seq(500), 5)
om <- iter(om,  idx)
oem <- iter(oem, idx)

# SETUP
mseargs <- list(iy=2019, data_lag=2, frq=3)

test <- list()


# --- TEST HCRs

# perfect.sa + fixedF.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr,
    args=list(ftrg=mean(refpts(om)$FMSY)))))

test$perf_fixf_fmsy <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, test$perf_fixf_fmsy)

# perfect.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.85, nyears=5, metric=stock))))

test$perf_tren_0.85 <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, test$perf_tren_0.85)

# perfect.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=mean(refpts(om)$SB0) * 0.10,
      trigger=mean(refpts(om)$SB0) * 0.40,
      target=mean(refpts(om)$MSY) * 0.90, min=500,
    metric='ssb', output='catch'))))

test$perf_hcst_090MSY <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, test$perf_hcst_090MSY)
plot_hockeystick.hcr(control$hcr) + xlab("SSB(t)")+ ylab("catch (t)")

save(test, file="model/test_perf.Rdata", compress="xz")

plot(om, test)

test <- list()

# --- TEST jabba.sa

# jabba.sa + fixedF.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=fixedF.hcr,
    args=list(ftrg=mean(refpts(om)$FMSY)))))

test$jabba_fixf_fmsy <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, test$jabba_fixf_fmsy)


# jabba.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.85, nyears=5, metric=stock))))

test$jabba_tren_0.85 <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, test$jabba_tren_0.85)


# jabba.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=11000, trigger=55000, target=30000, min=500,
    metric='ssb', output='catch'))))

test$jabba_hcst_090MSY <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, test$jabba_hcst_090MSY)

save(test, file="model/test_jabba.Rdata", compress="xz")
