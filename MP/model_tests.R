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

# SUBSAMPLE for testing
idx <- sample(seq(500), 5)
om <- iter(om,  idx)
oem <- iter(oem, idx)


# --- DEBUG
vcov(sr(om)) <- hessian(sr(om))

# SETUP
mseargs <- list(iy=2019, data_lag=2, frq=3)

library(doFuture)
registerDoFuture()
plan(multicore, workers=50)

handlers(global = TRUE)

# --- TEST HCRs

# perfect.sa + fixedF.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=fixedF.hcr,
    args=list(ftrg=mean(refpts(om)$FMSY)))))

perSA_fixHCR <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, perSA_fixHCR)


# perfect.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.85, nyears=5, metric=stock))))

perSA_treHCR <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, perSA_treHCR)


# perfect.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=perfect.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=mean(refpts(om)$SB0) * 0.10,
      trigger=mean(refpts(om)$SB0) * 0.40,
      target=mean(refpts(om)$MSY) * 0.90, min=500,
    metric='ssb', output='catch'))))

perSA_hstHCR <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, perSA_hstHCR)
plot_hockeystick.hcr(control$hcr) + xlab("SSB(t)")+ ylab("catch (t)")

# runs

hcrs <- list(fixf=perSA_fixHCR, trend=perSA_treHCR, hockey=perSA_hstHCR)


# --- TEST jabba.sa

# jabba.sa + fixedF.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=fixedF.hcr,
    args=list(ftrg=mean(refpts(om)$FMSY)))))

jabSA_fixHCR <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, jabSA_fixHCR)


# jabba.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.85, nyears=5, metric=stock))))

jabSA_treHCR <- mp(om, oem, ctrl=control, args=mseargs)

plot(om, jabSA_treHCR)


# jabba.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=11000, trigger=55000, target=30000, min=500,
    metric='ssb', output='catch'))))

jabSA_hstHCR <- mp(om, oem, ctrl=control, args=mseargs)


# --- TUNE

tperiod <- 2030:2039

data(statistics)

# perfect.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=0.85, nyears=5, metric=stock))))

ti <- system.time(
tune_jabSA_stHCR <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=list(SB=function(x) unitSums(ssb(x)), F=function(x) unitMeans(fbar(x))),
  statistic=statistics["green"], years=tperiod, tune=list(gamma=c(0.75, 1.15)),
  prob=0.6, tol=0.01, maxit=12, parallel=TRUE)
)


# runs

jabs <- list(fixf=jabSA_fixHCR, trend=jabSA_treHCR, hockey=jabSA_hstHCR)

# SAVE

save(hcrs, jabs, file="model/tests.Rdata", compress="xz")
