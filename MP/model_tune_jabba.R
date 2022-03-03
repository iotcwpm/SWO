# model_tune.R - DESC
# /model_tune.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


source("model.R")

library(doParallel)
registerDoParallel(2)

# load statistics
data(statistics)

# TPERIOD, last OM year + 11:15
tperiod <- list(2034:2039)

# PATH in anunna
#outpath <- "/home/WUR/mosqu003/backup/swo/MP/model/jabba"
#options(parallelly.availableCores.methods = "slurm")
outpath <- "model/jabba"


# --- TUNE(trigger) jabba.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0.10, trigger=0.40, target=mean(refpts(om)$MSY) * 0.90,
      metric=relmets$BMSY, output="catch", dlow=0.85, dupp=1.15))))

jabba_hcst_05_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.35, 0.75)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

performance(jabba_hcst_05_trig, metrics=mets[c("SB", "F")],
  statistics=statistics["green"], years=tperiod)

saveRDS(jabba_hcst_05_trig, file=file.path(outpath, "jabba_hcst_05_trig.Rds"),
  compress="xz")

tune$jabba_hcst_06_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.35, 0.75)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

saveRDS(jabba_hcst_06_trig, file=file.path(outpath, "jabba_hcst_06_trig.Rds"),
  compress="xz")

tune$jabba_hcst_07_trig <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(trigger=c(0.35, 0.75)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)

saveRDS(jabba_hcst_07_trig, file=file.path(outpath, "jabba_hcst_07_trig.Rds"),
  compress="xz")


# --- TUNE(target) jabba.sa + hockeystick.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=hockeystick.hcr,
    args=list(lim=0.10, trigger=0.40, target=mean(refpts(om)$MSY) * 0.90,
      metric=relmets$BMSY, output="catch", dlow=0.85, dupp=1.15))))

tune$jabba_hcst_05_targ <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

saveRDS(jabba_hcst_05_targ, file=file.path(outpath, "jabba_hcst_05_targ.Rds"),
  compress="xz")

tune$jabba_hcst_06_targ <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

saveRDS(jabba_hcst_06_targ, file=file.path(outpath, "jabba_hcst_06_targ.Rds"),
  compress="xz")

tune$jabba_hcst_07_targ <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(target=c(0.5e4, 8.5e4)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)

saveRDS(jabba_hcst_07_targ, file=file.path(outpath, "jabba_hcst_07_targ.Rds"),
  compress="xz")


# --- TUNE(gamma) jabba.sa + trend.hcr

control <- mpCtrl(list(
  est = mseCtrl(method=jabba.sa),
  hcr = mseCtrl(method=trend.hcr,
    args=list(k1=1.5, k2=3, gamma=1, nyears=5, metric=relmets$SB0))))

tune$jabba_tren_05_gamma <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(gamma=c(0.75, 1.15)), prob=0.5, tol=0.02, maxit=12, parallel=TRUE)

saveRDS(jabba_tren_05_gamma, file=file.path(outpath, "jabba_tren_05_gamma.Rds"),
  compress="xz")

tune$jabba_tren_06_gamma <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(gamma=c(0.75, 1.15)), prob=0.6, tol=0.02, maxit=12, parallel=TRUE)

saveRDS(jabba_tren_06_gamma, file=file.path(outpath, "jabba_tren_06_gamma.Rds"),
  compress="xz")

tune$jabba_tren_07_gamma <- tunebisect(om, oem=oem, control=control, args=mseargs,  
  metrics=mets[c("SB", "F")], statistic=statistics["green"], years=tperiod,
  tune=list(gamma=c(0.75, 1.15)), prob=0.7, tol=0.02, maxit=12, parallel=TRUE)

saveRDS(jabba_tren_07_gamma, file=file.path(outpath, "jabba_tren_07_gamma.Rds"),
  compress="xz")


# FINAL list

files <- list.files('model', pattern="*.Rds", full.name=TRUE)
tune <- lapply(setNames(files, gsub("\\.Rds$", "", basename(files))), readRDS)

save(tune, file="model/tune_jabba.Rdata", compress="xz")
