# model_partial_rate.R - DESC
# /model_partial_rate.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

library(forecast)
library(data.table)


# --- DIAGNOSTICS & METRICS

load("model/partial/partial.Rdata")
load("model/partial/retros.Rdata")

results <- partial$results

# 1. FIND unrealistic values (SSB_Virgin > 4.5e5 t, SSB_status > 3)

results[, del1:=SSB_Virgin > 4.5e5 | SSB_status > 3]

sum(results[, .(del1)])

# 2. CHECK convergence < 1e-3

results[, del2:=Convergence_Level > 1e-3]

# 3. COMPUTE retrospective Mohn's rho

mrhos <- lapply(retros, SSmohnsrho, startyr=2013, verbose=FALSE)

# ADD to res
results[, mrho:=unlist(lapply(mrhos, '[[', 'AFSC_Hurtado_SSB'))]

# 4. COMPUTE measure of process error

results[, Recr_sigma := c(sqrt(yearVars(residuals(partial$sr)[, ac(1950:2016)])))]

# 5. COMPUTE hcxval prediction skill

ssmases <- rbindlist(foreach(x=seq(retros)) %dopar% {
  
  # EXTRACT residuals: NW area
  res <-data.table(SSmase(retros[[x]], Season="default",
    residuals=TRUE)$Residuals)[Index %in% fifelse(results[iter==x, cpue=="jappt"|cpue=="jap"], "UJPLL_NW", "UTWLL_NW")]

  mase <- sum(abs(res$Pred.Res)) / sum(abs(res$Native.Res))
  
  # GET p-value
  pvalue <- unname(dm.test(res$Pred.Res, res$Native.Res,
    alternative="greater")$p.value)
  
  data.table(mase=mase, pvalue=pvalue)
  }
)

results[, c("mase","pvalue") := ssmases]

# ID runs with mase(NW) > 1

results[, del3 := mase > 1]

# 6. ACF for recruitment and SSB

# CREATE acfs table

# SSB from FLR
ssbs <- lapply(partial$output, extractSSB)
# SSB from SS
ssbs[["0"]] <- extractSSB(readOutputss3("model/base"))

acfs <- lapply(ssbs, function(x)
  acf(c(x), lag.max=30, plot=FALSE))
acfs <- lapply(acfs, function(x)
  data.table(lag=c(x$lag), acf=c(x$acf)))
acfs <- rbindlist(acfs, idcol="iter")

# 7. COMPUTE ccf(rec, ssb)

recs <- lapply(partial$output, extractRec)
recs[["0"]] <- extractRec(readOutputss3("model/base"))

ccfs <- Map(function(x, y) ccf(c(x), c(y), plot=FALSE, lag.max=20),
  recs, ssbs)

ccfs <- rbindlist(lapply(ccfs, function(x) data.table(lag=c(x$lag), acf=c(x$acf))),
  idcol="iter")

# TODO ADD ccfs &| acfs to results

# 8. ADD iters selection

results[, sel:= !del1 & !del2 & !del3]

# SAVE results + diagnostics
save(results, ssmases, acfs, ccfs, file="model/partial/diagnostics.Rdata",
  compress="xz")
