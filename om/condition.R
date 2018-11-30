# condition.R - DESC
# ioswomse/exec/condition.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ioswomse)

library(doParallel)
registerDoParallel(200)

# --- SCENARIOS

scenarios <- list(
  # Natural mortality, M
  M=c(0.20, 0.30, 999),
  # SR steepness
  steepness=c(0.6, 0.75, 0.9),
  # Rec variance
  sigmaR=c(0.2, 0.6),
  # Weight of length samples
  ess=c(2, 20),
  # Trends in LL catchability
  llq=c(1, 1.01),
  # Growth + maturity
  growmat=c("farley", "wang"),
  # CPUEs
  cpue=c("jappt", "jap", "twnpt"),
  # Area CPUE scaling factor
  scaling=c("area", "catch", "biomass"),
  # LL selectivity model
  llsel=c("DoNorm", "Logistic")
  )

# --- DATA

data(cpues)
data(lorenzen)

# -- SETUP grid/SS3 folders

dir <- "grid"

grid <- setioswogrid(scenarios, cpues=cpues, dir=dir, base='./sa/', write=TRUE)

# -- RUN SS3 grid

# $ parallel --joblog log --jobs 200 --progress 'cd {} && ss3_3.24z && packss3run' ::: *

runss3grid(grid, options="", dir=dir, pack=TRUE)

# -- LOAD all results

# res

results <- loadRES(dir=dir, subdirs=grid$id, repfile = "Report.sso.gz",
  covarfile = "covar.sso.gz", compfile = "CompReport.sso.gz", grid=grid)

save(grid, results, file="out/resultsALL.RData", compress="xz")


# SUBSET by convergence level

idx <- results$Convergence_Level < 0.001
results <- results[idx,]
grid <- grid[idx,]

# metrics

metrics <- loadFLQs(subdirs=file.path(dir, grid$id),
  repfile="Report.sso.gz", covarfile="covar.sso.gz", compfile = "CompReport.sso.gz",
  metrics=list(REC=rec, SSB=ssb, C=catch, F=fbar, B=stock))

units(metrics[["REC"]]) <- "1000"
units(metrics[["SSB"]]) <- "t"
units(metrics[["C"]]) <- "t"
units(metrics[["F"]]) <- "f"
units(metrics[["B"]]) <- "t"

# residuals(sr) & residuals(cpue)

residuals <- loadRESIDs(subdirs=file.path(dir, grid$id),
  repfile="Report.sso.gz", covarfile="covar.sso.gz", compfile = "CompReport.sso.gz")

save(results, grid, metrics, residuals, file="out/metrics.RData", compress="xz")





# --- TODO


# DEBUG
trace(parallel:::sendMaster, at = 3L, tracer = quote({ str(list(what = what)) }))

# LOAD FLS as DT

odt <- loadFLS(subdirs=file.path(dir, grid$id[idx]), combine=FALSE,
  repfile="Report.sso.gz", covarfile="covar.sso.gz", compfile = "CompReport.sso.gz")

setindex(odt, slot)

# SIMPLIFY odt

dt <- copy(odt)

# TRIM iters in m.spwn, harvest.spwn
dt[, keep:=TRUE]
dt[iter > 1 & slot %in%
  c("m.spwn", "harvest.spwn", "discards", "discards.n", "discards.wt"), keep:=FALSE]

setindex(dt, keep)
dt <- dt[(keep),]
dt[, keep:=NULL]
dt[, season:=NULL]

system.time(stk <- as(dt, "FLStock"))

system.time(stk <- as(dt[iter < 10], "FLStock"))
system.time(stk <- as(dt[iter < 100], "FLStock"))
system.time(stk <- as(dt[iter < 250], "FLStock"))
system.time(stk <- as(dt[iter < 500], "FLStock"))


# BUG catch.wt[1950:1955]

# EXTRACT m
system.time(m <- odt[slot == "m",])
m[, slot:=NULL]
m[, units:=NULL]
system.time(fqm <- as(m, 'FLQuant'))

# SUBSET 250 iters
system.time(tes <- simplify(as(odt[iter %in% sample(m$iter, 250),], "FLStock"), "area"))

plot(tes)

# ----------------

# STK

# SET range of ages fully selected
range(omf, c("minfbar", "maxfbar")) <- c(2,8)

# SET name and desc(ription)
name(omf) <- "SWO"
desc(omf) <- paste("IOTC SWO SS3", dir)

save(omf, res, file="out/omfull.RData", compress="xz")

# fqsfull
fqs <- metrics(omf)

save(fqs, res, file="out/fqsfull.RData", compress="xz")


# --- INSPECT and SUBSET runs

# (1) TRIM if BO > 1.5 Mt
idb0 <- res$TotBio_Unfished < 1.5e6

# (2) TRIM if Convergence level > 0.001
idcl <- res$Convergence_Level <= 0.001

idx <- idcl & idb0

# SUBSET by idx
om <- FLCore::iter(omf, idx)

# MERGE areas
om <- simplify(om, c("area", "unit"))

# DROP age 0
om <- om[-1,]

# MERGE ages 21-31 in plusgroup
om <- setPlusGroup(om, 20)

# rpts: MSY, SB_MSY, F_MSY, SB0
orpts <- with(res[idx,], FLPar(MSY=TotYield_MSY, SBMSY=2 * SSB_MSY, FMSY=Fstd_MSY,
  SB0=2 * SPB_Virgin, Ftarget=Fstd_MSY, SBlim=2 * 0.40 * SSB_MSY,
  units=c("t", "t", "f", "t", "f", "t")))

# sr
resid <- loadquants(subdirs=file.path("grid", grid$id[idx]), object="resid",
  repfile = "Report.sso.gz", covarfile = "covar.sso.gz", compfile="CompReport.sso.gz")

osr <- predictModel(model="bevholtss3",
  params=with(res[idx,], FLPar(s=steepness, R0=exp(`SR_LN(R0)`), v=SPB_1950,
    units=c("", "1000", "t"))), FLQuants(residuals=resid[, ac(1975:2013)]))

# res
res <- res[idx,]

# omp
orp <- FLBRP(om)
omp <- fwdWindow(om, orp, end=2045)

save(om, orpts, osr, res, file='out/om.RData', compress='xz')
#save(om, omp, orpts, osr, res, file='out/om.RData', compress='xz')

# fqs
fqs <- metrics(om)

save(fqs, res, orpts, file="out/fqs.RData", compress="xz")
