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

grid <- setioswogrid(scenarios, cpues=cpues, dir=dir, base='./src/', write=FALSE)

# -- RUN SS3 grid

# $ parallel --joblog log --jobs 200 --progress 'cd {} && ss3_3.24z && packss3run' ::: *

runss3grid(grid, options="", dir=dir, pack=TRUE)

# -- LOAD all results

# res

results <- loadRES(dir=dir, subdirs=grid$id, repfile = "Report.sso.gz",
  covarfile = "covar.sso.gz", compfile = "CompReport.sso.gz", grid=grid)

# FIND non-converged
idx <- results$Convergence_Level < 0.001

# RE-RUN with starter.ss$$init_values_src = 1
starter <- r4ss::SS_readstarter('src/starter.ss', verbose=FALSE)
starter$jitter_fraction <- 0.25

for(i in grid[!idx, 'id']) {
  SS_writestarter( starter, dir=file.path("grid", i), overwrite=TRUE)
}


# CHECK convergence level
sum(results$Convergence_Level < 0.001)

save(grid, results, file="out/results.RData", compress="xz")


# metrics

metrics <- loadFLQs(subdirs=file.path(dir, grid$id),
  repfile="Report.sso.gz", compfile = "CompReport.sso.gz",
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

