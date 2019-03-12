# load.R - DESC
# /load.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ss3om)

library(doParallel)
registerDoParallel(200)

dir <- "grid"
load("out/metrics_sub.RData")

# LOAD FLStock

swo <- loadFLS(subdirs=file.path(dir, grid$id[results$sample]),
  repfile="Report.sso.gz", compfile = "CompReport.sso.gz")

range(swo, c("minfbar", "maxfbar")) <- c(2,8)

# LOAD FLom elements

swom <- loadOMS(subdirs=file.path(dir, grid$id[results$sample]),
  repfile="Report.sso.gz", covarfile="covar.sso.gz",
  compfile = "CompReport.sso.gz", simplify="area")

range(swom$stock, c("minfbar", "maxfbar")) <- c(2,8)

save(swom, file="out/oms.RData", compress="xz")
