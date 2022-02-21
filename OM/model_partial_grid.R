# model_partial.R - DESC
# /model_partial.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ss3om)
library(ss3diags)

source("utilities.R")

library(AlgDesign)
library(forecast)

# SETUP parallel

library(doParallel)
registerDoParallel(2)

# LOAD data: CPUEs and Lorenzen M vectors

load("data/cpues.Rdata")
load("data/lorenzen.Rdata")

# SETUP full grid

full <- list(
  M=c(0.20, 0.30, 999),
  steepness=c(0.6, 0.75, 0.9),
  sigmaR=c(0.2, 0.6),
  ess=c(2, 20),
  llq=c(1, 1.01),
  growmat=c("farley", "wang"),
  cpue=c("jappt", "jap", "twnpt"),
  scaling=c("area", "catch", "biomass"),
  llsel=c("DoNorm", "Logistic")
)

fullgrid <- expand.grid(lapply(full, factor))

# --- EVALUATE nTrials designs

# APPLY optFederov
res <- lapply(setNames(nm=seq(50, 250)), optFederov, frml= ~ ., data=fullgrid,
  evaluateI=T)

# EVALUATE design
eva <- lapply(lapply(res, "[[", "design"), function(x)
  eval.design(frml= ~ ., x, confounding=TRUE, X=fullgrid))

# CREATE table
evd <- rbindlist(lapply(lapply(eva, "[", 2:8), as.data.frame), idcol="nTrials")

# GET design for 108 nTrials

des <- optFederov( ~ ., data=fullgrid, nTrials = 108)
eva <- eval.design( ~ ., des$design, confounding=TRUE, X=fullgrid)

# CHECK design table, equal no. per factor
lapply(des$design, function(x) all(table(x) == mean(table(x))))

# --- EXTRACT design 

design <- data.table(cbind(des$design, iter=seq(1, 108),
  row=as.numeric(rownames(des$design))))

design[, c("M","steepness","sigmaR","ess","llq"):=lapply(design[, 1:5],
  function(x) as.numeric(as.character(x)))]

design <- ss3om::nameGrid(design, from=1)

# --- SETUP model grid (write to TRUE)

grid <- setioswogrid(design, cpues=cpues, dir = "om/model/partial",
  base = "om/data/sa", name = "swo", write = FALSE)

save(grid, file = "model/partial/grid.Rdata")

# SETUP retro runs

lapply(file.path("om/model/partial", grid$id), prepareRetro)

# SHELL: run SS3 in parallel
# ls | parallel -j50 --bar --progress '(cd {}; ss3 && packss3run; cd retro; for d in ./*/ ; do (cd "$d" && ss3 && packss3run); done)'
