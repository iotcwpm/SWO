# model_base.R - Runs and diagnostics for the base case SS3 model
# SWO/OM/model_base.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
# Modified: Daniela Rosa (IPMA)

# Distributed under the terms of the EUPL-1.2


library(ss3om)
library(ss3diags)
library(icesTAF)

# SET base case = io4_h80_GoMf_r2_CL005

mkdir("om/model/base")

cp("om/data/sa/*", "om/model/base/")


# --- DIAGNOSTICS

# 2. Convergence level

res_base$Convergence_Level > 1e-4


# RE-RUN with starter.ss$$init_values_src = 1
# starter <- r4ss::SS_readstarter('model/base/starter.ss', verbose=FALSE)
# starter$jitter_fraction <- 0.25
# 
# SS_writestarter(starter, dir=file.path("model/base"), overwrite=TRUE)

# --- RETRO

# CREATE dir
mkdir("om/model/base/retro")

# CREATE retros
prepareRetro("om/model/base", years=10)

# RUN retro

# system("cd model/base/retro; ls | parallel -j10 --progress '(cd {}; ss3 -nox)'")

# LOAD output

base <- readOMSss3("model/base", range=c(minfbar=2, maxfbar=8))

# LOAD retro

dirs <- setNames(c("model/base", list.dirs("model/base/retro",
  recursive=FALSE)), nm=seq(0, 10))

retro <- lapply(dirs, readOutputss3)

retrostk <- loadFLS("model/retro")

# SUMMARIZE 5 peels only
retrosumm <- SSsummarize(retro[1:6])

# FLStocks retro
retrofls <- FLStocks(lapply(retro, buildFLSss330, range=c(minfbar=2, maxfbar=8)))

# SAVE
save(base, retro, retrosumm, file="model/base.Rdata", compress="xz")


# --- DIAGNOSTICS

load("model/base.Rdata")

# - fit & convergence

convergencelevel("model/base")

# - catch likelihood > 1e-5

base$likelihoods_used["Catch", "values"] > 1e-5

# - Mohn's rho

SSmohnsrho(retrosumm)

SSplotRetro(retrosumm, xmin=2005)

# - runs test

# CPUEs

# tiff(file="CPUE_runs.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 300)
sspar(mfrow=c(3, 2), plot.cex = 0.7)
cpue_rtes <- SSplotRunstest(out, add=T, subplots="cpue", indexselect=c(1:4,9))
# dev.off()


# LN
sspar(mfrow=c(2,2), plot.cex = 0.7)
len_rtes <- SSplotRunstest(out, add=T, subplots="len")

# - MASE HCXVAL

SShcbias(retrosumm)

sspar(mfrow=c(2, 2))
SSplotHCxval(retrosumm, indexselect = c(1,2,4,9))
