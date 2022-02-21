# model_partial_subset.R - DESC
# /model_partial_subset.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ss3om)
library(FLasher)
library(data.table)

load("model/partial/partial.Rdata")
load("model/partial/diagnostics.Rdata")


# --- SUBSET

sel <- results$sel
results <- results[(sel),]
sr <- iter(partial$sr, sel)
refpts <- partial$refpts[, sel]

# 2 SEX, no areas

stock <- stf(noarea(iter(partial$stock, sel)), end=2040)

# indices: index. index.q (index.var)

indices <- iter(FLIndices(partial$indices[c("UJPLL_NW", "UTWLL_NW")]), sel)
range(indices[[1]], c("startf", "endf")) <- c(0, 1)
range(indices[[2]], c("startf", "endf")) <- c(0, 1)

sel.pattern(indices[[1]]) <- unitMeans(sel.pattern(indices[[1]]))
sel.pattern(indices[[2]]) <- unitMeans(sel.pattern(indices[[2]]))

nqs <- computeQ(indices, window(nounit(stock), start=1994, end=2018),
  iter(partial$cpuefits, sel))
index.q(indices[[1]]) <- nqs[[1]]
index.q(indices[[2]]) <- nqs[[2]]

# FIX dimanmes$unit
indices[[1]] <- indices[[1]][,,1]
indices[[2]] <- indices[[2]][,,1]

dimnames(indices[[1]]) <- list(season='all')
dimnames(indices[[2]]) <- list(season='all')

# -- tests.R: TEST 2-sex model

save(stock, sr, refpts, results, indices, file="model/partial.Rdata",
  compress="xz")
