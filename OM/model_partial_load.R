# model_partial_load.R - DESC
# /model_partial_load.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(ss3om)


# --- LOAD OM grid

load("model/partial/grid.Rdata")

dirs <- list.dirs("model/partial", recursive=FALSE)

partial<- loadOMS(subdirs=dirs, grid=grid,range=c(minfbar=2, maxfbar=8))

# ADD cpuefits for indices 2 and 6
cpuefits <- lapply(partial$output, function(x)
  ss3om:::ss3index.fit(data.table(x$cpue),
  fleets=setNames(nm=c("UJPLL_NW", "UTWLL_NW")))
)

partial$cpuefits <- FLQuants(lapply(setNames(1:2, nm=c("UJPLL_NW", "UTWLL_NW")),
  function(i) Reduce(combine, lapply(cpuefits, '[[', i))))

save(partial, file="model/partial/partial.Rdata", compress="xz")

# LOAD retros 0 to 5

retros <- foreach(x=setNames(dirs, nm=seq(length(dirs)))) %dopar% {
  
  rdirs <- setNames(c(x, as.list(list.dirs(file.path(x, "retro"),
    recursive=FALSE))), nm=seq(0, 5))
  
  rretro <- lapply(rdirs, readOutputss3)
  
  return(SSsummarize(rretro, verbose=FALSE))
}

save(retros, grid, file="model/partial/retros.Rdata", compress="xz")

