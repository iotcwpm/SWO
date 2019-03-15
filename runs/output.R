# output.R - DESC
# /output.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


library(ioalbmse)
library(mseviz)

# DATA

load("out/perf_tune.RData")

data(iotcindicators)

# plotBPs

plotBPs(perf, indicators=c("S3", "S6", "F2", "Y1", "T1"),
  target=list(S3=1), limit=c(S3=0.4))

# plotTOs

plotTOs(perf, x="Y1", y=c("S3", "S6", "F2", "T2"))

# kobeMPs

kobeMPs(perf)

# kobeTS

kobeTS(perfkobe)

# plotOMruns

plotOMruns(om$FMSY, FLQuants(lapply(tuns, "[[", "FMSY")), limit=1.4, target=1)

plotOMruns(om$SBMSY, FLQuants(lapply(tuns, "[[", "SBMSY")), limit=0.4, target=1)

# SUMMARY table

summTable(perf)

# LONG table

resTable(perfts[year==2023], indicators)

resTable(perfts[year==2028], indicators)

resTable(perfts[year==2038], indicators)
