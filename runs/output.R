# output.R - DESC
# /output.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


library(ioswomse)
library(mseviz)

# DATA

load("out/perf_tunec.RData")

data(iotcindicators)

#Tunning for average over the entire projection period

# pdf("runs/perf_plots.pdf")
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

plotOMruns(omm$FMSY, FLQuants(lapply(tuns, "[[", "FMSY")), limit=1.4, target=1, ylim=c(0,3))

plotOMruns(omm$SBMSY, FLQuants(lapply(tuns, "[[", "SBMSY")), limit=0.4, target=1)


# SUMMARY table

summTable(perf)

# LONG table

resTable(perfts[year==2021], indicators)

resTable(perfts[year==2026], indicators)

resTable(perfts[year==2036], indicators)



#Tunning for average over the last 10yrs of projection
 # pdf("runs/perfb_plots.pdf")

# plotBPs

plotBPs(perfb, indicators=c("S3", "S6", "F2", "Y1", "T1"),
        target=list(S3=1), limit=c(S3=0.4))

# plotTOs

plotTOs(perfb, x="Y1", y=c("S3", "S6", "F2", "T2"))

# kobeMPs

kobeMPs(perfb)

# kobeTS

kobeTS(perfkobeb)


# plotOMruns

plotOMruns(omm$FMSY, FLQuants(lapply(tunsb, "[[", "FMSY")), limit=1.4, target=1, ylim=c(0,1.5))

plotOMruns(omm$SBMSY, FLQuants(lapply(tunsb, "[[", "SBMSY")), limit=0.4, target=1)


# SUMMARY table

summTable(perfb)

# LONG table

resTable(perftsb[year==2021], indicators)

resTable(perftsb[year==2026], indicators)

resTable(perftsb[year==2036], indicators)


#Tunning for average over 2030:2034
# pdf("runs/perfc_plots.pdf")

# plotBPs

plotBPs(perfc, indicators=c("S3", "S6", "F2", "Y1", "T1"),
        target=list(S3=1), limit=c(S3=0.4))

# plotTOs

plotTOs(perfc, x="Y1", y=c("S3", "S6", "F2", "T2"))

# kobeMPs

kobeMPs(perfc)

# kobeTS

kobeTS(perfkobec)


# plotOMruns

plotOMruns(omm$FMSY, FLQuants(lapply(tunsc, "[[", "FMSY")), limit=1.4, target=1, ylim=c(0,1.5))

plotOMruns(omm$SBMSY, FLQuants(lapply(tunsc, "[[", "SBMSY")), limit=0.4, target=1)


# SUMMARY table

summTable(perfc)

# LONG table

resTable(perftsc[year==2021], indicators)

resTable(perftsc[year==2026], indicators)

resTable(perftsc[year==2036], indicators)

