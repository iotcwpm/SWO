# test.R - DESC
# /test.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ss3om)
library(data.table)

dir <- "io4_NTP_h75_GaMf_r2_CL020"

# READ as FLStock
swo <- readFLSss3("io4_NTP_h75_GaMf_r2_CL020")

# PLOT by area
plot(swo) + 
  facet_grid(qname~area, scales="free_y")

# PLOT by area & unit
png(file="swoomu.png")
ggplot(metrics(swo, SSB=ssb, REC=rec, CATCH=catch), aes(x=year, y=data, group=unit)) +
  geom_line(aes(colour=unit)) + ylab("") +
  facet_grid(qname~area, scales="free_y")
dev.off()

# unit & areaSums of metrics
met <- lapply(lapply(metrics(swo), unitSums), areaSums)
png(file="swoom.png")
plot(met)
dev.off()

# r4ss
out <- r4ss::SS_output("io4_NTP_h75_GaMf_r2_CL020")
out <- lapply(out, function(x) if(is(x, "data.frame")) data.table(x) else x)

# CHECKS

# REC & SSB (out$recruit)
outsr <- out$recruit[era == "Main", c("Yr", "SpawnBio", "exp_recr")]

# diff(rec)
mean(outsr$exp_recr / met$Rec[, ac(outsr$Yr)])

# CATCH
outca <- out$catch[, .(catch=sum(Obs)), by=Yr]

# diff(catch)
mean(outca$catch / met$Catch)

ggplot(outca, aes(x=Yr, y=catch)) + geom_line()

# ---

readRESIDss3(dir)

# ---

library(r4ss)

dats <- r4ss::SS_readdat("io4_NTP_h75_GaMf_r2_CL020/DATA.SS", verbose=FALSE)
ctls <- r4ss::SS_readctl_3.24(file="io4_NTP_h75_GaMf_r2_CL020/CONTROL.SS",
  use_datlist=T, datlist=dats, verbose=FALSE)
