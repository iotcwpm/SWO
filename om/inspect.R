# inspect.R - DESC
# /inspect.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# XX {{{
# }}}

load("out/resultsALL.RData")

# SUBSET

idx <- results$Convergence_Level < 0.001

results <- results[idx,]


# dist(SSB_Virgin)

# dist(SSB_MSY/SSB_Virgin)

# dist(SSB_MSY/SSB_Virgin) by relevant factor

ggplot(results, aes(x=ratio, y=..density..)) +
  geom_density(aes(fill=factor(steepness))) +
  facet_grid(M~growmat, scales="free_y") +
  xlab("SBMSY/SBVirgin")





ggplot(results, aes(SSB_Virgin)) + geom_density(aes(fill=factor(steepness)))


hist(results$SSB_Virgin)


# EXPLORE large and small SSB0 values

# CONVERT to factor
results[, (colnames(results)[1:9]) := lapply(.SD, factor), .SDcols = colnames(results)[1:9]]

# PLOTS
pdf(file="ssbvirgin.pdf")

for (i in colnames(results)[1:9]) {

  print(ggplot(results, aes_string(x=i, y="SSB_Virgin")) + geom_boxplot())

  print(ggplot(results, aes(reorder(iter, SSB_Virgin), SSB_Virgin)) +
    geom_point(aes_string(colour=i)) +
    geom_hline(yintercept=100000, colour='red') +
    facet_wrap(i))

  print(ggplot(results, aes(reorder(iter, SSB_endyr/SSB_Virgin), SSB_endyr/SSB_Virgin)) +
    geom_point(aes_string(colour=i)) +
    geom_hline(yintercept=0.15, colour='red') +
    facet_wrap(i))

}

dev.off()

#

results[, ratio:=SSB_Virgin / TotBio_Unfished]

pdf(file="ssb2biom.pdf")

for (i in colnames(results)[1:9]) {

  print(ggplot(results, aes_string(x=i, y="ratio")) + geom_boxplot())

  print(ggplot(results, aes(reorder(iter, ratio), ratio)) +
    geom_point(aes_string(colour=i)) +
    geom_hline(yintercept=0.5, colour='red') +
    facet_wrap(i))
}

dev.off()

# ---

dat <- r4ss::SS_readdat_3.24("sa/swo.dat", verbose=FALSE)
ctl <- r4ss::SS_readctl_3.24(file="sa/swo.ctl", use_datlist=T, datlist=dat,
  verbose=FALSE, ptype=FALSE)

r4ss::SS_writectl_3.24(ctl, "check_sa/swo.ctl", nseas=ctl$nseas)
r4ss::SS_writedat_3.24(dat, outfile="check_sa/swo.dat")

