#' ---
#' title: "SWO"
#' author: "Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>"
#' date: "IOTC WPM MSE - Lisbon - March 2018"
#' output:
#'    beamer_presentation:
#'       theme: metropolis
#' ---

#+ echo=FALSE, warning=FALSE, message=FALSE
library(ioswomse)
library(data.table)
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE) 
# theme_set(theme_light())

# --- OM

#' # SA
data(sa)
plot(sa)

#' # GRID
load('./out/fqsfull.RData', verbose=FALSE)
res <- data.table(res)

#' - Natural mortality, M=c(0.2, 0.4, 999)
#' - SR steepness, h=c(0.6, 0.75, 0.9),
#' - Rec variance, sigmaR=c(0.2, 0.6),
#' - Weight of length samples, ess=c(2, 20),
#' - Trends in LL catchability, llq=c(1, 1.01),
#' - Growth + maturity, growmat=c("farley", "wang"),
#' - CPUEs, cpue=c("jappt", "jap", "twnpt"),
#' - Area CPUE scaling factor, scaling=c("area", "catch", "biomass"))
#'

#' # $B_0$

ggplot(res) + geom_histogram(aes(TotBio_Unfished)) +
  xlab(expression(B[0] (t))) + ylab("")

#' # $SB_{MSY}$

ggplot(res) + geom_histogram(aes(SSB_MSY * 2)) +
  xlab(expression(B[MSY] (t))) + ylab("")

#' # $MSY$

ggplot(res) + geom_histogram(aes(TotYield_MSY * 2)) +
  xlab(expression(MSY (t))) + ylab("")

#' # $B_0$ by factor

resl <- melt(res, id.vars=colnames(res)[seq(1, 7)],
  measure.vars=colnames(res)[seq(9, 22)]) 

vcols <- colnames(resl)[1:7] 
resl[, (vcols):=lapply(.SD, as.character), .SDcols = vcols]

dat <- melt(resl[variable == "TotBio_Unfished"],
  id.vars="value", measure.vars=vcols,
  variable.name="factor", value.name = "level")

ggplot(dat, aes(x=as.factor(level), y=value, fill=factor)) + geom_boxplot() +
  facet_grid(.~factor, scales="free_x") + xlab("") + ylab(expression(B[0])) +
  theme(legend.position="none")

#' # MSY by factor

dat <- melt(resl[variable == "TotYield_MSY"],
  id.vars="value", measure.vars=vcols,
  variable.name="factor", value.name = "level")

ggplot(dat, aes(x=as.factor(level), y=value, fill=factor)) + geom_boxplot() +
  facet_grid(.~factor, scales="free_x") + xlab("") + ylab("MSY") +
  theme(legend.position="none")

# SR & residuals vs. steepness

#' # Subsetting
#' - BO > 1.5 Mt, 1038
#' - Convergence level > 0.001, 1198
#' - Combined, 1002
#'

#' # OM
data(om)
res <- data.table(res)
plot(FLStocks(SA=sa, OM=om))

# TODO 
#' # $B_0$ by factor

resl <- melt(res, id.vars=colnames(res)[seq(1, 7)],
  measure.vars=colnames(res)[seq(9, 22)]) 

vcols <- colnames(resl)[1:7] 
resl[, (vcols):=lapply(.SD, as.character), .SDcols = vcols]

dat <- melt(resl[variable == "TotBio_Unfished"],
  id.vars="value", measure.vars=vcols,
  variable.name="factor", value.name = "level")

dat[, count := .N, by = .(factor, level)]

ggplot(dat, aes(x=as.factor(level), y=value, fill=factor)) + geom_boxplot() +
  facet_grid(.~factor, scales="free_x") + xlab("") + ylab(expression(B[0])) +
  theme(legend.position="none") +
  geom_text(data=unique(dat, by=c('factor', 'level')),
    aes(label=count), y=min(dat$value), size=3)

#' # MSY by factor

dat <- melt(resl[variable == "TotYield_MSY"],
  id.vars="value", measure.vars=vcols,
  variable.name="factor", value.name = "level")

dat[, count := .N, by = .(factor, level)]

ggplot(dat, aes(x=as.factor(level), y=value, fill=factor)) + geom_boxplot() +
  facet_grid(.~factor, scales="free_x") + xlab("") + ylab("MSY") +
  theme(legend.position="none") +
  geom_text(data=unique(dat, by=c('factor', 'level')),
    aes(label=count), y=min(dat$value), size=3)

#' # SSB(OM) / SBMSY

plot(ssb(om) / orpts$SBMSY)

#' # SB/SBMSY SA vs. OM

plot(FLQuants(SA=ssb(sa) / resa$SSB_MSY, OM=ssb(om) / orpts$SBMSY)) +
  geom_hline(aes(yintercept=1)) + ylab(expression(SB/SB[MSY])) +
  facet_grid(qname~.)

#' # C/MSY SA vs. OM

plot(FLQuants(SA=catch(sa) / resa$TotYield_MSY, OM=catch(om) / orpts$MSY)) +
  geom_hline(aes(yintercept=1)) + ylab(expression(C/MSY)) +
  facet_grid(qname~.)

#' # SA vs. OM
# TODO
# plot(FLCore::iter(fqs$SSB, idx) / (orpts$SBMSY/4)) + geom_hline(aes(yintercept=1))

