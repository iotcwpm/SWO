# model_maineffects.R - DESC
# OM/model_maineffects.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
# Modified: Daniela Rosa (IPMA)
#
# Distributed under the terms of the EUPL-1.2

library(data.table)
library(icesTAF)
library(ss3om)
library(ioswomse)

library(doParallel)
registerDoParallel(3)

load("om/data/cpues.Rdata")
load("om/data/lorenzen.Rdata")

# SETUP runs for grid corners

full <- list(
  # Natural mortality, M
  M=c(0.20, 0.30, 999),
  # SR steepness
  steepness=c(0.6, 0.75, 0.9),
  # Rec variance
  sigmaR=c(0.2, 0.6),
  # Weight of length samples
  ess=c(2, 20),
  # Trends in LL catchability
  llq=c(1, 1.01),
  # Growth + maturity
  growmat=c("farley", "wang"),
  # CPUEs
  cpue=c("jappt", "jap", "twnpt"),
  # Area CPUE scaling factor
  scaling=c("area", "catch", "biomass", "region"),
  # LL selectivity model
  llsel=c("DoNorm", "Logistic")
)

nsam <- prod(unlist(lapply(full, length)))

# --- RUN grid

basecase  <- list(M=0.25, steepness=0.8, sigmaR=0.2, ess=5, llq=1, growmat="farley", cpue="jappt", scaling="biomass",llsel="DoNorm" )

meffs <- mapply(function(x, y) x[ac(x) != ac(y)], full, basecase)

grid <- rbindlist(lapply(rep(list(basecase),
                             sum(unlist(lapply(meffs, length)))), as.data.table))

grid[, col := rep(names(meffs), unlist(lapply(meffs, length)))]

for(i in names(grid))
  grid[col == i, i] <- meffs[[i]]

grid <- ss3om::nameGrid(grid, from=1)

grid <- setioswogrid(grid, cpues=cpues, dir = "om/model/maineffects",
                     base = "om/data/sa", name = "swo", write = TRUE)

save(grid, file = "om/model/maineffects/grid.Rdata")

lapply(file.path("om/model/maineffects", grid$id), prepareRetro)


# RUN models
# ls | parallel -j10 --bar --progress '(cd {}; ss3)'


# --- LOAD results

#LOAD SA
sa <- loadOMS(dir="data/gridIO4")
res_sa <- sa$results
stk_sa <- simplify(sa$stock, "area")
range(stk_sa, c("minfbar", "maxfbar")) <- c(2,8)

save(sa, file="model/gridIO4.Rdata", compress="xz")


# tiff(file="stock_sa.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 300)
plot(stk_sa)
#dev.off()

#LOAD MAINEFFECTS

load("model/maineffects/grid.Rdata")

maineffects <- loadOMS(dir="model/maineffects", grid=grid, combine=FALSE)


save(maineffects, file="model/maineffects/main.Rdata", compress="xz")


res <- maineffects$res

stk <- maineffects$stock

srr <- maineffects$sr



# --- DIAGNOSTICS


# 1. CHECK convergence < 1e-4

id1 <- res$Convergence_Level > 1e-4


# RE-RUN with starter.ss$$init_values_src = 1
# starter <- SS_readstarter('../sa/starter.ss', verbose=FALSE)
# starter$jitter_fraction <- 0.25
# 
# apply(grid[id1, "id"],1,function (x) SS_writestarter(starter, dir=file.path("model/maineffects", x), overwrite=TRUE))

# for(i in grid[id1, 'id']) {
#   SS_writestarter(starter, dir=file.path("model/maineffects", i), overwrite=TRUE)
# }


labels <- c("M=0.2","M=0.3", "M=Lorenzen", "h=0.6","h=0.75", "h=0.9", "sigmaR=0.6", "ess=2","ess=20",
             "llq=1.01","growmat=Wang", "cpue=jpn","cpue=twnpt", "scaling=area", "scaling=catch", "llsel=Logistic")

#PLOT SSB2018
ggplot() + geom_bar(data=res[-16,], aes(x=factor(iter), y=SSB_endyr, fill=col), stat="identity")+
  geom_hline(yintercept = min(res_sa$SSB_endyr))+
  geom_hline(yintercept=max(res_sa$SSB_endyr))+
  geom_hline(yintercept=res_sa$SSB_endyr[9], linetype=2)+
  scale_x_discrete("",labels=labels)+
  ylab(expression(SSB[2018]))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#PLOT SSB_Virgin

ggplot() + geom_bar(data=res[-16,], aes(x=factor(iter), y=SSB_Virgin, fill=col), stat="identity")+
  geom_hline(yintercept = min(res_sa$SSB_Virgin))+
  geom_hline(yintercept=max(res_sa$SSB_Virgin))+
  geom_hline(yintercept=res_sa$SSB_Virgin[9], linetype=2)+
  scale_x_discrete("", labels=labels)+
  ylab(expression(SSB[0]))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#PLOT status

ggplot() + geom_bar(data=res[-16,], aes(x=factor(iter), y=SSB_status, fill=col), stat="identity")+
  geom_hline(yintercept = min(res_sa$SSB_status))+
  geom_hline(yintercept=max(res_sa$SSB_status))+
  geom_hline(yintercept=(res_sa$SSB_status[9]), linetype=2)+
  scale_x_discrete("",labels=labels)+
  ylab(expression(SSB[2018]/SSB[MSY]))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#PLOT depletion

ggplot() + geom_bar(data=res[-16,], aes(x=factor(iter), y=SSB_depletion, fill=col), stat="identity")+
  geom_hline(yintercept = min(res_sa$SSB_depletion))+
  geom_hline(yintercept=max(res_sa$SSB_depletion))+
  geom_hline(yintercept=(res_sa$SSB_depletion[9]), linetype=2)+
  scale_x_discrete("", labels=labels)+
  ylab(expression(SSB[2018]/SSB[0]))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#PLOT SSBMSY/SSBVir
res_sa[,SSBMSYSSBVir:=SSB_MSY/SSB_Virgin]

ggplot() + geom_bar(data=res, aes(x=factor(iter), y=SSB_MSY/SSB_Virgin, fill=col), stat="identity")+
  geom_hline(yintercept = min(res_sa$SSBMSYSSBVir))+
  geom_hline(yintercept=max(res_sa$SSBMSYSSBVir))+
  geom_hline(yintercept=(res_sa$SSBMSYSSBVir[9]), linetype=2)+
  scale_x_discrete(breaks=1:17, labels=labels)



plot(stk, metrics=list(Rec=rec, SSB=ssb)) + facet_grid(qname~stock, scales="free")

plot(stk)+facet_grid(qname~area, scales="free")


#Length comp fits
essLog <- readOutputss3("model/maineffects/17-M0.25_sigmaR0.2_steepness0.80_ess5_llq1.00_growmatfarley_cpuejappt_scalingbiomass_llselLogistic_colllsel_iter17")
SSplotComps(essLog,subplot=21)
