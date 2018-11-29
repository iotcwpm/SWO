# inspect.R - DESC
# /inspect.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# XX {{{
# }}}

library(ggplot2)
library(data.table)
library(gridExtra)
library(ioswomse)
library(ss3om)

# setwd("C:/Users/USER/Desktop/SWO-MSE/Git-SWO/SWO/om")

load("om/out/resultsALL.RData")

SB0_low <- 159445 
SB0_high <- 277605

# SUBSET

idx <- results$Convergence_Level < 0.001

results <- results[idx,]

# CONVERT to factor

results[, (colnames(results)[1:9]) := lapply(.SD, factor), .SDcols = colnames(results)[1:9]]

results[M==999, M:="lorenzen"]


#Creating status variables
results[,c("ratio","SSB15SSB0","SSB15SSBMSY"):=list(SSB_MSY/SSB_Virgin,SSB_endyr/SSB_Virgin,SSB_endyr/SSB_MSY)]


#identifying runs with SB0 > 450000 and SBcurrent/SB0 > 0.61
results[, SBvirgin_high:=ifelse(results$SSB_Virgin>450000&results$SSB15SSB0>0.61,1,2)]
# write.table(test,"test_SSBB0.csv",sep=";", dec=".")

#INSPECTION PLOTS

# dist(SSB_Virgin)

ggplot(results, aes(SSB_Virgin)) + geom_density(fill="grey90")+
  geom_vline(xintercept=SB0_low, colour='red') +
  geom_vline(xintercept=SB0_high, colour='red') +
  xlab("SB0")+
  ylab("Density")+
  theme_bw()

# dist(SSB_Virgin) by relevant factor

ggplot(results, aes(SSB_Virgin)) + geom_density(aes(fill=factor(cpue)))+
  facet_grid(ess~llsel+scaling, scales="free_y") +
  xlab("SB0")+
  ylab("Density")+
  theme_bw()+
  guides(fill=guide_legend("CPUE"))


# dist(SSB_MSY/SSB_Virgin)

ggplot(results, aes(x=ratio)) +
  geom_density()

# dist(SSB_MSY/SSB_Virgin) by relevant factor

ggplot(results, aes(x=ratio)) +
  geom_density(aes(fill=factor(steepness))) +
  facet_grid(M~growmat, scales="free_y") +
  xlab("SBMSY/SB0")+
  ylab("Density")+
  theme_bw()+
  guides(fill=guide_legend("Steepness"))


# dist(SSB_endyr/SSB_Virgin)

ggplot(results, aes(x=SSB15SSB0)) +
  geom_density(fill="grey90")+
  xlab("SBcurrent/SB0")+
  ylab("Density")+
  theme_bw()

# dist(SSB_endyr/SSB_Virgin) by relevant factor

ggplot(results, aes(x=SSB15SSB0)) +
  geom_density(aes(fill=factor(cpue)), alpha=0.3)+
  facet_grid(ess~scaling)+#, scales="free_y") +
  xlab("SBcurrent/SB0")+
  ylab("Density")+
  guides(fill=guide_legend("CPUE"))+
  theme_bw()

ggplot(results, aes(x=SSB15SSB0)) +
  geom_density(aes(fill=factor(cpue)), alpha=0.3)+
  facet_grid(ess~llsel+scaling)+#, scales="free_y") +
  xlab("SBcurrent/SB0")+
  ylab("Density")+
  guides(fill=guide_legend("CPUE"))+
  theme_bw()

#INSPECT results with high SSB_Virgin and high SSB_endyr/SSB_Virgin

# SSB_Virgin vs SSB_endyr/SSB_Virgin

ggplot(results, aes(SSB_Virgin,SSB15SSB0)) + 
  geom_point(aes(col=SBvirgin_high))+
  xlab("SB0")+
  ylab("SBcurrent/SB0")+
  theme_bw()+
  theme(legend.position="none")


#  SUBSETTING results by SSB_Virgin and SSB_endyr/SSB_Virgin
  
idx <- results$SSB_Virgin < 450000 & results$SSB15SSB0 < 0.61
results <- results[idx,]


# EXPLORE 


# PLOTS

plots1 <-list()
plots2 <-list()
plots3 <-list()
plots4 <-list()

# pdf(file="status_plots.pdf")
for (i in colnames(results)[1:9]) {

 plots1[[i]]<- ggplot(results, aes_string(x=i, y="SSB_Virgin")) + geom_boxplot()+
               ylab("SB0")
    
 plots2[[i]] <- ggplot(results, aes_string(x=i, y="SSB15SSBMSY")) + geom_boxplot()+
                 ylab("SBcurrent/SBMSY")
  
 plots3[[i]] <- ggplot(results, aes_string(x=i, y="SSB15SSB0")) + geom_boxplot()+
                 ylab("SBcurrent/SB0")
  
 plots4[[i]]<- ggplot(results, aes_string(x=i, y="ratio")) + geom_boxplot()+
                ylab("SBMSY/SB0")
 
}

# tiff(file="boxplot_SSBVirgin.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
grid.arrange(plots1$M,plots1$steepness,plots1$sigmaR,plots1$ess,plots1$llq,
             plots1$growmat,plots1$cpue,plots1$scaling,plots1$llsel,ncol=3,nrow=3)
#dev.off()

# tiff(file="boxplot_SSBcurr_SSBMSY.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
grid.arrange(plots2$M,plots2$steepness,plots2$sigmaR,plots2$ess,plots2$llq,
             plots2$growmat,plots2$cpue,plots2$scaling,plots2$llsel,ncol=3,nrow=3)
#dev.off()

# tiff(file="boxplot_plots_SSBcurr_SSB0.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
grid.arrange(plots3$M,plots3$steepness,plots3$sigmaR,plots3$ess,plots3$llq,
             plots3$growmat,plots3$cpue,plots3$scaling,plots3$llsel,ncol=3,nrow=3)
#dev.off()

# tiff(file="boxplot_SSBMSY_SSB0.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
grid.arrange(plots4$M,plots4$steepness,plots4$sigmaR,plots4$ess,plots4$llq,
             plots4$growmat,plots4$cpue,plots4$scaling,plots4$llsel,ncol=3,nrow=3)
#dev.off()


# dist(SSB_MSY/SSB_Virgin) by relevant factor after subsettiing

ggplot(results, aes(x=ratio)) +
  geom_density(aes(fill=factor(steepness))) +
  facet_grid(M~growmat, scales="free_y") +
  xlab("SBMSY/SB0")+
  ylab("Density")+
  theme_bw()+
  guides(fill=guide_legend("Steepness"))


ggplot(results, aes(y=ratio, x=steepness)) +
  geom_boxplot() +
  xlab("Steepness")+
  ylab("SBMSY/SBVirgin")+
  theme_bw()

# dist(SSB_Virgin) by relevant factor after subsettiing
ggplot(results, aes(SSB_Virgin)) + geom_density(aes(fill=factor(M)))

ggplot(results, aes(SSB_Virgin)) + geom_density(aes(fill=factor(cpue)), alpha=0.3)+
  facet_grid(~scaling, scales="free_y") +
  xlab("SB0")+
  ylab("Density")+
  guides(fill=guide_legend("CPUE"))

# dist(SSB_endyr/SSB_Virgin) by relevant factor after subsettiing
ggplot(results, aes(SSB15SSB0)) + geom_density(aes(fill=factor(cpue)), alpha=0.3)+
  facet_grid(~scaling, scales="free_y") +
  xlab("SBcurrent/SB0")+
  ylab("Density")+
  guides(fill=guide_legend("CPUE"))


# dist(SSB_MSY/SSB_Virgin) by relevant factor after subsettiing
ggplot(results, aes(ratio)) + geom_density(aes(fill=steepness), alpha=0.3)+
  facet_grid(~M, scales="free_y") +
  xlab("SBMSY/SB0")+
  ylab("Density")+
  guides(fill=guide_legend("Steepness"))


#Loading residuals
load("om/out/metrics.RData")

###PLOT S-R residuals

plot(residuals$sr)+geom_hline(yintercept=0, col="red")+theme_bw()


###PLOT CPUE residuals
plot(residuals$indices)


###PLOTTING OM

metrics$F <- areaSums(metrics$F * metrics$B) / areaSums(metrics$B)
metrics$REC <- areaSums(metrics$REC)
metrics$SSB <- areaSums(metrics$SSB)
metrics$C <- areaSums(metrics$C)


plot(metrics[1:4])+geom_line(col="black")+facet_grid(qname~unit, scales="free_y")

#PLOT SA

sa <- readFLSss3("ioswomse/data-raw/sa", repfile="Report.sso.gz", covarfile="covar.sso.gz",
                 compfile="CompReport.sso.gz")
catch.sa <- areaSums(catch(sa))
sa <- simplify(sa, c("area"))
catch(sa) <- catch.sa
plot(sa)+facet_grid(qname~unit, scales="free_y")

# save(sa, file="om/out/sa.RData", compress="xz")


#OTHER PLOTS

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

# ---

Are you able to check why the residuals 2000-2015 in "indices$UTWLL_SW" are not NAs?


