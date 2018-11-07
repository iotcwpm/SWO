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

load("out/resultsALL.RData")

# SUBSET

idx <- results$Convergence_Level < 0.001

results <- results[idx,]


# dist(SSB_Virgin)
ggplot(results, aes(SSB_Virgin)) + geom_density(aes(fill=factor(M)))

ggplot(results, aes(SSB_Virgin)) + geom_density(aes(fill=factor(cpue)))+
  facet_grid(ess~llsel+scaling, scales="free_y") +
  xlab("SBVirgin")+
  ylab("Density")+
  guides(fill=guide_legend("CPUE"))


# dist(SSB_MSY/SSB_Virgin)

# dist(SSB_MSY/SSB_Virgin) by relevant factor

results[, ratio:=SSB_MSY/SSB_Virgin]

ggplot(results, aes(x=ratio)) +
  geom_density(aes(fill=factor(steepness))) +
  facet_grid(M~growmat, scales="free_y") +
  xlab("SBMSY/SBVirgin")+
  ylab("Density")+
  guides(fill=guide_legend("Steepness"))


# dist(SSB_endyr/SSB_Virgin=depletion) by relevant factor

results[, depletion:=SSB_endyr/SSB_Virgin]

ggplot(results, aes(x=depletion)) +
  geom_density(aes(fill=factor(cpue)), alpha=0.3)+
  facet_grid(ess~scaling)+#, scales="free_y") +
  xlab("Depletion")+
  ylab("Density")+
  guides(fill=guide_legend("CPUE"))

ggplot(results, aes(x=depletion)) +
  geom_density(aes(fill=factor(ess)))

#  SUBSET by SSB_Virgin and Depletion
results[, SBvirgin_high:=ifelse(results$SSB_Virgin>450000&results$depletion>0.61,1,2)]
plot(results$SSB_Virgin, results$depletion, col=results$SBvirgin_high)
ggplot(results, aes(SSB_Virgin,depletion)) + geom_point()+
  theme_bw()

# write.table(test,"test_SSBB0.csv",sep=";", dec=".")

idx2 <- results$SSB_Virgin<450000&results$depletion<0.61
  
results <- results[idx2,]


# EXPLORE 

# CONVERT to factor
results[, (colnames(results)[1:9]) := lapply(.SD, factor), .SDcols = colnames(results)[1:9]]

results[, F15FMSY:=F_endyr/Fstd_MSY]
results[, SB15SBMSY:=SSB_endyr/SSB_MSY]
results[, SB15SBVirgin:=SSB_endyr/SSB_Virgin]

# PLOTS
# pdf(file="status_plots.pdf")


for (i in colnames(results)[1:9]) {
  
  print(ggplot(results, aes_string(x=i, y="SSB_Virgin")) + geom_boxplot())
    
  
  print(ggplot(results, aes_string(x=i, y="SSB15SSBMSY")) + geom_boxplot())
  
  print(ggplot(results, aes_string(x=i, y="SSB15SSBVirgin")) + geom_boxplot())
  
  print(ggplot(results, aes_string(x=i, y="F15FMSY")) + geom_boxplot())
}

# dev.off()


# test<- melt(results, id = "SSB_Virgin", measure = c("M","steepness"))

p1 <-  print(ggplot(results, aes_string(x="M", y="SSB_Virgin")) + geom_boxplot()+scale_y_continuous(labels = scales::comma))
p2 <-  print(ggplot(results, aes_string(x="steepness", y="SSB_Virgin")) + geom_boxplot())
p3 <-  print(ggplot(results, aes_string(x="llq", y="SSB_Virgin")) + geom_boxplot())
p4 <-  print(ggplot(results, aes_string(x="llsel", y="SSB_Virgin")) + geom_boxplot())
p5 <-  print(ggplot(results, aes_string(x="cpue", y="SSB_Virgin")) + geom_boxplot())
p6 <-  print(ggplot(results, aes_string(x="scaling", y="SSB_Virgin")) + geom_boxplot())
p7 <-  print(ggplot(results, aes_string(x="sigmaR", y="SSB_Virgin")) + geom_boxplot())
p8 <-  print(ggplot(results, aes_string(x="ess", y="SSB_Virgin")) + geom_boxplot())
p9 <-  print(ggplot(results, aes_string(x="growmat", y="SSB_Virgin")) + geom_boxplot())

library(grid)

grid.newpage()

# tiff(file="status_plots_SSBMSY.tiff", bg = "white", compression="lzw",width = 32, 
#      height = 20, units = "cm", res = 600)
pushViewport(viewport(layout = grid.layout(3, 3)))

vplayout <- function(x, y)
  
  viewport(layout.pos.row = x, layout.pos.col = y)

print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(1, 2))
print(p3, vp = vplayout(1, 3))
print(p4, vp = vplayout(2, 1))
print(p5, vp = vplayout(2, 2))
print(p6, vp = vplayout(2, 3))
print(p7, vp = vplayout(3, 1))
print(p8, vp = vplayout(3, 2))
print(p9, vp = vplayout(3, 3))

# dev.off()

ggplot(results, aes(y=ratio, x=steepness)) +
  geom_boxplot() +
  xlab("Steepness")+
  ylab("SBMSY/SBVirgin")+
  theme_bw()


ggplot(results, aes(SSB_Virgin)) + geom_density(aes(fill=factor(M)))

ggplot(results, aes(SSB_Virgin)) + geom_density(aes(fill=factor(cpue)), alpha=0.3)+
  facet_grid(llsel~scaling, scales="free_y") +
  xlab("SBVirgin")+
  ylab("Density")+
  guides(fill=guide_legend("CPUE"))



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

