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

load("om/out/metrics.RData")

SB0_low <- 159445 
SB0_high <- 277605

# SUBSET
# 
# idx <- results$Convergence_Level < 0.001
# 
# results <- results[idx,]

# CONVERT to factor

results[, (colnames(results)[1:9]) := lapply(.SD, factor), .SDcols = colnames(results)[1:9]]

results[M==999, M:="lorenzen"]
results[, M:=as.factor(as.character(M))]

#Creating status variables
results[,c("ratio","SSB15SSB0","SSB15SSBMSY"):=list(SSB_MSY/SSB_Virgin,SSB_endyr/SSB_Virgin,SSB_endyr/SSB_MSY)]

# save(results, file="om/out/filter_results.RData", compress="xz")

#identifying runs with SB0 > 450000 and SBcurrent/SB0 > 0.61
# results[, SBvirgin_high:=ifelse(results$SSB_Virgin>450000&results$SSB15SSB0>0.61,1,2)]
# write.table(test,"test_SSBB0.csv",sep=";", dec=".")

#INSPECTION PLOTS

# SSB_Virgin vs SSB_endyr/SSB_Virgin
ggplot(results, aes(SSB_Virgin,SSB15SSB0)) + 
  geom_point(col="darkblue")+
  xlab("SB0")+
  ylab("SBcurrent/SB0")+
  theme_bw()+
  theme(legend.position="none")

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

# dist(SSB_Virgin)

ggplot(results, aes(SSB_Virgin)) + geom_density(fill="grey90")+
  geom_vline(xintercept=SB0_low, colour='red') +
  geom_vline(xintercept=SB0_high, colour='red') +
  xlab("SB0")+
  ylab("Density")+
  theme_bw()

# dist(SSB_Virgin) by relevant factor

ggplot(results, aes(SSB_Virgin)) + 
  geom_density(aes(fill=factor(cpue)),alpha=0.3)+
  facet_grid(llsel~scaling, scales="free_y") +
  xlab("SB0")+
  ylab("Density")+
  theme_bw()+
  guides(fill=guide_legend("CPUE"))


# dist(SSB_endyr/SSB_Virgin)

ggplot(results, aes(x=SSB15SSB0)) +
  geom_density(fill="grey90")+
  xlab("SBcurrent/SB0")+
  ylab("Density")+
  theme_bw()

# dist(SSB_endyr/SSB_Virgin) by relevant factor

ggplot(results, aes(x=SSB15SSB0)) +
  geom_density(aes(fill=factor(cpue)), alpha=0.3)+
  facet_grid(~scaling)+#, scales="free_y") +
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

# dist(SSB_endyr/SSB_MSY)

ggplot(results, aes(x=SSB15SSBMSY)) +
  geom_density(fill="grey90")+
  xlab("SBcurrent/SBMSY")+
  ylab("Density")+
  theme_bw()

# dist(SSB_endyr/SSB_MSY) by relevant factor

ggplot(results, aes(x=SSB15SSBMSY)) +
  geom_density(aes(fill=factor(cpue)), alpha=0.3)+
  facet_grid(steepness~scaling)+#, scales="free_y") +
  xlab("SBcurrent/SBMSY")+
  ylab("Density")+
  guides(fill=guide_legend("CPUE"))+
  theme_bw()

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


###PLOT S-R residuals

SR_plot <- plot(residuals$sr)+geom_hline(yintercept=0, col="red")+theme_bw()


###PLOT CPUE residuals
CPUE_plot <- plot(residuals$indices)


###PLOTTING OM

metrics$F<- areaSums(metrics$F * metrics$B) / areaSums(metrics$B)
metrics$REC <- areaSums(metrics$REC)
metrics$SSB <- areaSums(metrics$SSB)
metrics$C <- areaSums(metrics$C)


OM_plot <- plot(metrics[1:4])+geom_line(col="black")+ facet_grid(qname~unit, scales="free_y")

#PLOT SA
load("sa.RData")
name(sa) <- "SA"
range(sa, c("minfbar", "maxfbar")) <- c(2,8)
plot(sa)+facet_grid(qname~unit, scales="free_y")


## PLOT OM+SA
load("om.RData")
name(stock(om)) <- "OM"

plot(sa,stock(om)[,ac(1950:2015)])+facet_grid(qname~unit, scales="free_y")+theme_bw()

### INSPECTING SUBSET OF RUNS RELATIVE TO ALL OM

# pdf(file="SUBSETvsOM.pdf")

res_sample <- rbind(results, results[results$sample==T,])
res_sample [, subset:=c(rep("Total",dim(results)[1]),rep("Subset",dim(results[results$sample==T,])[1]))]


# SSB_Virgin vs SSB_endyr/SSB_Virgin
# tiff(file="SB0_SB15_SB0.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
ggplot(res_sample, aes(SSB_Virgin,SSB15SSB0)) + 
  geom_point(col="darkblue")+
  facet_grid(~subset)+
  xlab("SB0")+
  ylab("SBcurrent/SB0")+
  theme_bw()+
  theme(legend.position="none")
#dev.off()


# dist(SSB_Virgin)
# tiff(file="SB0_dist.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
ggplot(res_sample, aes(SSB_Virgin)) + geom_density(fill="grey90")+
  geom_vline(xintercept=SB0_low, colour='red') +
  geom_vline(xintercept=SB0_high, colour='red') +
  facet_grid(~subset)+
  xlab("SB0")+
  ylab("Density")+
  theme_bw()
#dev.off()

# dist(SSB_endyr/SSB_Virgin)

# tiff(file="dist_SB15_SB0.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
ggplot(res_sample, aes(x=SSB15SSB0)) +
  geom_density(fill="grey90")+
  facet_grid(~subset)+
  xlab("SBcurrent/SB0")+
  ylab("Density")+
  theme_bw()
#dev.off()

#dist(SSB_endyr/SSB_MSY)
# tiff(file="dist_SB15_SBMSY.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
ggplot(res_sample, aes(x=SSB15SSBMSY)) +
  geom_density(fill="grey90")+
  facet_grid(~subset)+
  xlab("SBcurrent/SBMSY")+
  ylab("Density")+
  theme_bw()
#dev.off()

###PLOT S-R residuals

#subsetting by sampled suns
resid_samp_sr <- iter(residuals$sr, results[results$sample==TRUE,]$iter)

#plotting subset
SR_plot2 <- plot(resid_samp_sr)+geom_hline(yintercept=0, col="red")+theme_bw()


###PLOT CPUE residuals
resid_ind_samp <- iter(residuals$indices, results[results$sample==TRUE,]$iter)
CPUE_plot2 <- plot(resid_ind_samp)


###PLOTTING OM

metrics_sample <- iter(metrics, results[results$sample==TRUE,]$iter)
#no need to redo the areaSums as this metrics is already summed by area

OM_plot2 <- plot(metrics_sample[1:4])+geom_line(col="black")+facet_grid(qname~unit, scales="free_y")

# tiff(file="SR-resid_compare.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
grid.arrange(SR_plot, SR_plot2,ncol=2,nrow=1)
#dev.off()

# tiff(file="CPUE-resid_compare.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
grid.arrange(CPUE_plot, CPUE_plot2,ncol=2,nrow=1)
#dev.off()

# tiff(file="OM_compare.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
grid.arrange(OM_plot, OM_plot2,ncol=2,nrow=1)
#dev.off()


####IDENTIFYING DRIVERS FOR CLUSTERS

tot <-lapply(c(1:5), function(x) lapply(results[cl==x,1:9],length)) 
clst <- lapply(c(1:5), function(x) lapply(results[cl==x,1:9],table))

fin<-mapply(function(x,y) mapply(function(a,b) a/b, a=x,b=y), x=clst, y=tot, SIMPLIFY=FALSE)

final <- as.data.frame(do.call(rbind, lapply(fin, Reduce, f=c)))
final$clst <- rownames(final)
colnames(final)[c(1,4)] <- c("0.20","0.60")
final <- as.data.table(melt(final, id.vars="clst"))
final[,Factor:=rep(colnames(results)[1:9],c(15,15,10,10,10,10,15,15,10))]
final[,Factor:= factor(Factor, levels=c("M", "steepness","scaling", "cpue",
                                        "sigmaR","growmat","ess","llq","llsel"))]
dummy <- data.frame(Factor = c("M", "steepness","scaling", "cpue",
                          "sigmaR","growmat","ess","llq","llsel"), 
                    Z = c(rep(c(0.33,0.5), c(4,5))))
# tiff(file="Prop_byCluster.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
ggplot(data=final,aes(variable, value))+
  geom_point()+
  facet_grid(clst~Factor, scale="free")+
  xlab("Levels")+
  ylab("Proportion")+
  ylim(c(0,1))+
  geom_hline(data = dummy, aes(yintercept = Z),col="red")+
  theme_bw()
#dev.off()

# tiff(file="Factors_byCluster.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
ggplot(data=final,aes(variable, value, fill=Factor))+
  geom_bar(stat = "identity", position="dodge")+
  facet_grid(clst~Factor,scale="free")+
  xlab("Levels")+
  ylab("Proportion")+
  ylim(c(0,1))+
  theme_bw()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, hjust = 1))

#dev.off for overall pdf saving
#dev.off()


#IDENTIFYING FACTORS AND FACTOR LEVELS IN THE SUBSETTED OM

res_sub <- results[sample==T,]
clres <- lapply(c(1:5), function(x) lapply(res_sub[cl==x,1:9],table))

clres <- as.data.frame(do.call(rbind, lapply(clres, Reduce, f=c)))
clres$clst <- rownames(clres)
colnames(clres)[c(1,4)] <- c("0.20","0.60")
clres <- as.data.table(melt(clres, id.vars="clst"))
clres[,Factor:=rep(colnames(res_sub)[1:9],c(15,15,10,10,10,10,15,15,10))]
clres[,Factor:= factor(Factor, levels=c("M", "steepness","scaling", "cpue",
                                        "sigmaR","growmat","ess","llq","llsel"))]

# tiff(file="Factors_byCluster.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 600)
ggplot(data=clres,aes(variable, value, fill=Factor))+
  stat_summary(fun.y = "sum", geom = "bar", position = "identity")+
  facet_wrap(Factor~.,scale="free")+
  xlab("Levels")+
  ylab("Frequency")+
  ylim(c(0,300))+
  theme_bw()+
  theme(legend.position="none",
        axis.text.x = element_text(angle = 90, hjust = 1))
#dev.off()


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
