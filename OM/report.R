
library(ggplotFL)
library(patchwork)

# data

# model_base

# model_maineffects

# --- model_partial.R {{{

load("model/partial/...")

# Geff ~ nTrials w/ N=108
ggsave("report/ntrials.png",
ggplot(evd, aes(x=as.numeric(nTrials), y=Geff)) + geom_point()+
  ylim(c(0.75, NA)) + geom_vline(xintercept=108, colour="red")
)

# unrealistic values (SSB_Virgin > 4.5e5 t, SSB_status > 3)

ggsave("report/unrealvalues.png",
(ggplot(results, aes(SSB_Virgin)) +
  geom_histogram(fill="grey90", color="black") + theme_bw() +
  geom_vline(xintercept=4.5e5, colour='red') +
  xlab(expression(SB[0] (t))) + ylab("") +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank())) +

(ggplot(results, aes(SSB_status)) +
  geom_histogram(fill="grey90", color="black") + theme_bw() +
  geom_vline(xintercept=3, colour='red') +
  xlab(expression(SB[2018] / SB[MSY])) + ylab("") +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank()))
)

# convergence level
ggsave("report/convergence.png",
ggplot(results, aes(Convergence_Level)) +
  geom_histogram(fill="grey90", color="black") + theme_bw() +
  geom_vline(xintercept=1e-3, colour='red') +
  xlab("Convergence (final gradient)") + ylab("") +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank())
)

# mrho
ggsave("report/mrho.png",
ggplot(results, aes(mrho)) +
  geom_histogram(fill="grey90", color="black") + theme_bw() +
  geom_vline(xintercept=0.20, colour='red') +
  xlab("Mohn's rho (SB)") + ylab("") +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank())
)

# MASE JAN NW CPUE
ggsave("report/mase.png",
ggplot(results, aes(mase)) +
  geom_histogram(fill="grey90", color="black") + theme_bw() +
  geom_vline(xintercept=1, colour='red') +
  xlab("MASE(NW JAP LL CPUE)") + ylab("") +
  theme(axis.text.y = element_blank(),
  axis.ticks.y = element_blank())
)


# SRRs

png(file="report/srr%03d.png", width=300, heigh=300)
  for (i in seq(1, 70)){
    print(plot(iter(sr, i)))
  }
dev.off()

system("convert -delay 40 report/srr*.png report/srrs.gif")

# }}}







# --- SORT

load("om/data/gridIO4.Rdata")
results_sa <- sa$results
stock_sa <- simplify(sa$stock, "area")
range(stock_sa) <- c(minfbar=2, maxfbar=8)

rangeSSB <- range(results_sa$SSB_MSY)
rangeStatus <- range(results_sa$SSB_status)


load("om/model/partial_areas.Rdata")

labels <- c("M=0.2","M=0.3", "M=Lorenzen", "h=0.6","h=0.75", "h=0.9", "sigmaR=0.6", "ess=2","ess=20",
            "llq=1.00","llq=1.01","growmat=Farley", "growmat=Wang", 
            "cpue=jpnpt","cpue=jpn","cpue=twnpt", "scaling=area", "scaling=catch", "scaling=region", "llsel=Logistic","llsel=DoNorm")

#PLOT SSB2018

ggplot(results, aes(SSB_endyr)) + geom_density(fill="grey90")+
  geom_vline(xintercept=min(results_sa$SSB_endyr), colour='red') +
  geom_vline(xintercept=max(results_sa$SSB_endyr), colour='red') +
  xlab("SB0")+
  ylab("Density")+
  theme_bw()


#PLOT SSB_Virgin

# tiff(file="om/Figures/distSB0.tiff", bg = "white", compression="lzw",width = 8,
#      height = 5, units = "cm", res = 300)
ggplot(results, aes(SSB_Virgin)) + geom_density(fill="grey90")+
  geom_vline(xintercept=min(results_sa$SSB_Virgin), colour='red') +
  geom_vline(xintercept=max(results_sa$SSB_Virgin), colour='red') +
  xlab(expression(SB[0]))+
  ylab("Density")+
  theme_bw()
#dev.off()

#PLOT SSB_Status

# tiff(file="om/Figures/distSBstatus.tiff", bg = "white", compression="lzw",width = 8,
#      height = 5, units = "cm", res = 300)
ggplot(results, aes(SSB_status)) + geom_density(fill="grey90")+
  geom_vline(xintercept=min(results_sa$SSB_status), colour='red') +
  geom_vline(xintercept=max(results_sa$SSB_status), colour='red') +
  xlab(expression(SB[2018]/SB[MSY]))+
  ylab("Density")+
  theme_bw()
#dev.off()

#PLOT SSB_depletion
ggplot(results, aes(SSB_depletion)) + geom_density(fill="grey90")+
  geom_vline(xintercept=min(results_sa$SSB_depletion), colour='red') +
  geom_vline(xintercept=max(results_sa$SSB_depletion), colour='red') +
  xlab("Depletion")+
  ylab("Density")+
  theme_bw()

# tiff(file="om/Figures/SBMSY_factor.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 300)
(ggplot(results, aes(y=SSB_MSY)) +
    geom_boxplot(fill="white") + 
    geom_hline(yintercept=rangeSSB, linetype=2, alpha=0.6) +
    xlab("All") + ylab(expression(SB[MSY])))+
(ggplot(results, aes(x=factor(M), y=SSB_MSY)) +
   geom_boxplot(fill="red") + xlab("M") + ylab("") +
   geom_hline(yintercept=rangeSSB, linetype=2, alpha=0.6) +
   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
(ggplot(results, aes(x=factor(steepness), y=SSB_MSY)) +
   geom_boxplot(fill="green") + xlab("steepness") + ylab("") +
   geom_hline(yintercept=rangeSSB, linetype=2, alpha=0.6) +
   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(sigmaR), y=SSB_MSY)) +
     geom_boxplot(fill="gray") + xlab("sigmaR") + ylab("") +
     geom_hline(yintercept=rangeSSB, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(ess), y=SSB_MSY)) +
     geom_boxplot(fill="cyan") + xlab("ESS") + ylab("") +
     geom_hline(yintercept=rangeSSB, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(llq), y=SSB_MSY)) +
     geom_boxplot(fill="orange") + xlab("LLQ") + ylab("") +
     geom_hline(yintercept=rangeSSB, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(growmat), y=SSB_MSY)) +
     geom_boxplot(fill="yellow") + xlab("Growmat") + ylab("") +
     geom_hline(yintercept=rangeSSB, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
   (ggplot(results, aes(x=factor(cpue), y=SSB_MSY)) +
     geom_boxplot(fill="pink") + xlab("cpues") + ylab("") +
     geom_hline(yintercept=rangeSSB, linetype=2, alpha=0.6) +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(scaling), y=SSB_MSY)) +
     geom_boxplot(fill="aquamarine") + xlab("Scaling") + ylab("") +
     geom_hline(yintercept=rangeSSB, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(llsel), y=SSB_MSY)) +
     geom_boxplot(fill="purple") + xlab("Sel") + ylab("") +
     geom_hline(yintercept=rangeSSB, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm")))+
  plot_layout(nrow = 1, byrow = FALSE)
# dev.off()

# tiff(file="om/Figures/SBstatus_factor.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 300)
(ggplot(results, aes(y=SSB_status)) +
    geom_boxplot(fill="white") + 
    geom_hline(yintercept=rangeStatus, linetype=2, alpha=0.6) +
    xlab("All") + ylab(expression(SB[2018]/SB[MSY])))+
  (ggplot(results, aes(x=factor(M), y=SSB_status)) +
     geom_boxplot(fill="red") + xlab("M") + ylab("") +
     geom_hline(yintercept=rangeStatus, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(steepness), y=SSB_status)) +
     geom_boxplot(fill="green") + xlab("steepness") + ylab("") +
     geom_hline(yintercept=rangeStatus, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(sigmaR), y=SSB_status)) +
     geom_boxplot(fill="gray") + xlab("sigmaR") + ylab("") +
     geom_hline(yintercept=rangeStatus, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(ess), y=SSB_status)) +
     geom_boxplot(fill="cyan") + xlab("ESS") + ylab("") +
     geom_hline(yintercept=rangeStatus, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(llq), y=SSB_status)) +
     geom_boxplot(fill="orange") + xlab("LLQ") + ylab("") +
     geom_hline(yintercept=rangeStatus, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(growmat), y=SSB_status)) +
     geom_boxplot(fill="yellow") + xlab("Growmat") + ylab("") +
     geom_hline(yintercept=rangeStatus, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(cpue), y=SSB_status)) +
     geom_boxplot(fill="pink") + xlab("cpues") + ylab("") +
     geom_hline(yintercept=rangeStatus, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(scaling), y=SSB_status)) +
     geom_boxplot(fill="aquamarine") + xlab("Scaling") + ylab("") +
     geom_hline(yintercept=rangeStatus, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm"))) +
  (ggplot(results, aes(x=factor(llsel), y=SSB_status)) +
     geom_boxplot(fill="purple") + xlab("Sel") + ylab("") +
     geom_hline(yintercept=rangeStatus, linetype=2, alpha=0.6) +
     theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
           plot.margin = unit(c(0, 0, 0, 0), "cm")))+
  plot_layout(nrow = 1, byrow = FALSE)
# dev.off()

# tiff(file="om/Figures/om.tiff", bg = "white", compression="lzw",width = 32,
#      height = 20, units = "cm", res = 300)
plot(stock)
# dev.off()

stocks <- FLStocks(sa=stock_sa, om=stock)

# tiff(file="om/Figures/omsa.tiff", bg = "white", compression="lzw",width = 32,
     # height = 20, units = "cm", res = 300)
plot(stocks)
# dev.off()
