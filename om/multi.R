# multi.R - DESC
# /multi.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu> & Rui Coelho (IPMA)
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# XX {{{
# }}}

library(ggplot2)
library(data.table)
library(corrplot)
library(Hmisc)
library(vegan)
library(dplyr)
library(grid)


load("om/out/resultsALL.RData")
head(results)

# SUBSET by converge level
idx <- results$Convergence_Level < 0.001
results <- results[idx,]

results$SSB15SSB0 <- results$SSB_endyr/results$SSB_Virgin
# 
# response variables to consider: SSB_Virgin; SSB_MSY; SSB_endyr; TotYield_MSY
resp <- results[, c("SSB_Virgin", "SSB_MSY", "SSB_endyr", "TotYield_MSY")]
# predictor variables
pred <- results[, c("M", "steepness", "sigmaR", "ess", "llq", "growmat", "cpue", "scaling", "llsel")]
#all predictors as categorical
pred$M <- factor(pred$M)
pred$steepness <- factor(pred$steepness)
pred$steepness <- factor(pred$steepness)
pred$sigmaR <- factor(pred$sigmaR)
pred$ess <- factor(pred$ess)
pred$llq <- factor(pred$llq)
pred$growmat <- factor(pred$growmat)
pred$cpue <- factor(pred$cpue)
pred$scaling <- factor(pred$scaling)
pred$llsel <- factor(pred$llsel)
summary(pred)


## Correlation values
cor <- cor(resp); round(cor,3)

#correlation plots
corrplot(cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#calculate coefs and p-values
cor2 <- rcorr(as.matrix(resp)); cor2
# Extract the cor coefs and p-values 
cor2$r; cor2$P

####
#NMSD - takes some minutes...
dist <- vegdist(resp)
mds <- monoMDS(dist, k = 3, model = "loc", maxit=500)
mds
plot(mds)


####
# DENDOGRAM
dim(pred)
dim(resp)
resp.dist <- vegdist(resp)
csin <- hclust(resp.dist, method="average")
plot(csin, hang=-1)

#make 4 clurster as an example
rect.hclust(csin, 4)
cl <- cutree(csin, 4)
dim(results)

#Add variable cluster ID to the results table
results$cl <- factor(cl)
head(results)
summary(results$cl) # Number of models in each cluster

#save plot
# png(file="reports/IOTC-2018-SC21-XX/figures/Dendogram.png", bg = "white", #compression="lzw",
#      width = 24, height = 16, units = "cm", res = 600)
plot(csin, hang=-1)
rect.hclust(csin, 4)
# dev.off()


#####
## make summaries for each cluster
summaries <- results %>%
  select(SSB_Virgin, TotYield_MSY, SSB15SSB0) %>%
  group_by(factor(cl)) %>%
  summarise(SSB_Virgin = mean(SSB_Virgin), TotYield_MSY = mean(TotYield_MSY), SSB15SSB0 = mean(SSB15SSB0))
summaries

###
## Plots from the clusters 

cl.data <- melt(results, id="cl",measure=c("SSB_Virgin", "TotYield_MSY","SSB15SSB0"))
labels <- c(SSB_Virgin="SB0",TotYield_MSY="BMSY",SSB15SSB0="SBcurr/SB0")

cl.boxplot<- ggplot(cl.data, aes(factor(cl), value))+
  geom_boxplot(aes(fill=factor(cl)))+
  facet_grid(variable~., scales="free_y", labeller=labeller(variable=labels))+
  theme_bw()+
  theme(legend.position="none",
        axis.title = element_blank())
# png(file="reports/IOTC-2018-SC21-XX/figures/Clustering_quantities.png", bg = "white", #compression="lzw",
#      width = 24, height = 24, units = "cm", res = 600)
cl.boxplot
# dev.off()


# Virgin SSB
cl.virgin <-ggplot(results, aes(factor(cl), SSB_Virgin))+
  geom_boxplot(aes(fill=factor(cl)))+
  ggtitle("SSB_Virgin")+
  theme_bw()
cl.virgin

# MSY
cl.msy <-ggplot(results, aes(factor(cl), TotYield_MSY))+
  geom_boxplot(aes(fill=factor(cl)))+
  ggtitle("TotYield_MSY")+
  theme_bw()
cl.msy

# SSB15SSB0
cl.SSB15SSB0 <-ggplot(results, aes(factor(cl), SSB15SSB0))+
  geom_boxplot(aes(fill=factor(cl)))+
  ggtitle("SSB15SSB0")+
  theme_bw()
cl.SSB15SSB0

#make the plots
# tiff(file="Clustering_quantities.tiff", bg = "white", compression="lzw",
#      width = 24, height = 24, units = "cm", res = 600)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 1)))
vplayout <- function(x, y)
  viewport(layout.pos.row = x, layout.pos.col = y)
print(cl.virgin, vp = vplayout(1, 1))
print(cl.msy, vp = vplayout(2, 1))
print(cl.SSB15SSB0, vp = vplayout(3, 1))
# dev.off()



