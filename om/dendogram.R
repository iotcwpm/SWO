# multi.R - DESC
# /multi.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu> & Rui Coelho (IPMA)
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(ggplot2)
library(data.table)
# install.packages("corrplot")
library(corrplot)
# install.packages("Hmisc")
library(Hmisc)
# install.packages("vegan")
library(vegan)
# install.packages("dplyr")
library(dplyr)
library(grid)


load("om/out/metrics.RData")
head(results)

# SUBSET by converge level
# idx <- results$Convergence_Level < 0.001
# results <- results[idx,]

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

# resp$SSB_Virgin <-log(resp$SSB_Virgin)

resp_scaled <- scale(resp)
resp.dist <- dist(resp_scaled)
csin <- hclust(resp.dist, method="ward.D")
plot(csin, hang=-1)

# m <- c( "average", "single", "complete", "ward")
# > names(m) <- c( "average", "single", "complete", "ward")
# > 
#   > # function to compute coefficient
#   > ac <- function(x) {
#     +     agnes(resp.dist, method = x)$ac
#     + }
# > 
#   > map_dbl(m, ac)
# average    single  complete      ward 
# 0.9803841 0.8591921 0.9900113 0.9990978 

# #Elbow Method for finding the optimal number of clusters
# set.seed(123)
# # Compute and plot wss for k = 2 to k = 15.
# k.max <- 15
# data <- resp_scaled
# wss <- sapply(1:k.max, 
#               function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
# wss
# plot(1:k.max, wss,
#      type="b", pch = 19, frame = FALSE, 
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")

#make 5 clurster as an example
rect.hclust(csin, 5)
cl <- cutree(csin, 5)
cl2<-cutree(csin, 2)
dim(results)

#Add variable cluster ID to the results table
results$cl <- factor(cl)
head(results)
summary(results$cl) # Number of models in each cluster


#save plot
# png(file="reports/IOTC-2018-SC21-XX/figures/Dendogram.png", bg = "white", #compression="lzw",
#      width = 24, height = 16, units = "cm", res = 600)
plot(csin, hang=-1)
rect.hclust(csin, 5)
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

cl.data <- melt(results, id="cl",measure=c("SSB_Virgin","SSB_MSY", "SSB_endyr", "TotYield_MSY","SSB15SSB0"))
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

#Sampling 500 models from the clusters
set.seed(123)
om_subset <- unlist(lapply(1:5, function(x) sample(results[cl==x]$iter,100,replace=FALSE)))

#setting sample column in results dataframe
results[,sample:=results$iter%in%om_subset]

save(results, grid, metrics, residuals, file="om/out/metrics_sub.RData", compress="xz")

