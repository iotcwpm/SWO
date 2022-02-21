# output.R - DESC
# /output.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

load("model/partial.Rdata")

# --- RESAMPLE with weighting

# GENERATE weights

results[, weight:=pvalue]

# GENERATE resamples
   
set.seed(47)

its <- 500

samps <- sample.int(dim(results)[1], size=its, prob=results[,weight],
  replace=TRUE)

# RESAMPLE runs based on weights

stock <- iter(stock, samps)
dimnames(stock) <- list(iter=seq(its))

sr <- iter(sr, samps)
dimnames(sr) <- list(iter=seq(its))

refpts <- iter(refpts, samps)
dimnames(refpts)$iter <- seq(its)

results <- results[samps, ]
results[, orig:=iter]
results[, iter:=seq(its)]

# Adapt the FLIndices objects. Keep only UJPLL_NW & UTWLL_NW

indices <- iter(indices, samps)

# RENAME iters
 
indices <- lapply(indices, function(x) {
  dimnames(x) <- list(iter=seq(its))
  return(x)
  }
)


# --- DEVIANCES

iy <- 2020
fy <- 2040


# rho: fishlife:sword fish rho(rec) = 0.68
rho <- 0.68

# PAST deviances

lastdevs <- residuals(sr)[, ac(2010:2015)]

alldevs <- residuals(sr)[, ac(1970:2015)]

rho <- c()
for (its in 1:500)   rho[its] <- acf(iter(alldevs,its))$acf[2]
ggplot(data.frame(rho), aes(rho))  + geom_density()   +geom_vline(xintercept = 0.69)
plot(results[, Recr_sigma],rho)
plot(results[, sigmaR],rho)

# 1. Autocorrelated from 2010:2015

devsrho <- Reduce(combine, lapply(seq(its), function(x)
    ar1rlnorm(rho=rho[x], years=seq(2016, fy), iters=1,
    sdlog=results[x, Recr_sigma]) %*% lastdevs[, '2015',,,,x]))

dimnames(devsrho) <- list(age=1, iter=results$iter)

plot(FLQuants(historic=lastdevs,sim=devsrho),worm=c(1:5))


# 2. Autocorrelated moving to N(0,1)

devsmov <- window(lastdevs, end=fy)

var <- rnorm(its, devsmov %=% 0, results[, Recr_sigma]) 

rho<-FLQuant(rho,dim=c(1,1,1,1,1,500))

for(i in seq(2016, fy))
  devsmov[, ac(i)] <- rho * devsmov[, ac(i-1)] + var[, ac(i)] * sqrt(1 - rho^2)
  
plot(devsmov,worm=1:5) + geom_vline(xintercept = 2016)  

# 3. GENERATE lnorm(0, sigma_i) deviances

devs0 <- Reduce(combine, lapply(results[, Recr_sigma], function(x)
  rlnorm(1, FLQuant(0, dimnames=list(year=seq(2016, fy), age=1)), x)))

# SRR deviances

deviances <- FLQuants(N=append(exp(lastdevs), devs0),
  RHO=exp(append(lastdevs, devsrho)), MOV=exp(devsmov))

deviances <- lapply(deviances, expand, unit=c('F','M'))

# deviances <- lapply(deviances, expand, unit=c('F', 'M'))

ggsave("./report/output/deviances.png",
plot(deviances,worm=1:3) + geom_vline(xintercept=c(2015, 2018), linetype=3) +
  geom_hline(yintercept=1, linetype=2, size=0.3)
)

# deviances <- lapply(deviances, expand, unit=c('F', 'M'), fill=TRUE)

# DEFAULT to moving deviances with rho=0.47

residuals(sr) <- deviances$MOV


# --- update the stock object to current year FWD 2019-20 based on total catches

# iotc.org, 10 FEB 2022: 2019=33589.46, 2020=26004.89

upd <- fwd(stf(stock, 2), sr=sr, control=fwdControl(year=c(2019,2020), quant="catch",
  value=c(33589.46, 26004.89)))

plot(upd)

# take this updated stock as the basis for the OM
stock <- upd


# --- OM & OEM


library(mse)

tmp <- stf(window(stock, end=2018), end=fy)
stock <- window(stock, end=fy)
stock[, ac(2020:fy)] <- tmp[, ac(2020:fy)]

om <- FLom(stock=stock, sr=sr, refpts=refpts,
  projection=mseCtrl(method=fwd.om))

# OEM observations

oem.obs <- list(stk=nounit(stock), idx=lapply(indices, fwdWindow, end=fy))

# OEM deviances, CHECK SDs

stock.devs <- FLQuants(
  catch.n=rlnorm(500, catch.n(oem.obs$stk) %=% 0, 0.2)
)

idx.devs <- lapply(oem.obs$idx, function(x) rlnorm(500, index.q(x) %=% 0, 0.3))

# CONSTRUCT OEM

oem <- FLoem(observations=oem.obs, deviances=list(stk=stock.devs, idx=idx.devs),
  method=sampling.oem)

# SAVE

save(om, oem, results, deviances, file="output/om.Rdata", compress="xz")
