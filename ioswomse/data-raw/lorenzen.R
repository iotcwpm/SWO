# lorenzen.R - DESC
# /lorenzen.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# VB
VB <- function(Linf, k, t0, ageMax){
  return(1.066 * (Linf*(1-exp(-k*(0:ageMax-t0)))) + 10.449)
}

natM <- function(Linf, k, t0, ageMax){
  L = VB(Linf=Linf, k=k, t0=t0, ageMax = ageMax)
  W = (3.815e-006 * L ^ 3.188)*1000
  M = 3.0 * W^(-0.288)
  return(data.frame(Age = 0:ageMax, Size = L, Weight=W/1000, M = M))
}

optima <- function(data, firstage){
  functionM <- function(a){
    (exp(mean(log(a*data$M[4:30])))-M)^2
  }
  optim(2, functionM, method="BFGS")$par
}

### From Then et al. (2014)
M = 4.899*30^(-0.916)

## For female ray
rayfem <- natM(279.6, 0.123, -2.55, 30)
rayfem$M <- rayfem$M * optima(rayfem, 4)

## For male ray
raymal <- natM(213.8, 0.197, -2.28, 30)
raymal$M <- raymal$M * optima(raymal, 4)

## For female otolith
otofem <- natM(249.6, 0.157, -2.13, 30)
otofem$M <- otofem$M * optima(otofem, 4)
# RESCALE to 0.30 at age50
otofem$M <- otofem$M / (otofem$M[7] / 0.25)

## For male otolith
otomal <- natM(191.9, 0.235, -2.10, 30)
otomal$M <- otomal$M * optima(otomal, 4)
# RESCALE to 0.30 at age50
otomal$M <- otomal$M / (otomal$M[7] / 0.25)

lorenzen <- data.frame(age=otofem$Age, femM=otofem$M, malM=otomal$M)

save(lorenzen, file="../data/lorenzen.RData")


plot(femM~age, lorenzen, type='b', ylim=c(0, 1))
  points(malM~age, lorenzen, pch=19)
