# model.R - DESC
# /model.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

source("utilities.R")

# DATA
load("data/om.Rdata")
# --- DEBUG
vcov(sr(om)) <- hessian(sr(om))

# SET Ftarget and SBlim

refpts(om)$Ftarget <- refpts(om)$FMSY 
refpts(om)$SBlim <- refpts(om)$SBMSY * 0.20

# MSE arguments

mseargs <- list(iy=2019, data_lag=2, frq=3)
