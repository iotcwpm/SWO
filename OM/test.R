# test.R - DESC
# /test.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)

# --- model_partial.R {{{

load("model/partial.Rdata")


# PROJECTIONS

fut <- fwdWindow(stock, end=2050)

# fwd(F=FMSY)

ctrl <- fwdControl(lapply(2019:2050, function(x)
  list(year=x, quant="fbar", value=refpts$FMSY)))

ffmsy <- fwd(fut, sr=sr, control=ctrl)

plot(ffmsy, ctrl)

# fwd(F=0)

ctrl <- fwdControl(year=2019:2050, quant="fbar", value=1e-8)

ff0 <- fwd(fut, sr=sr, control=ctrl)

plot(ff0, ctrl)

# DEBUG ffwd fails for 2 sex model at predict(sr) step

nff0 <- ffwd(fut, sr=sr, control=ctrl)

plot(nff0, ctrl)

# }}}

# --- output.R {{{

# }}}
