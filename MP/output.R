# output.R - DESC
# /output.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(mse)


# model_tune_perfect {{{

load("model/tune_perf.Rdata")


# EXTRACT metrics

mets_tune_perf <- lapply(tune, function(x) metrics(stock(x), metrics=mets))



# }}}
