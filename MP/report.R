# report.R - DESC
# /report.R

# Copyright Iago MOSQUEIRA (WMR), 2022
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# model_test {{{

load("model/test.Rdata")

# COMPUTE performance

# TRACKING

x <- tracking(test[[1]])

# - COMPARE SSB: om, obs, est



# }}}

# tune_perfect

plot(om, tune[1:3])

# perf_hcst_05_trig

plot(om, tune[[4]])

# IS trigger being hit?
