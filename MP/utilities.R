# utilities.R - DESC
# /utilities.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


library(JABBA)

# jabba.sa {{{

jabba.sa <- function(stk, idx, args, tracking, idx.se=rep(0.2, length(idx)),
  model.type="Fox", ...) {

  # PREPARE outputs

  B <- stock(stk) %=% an(NA)
  
  refpts <- FLPar(NA, dimnames=list(params=c("FMSY", "BMSY", "MSY", "K"),
    iter=dimnames(stk)$iter), units=c("t", "f", "t", "t"))

  conv <- rep(0, args$it)

  # progressr
  # p <- progressr::progressor(steps=args$it)

  # LOOP
  for(i in seq(args$it)) {

    # p(sprintf("x=%g", i))
   
    # EXTRACT catch and index
    
    ca <- as.data.frame(iter(catch(stk), i), drop=TRUE)
    id <- model.frame(window(iter(lapply(idx, index), i), start=args$y0), drop=TRUE)
    se <- id
    se[, -1] <- as.list(idx.se)

    # CONSTRUCT input object
    inp <- build_jabba(catch=ca, cpue=id, se=se,
      assessment="ALB", scenario="test", model.type=model.type, sigma.est=FALSE,
      fixed.obsE=0.05, verbose=FALSE)
    
    # FIT
    # capture.output({fit <- fit_jabba(inp, quickmcmc=TRUE)}, type="message")
    fit <- tryCatch(
      fit_jabba(inp, quickmcmc=TRUE, verbose=FALSE, progress.bar="none", ...),
      # error, RETURN 0 output
      error = function(e) return(list(
        timeseries=array(9, dim=c(dim(ca)[1],1,1)),
        refpts=data.frame(k=9, bmsy=1, fmsy=0.1, msy=1)))
    )

    # B
    iter(B, i)[] <- fit$timeseries[, 1, 1]

    # refpts
    iter(refpts, i) <- unlist(fit$refpts[1, c('fmsy', 'bmsy', 'msy', 'k')])

    # tracking
    if(length(fit) > 2) {
      conv[i] <- 1
    }
  }

  # STORE outputs: biomass in @stock

  stock(stk) <- B
  attr(stk, "refpts") <- refpts

  track(tracking, "conv.est", ac(args$ay)) <- conv
  
  list(stk = stk, tracking = tracking)
}
# }}}

# METRICS {{{

mets <- list(Rec=function(x) unitSums(rec(x)), SB=function(x) unitSums(ssb(x)),
  C=function(x) unitSums(catch(x)), F=function(x) unitMeans(fbar(x)))

# RELATIVE metrics

relmets <- list(SBMSY=function(x) unitSums(ssb(x)) %/% refpts(om)$SBMSY,
  SB0=function(x) unitSums(ssb(x)) %/% refpts(om)$SB0,
  FMSY=function(x) unitMeans(fbar(x)) %/% refpts(om)$FMSY) 
# }}}
