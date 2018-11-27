# grid.R - DESC
# ioalbmse/R/grid.R

# Copyright European Union, 2015-2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# setioswogrid {{{

#' Sets a grid of SS3 runs for the IOTC SWO OM
#'
#' @name setioswogrid
#' @examples
#' # Simple example with a single parameter
#' scenarios  <- list(steepness=c(0.7, 0.8), M=c(0.2, 0.4))
#'
#' scenarios  <- list(steepness=c(0.7, 0.8), M=list(0.2, 0.4, lo=seq(0.2, 0.4, length=10)))
#' scenarios  <- list(steepness=c(0.7, 0.8), M=c(0.2, 0.4, "lo"))
#'
#' setioswogrid(scenarios, dir=tempdir())
#' list.dirs(path = tempdir(), recursive = TRUE)

setioswogrid <- function(scenarios, cpues,
  dir=paste0("grid_", format(Sys.time(), "%Y%m%d")),
  base=system.file("ext-data/sa", package="ioswomse"), name='swo', from=1,
  write=TRUE) {
	
  # EXPAND grid from sce
  
  grid <- nameGrid(expand.grid(scenarios, stringsAsFactors=FALSE), from=from)

  if(!write)
    return(grid)

  # SET ctl, dat full paths
  ctlf <- file.path(base, paste0(name, ".ctl"))
  datf <- file.path(base, paste0(name, ".dat"))
 	
  # READ source files
  dats <- r4ss::SS_readdat_3.24(datf, verbose=FALSE)
  ctls <- r4ss::SS_readctl_3.24(file=ctlf, use_datlist=T, datlist=dats,
    verbose=FALSE, ptype=FALSE)

  # DEFAULT cpue
  cpue <- cpues_orig

  # NAMES in grid
  pars <- names(scenarios)

  # CREATE dir
  if(!dir.exists(dir))
    dir.create(dir)

	# SETUP grid
  foreach (i=grid$iter, .errorhandling = "remove") %dopar% {

    dat <- dats
    ctl <- ctls

    row <- which(grid$iter == i)
 
    # llsel
    if("llsel" %in% pars) {
      if(grid[row, "llsel"] == "Logistic") {
        # CHANGE fleet 3 (S) 
        ctl$size_selex_parms[7:12, "INIT"] <- c(142.2780, -20, 6.9794, 20, -999, -999)
        ctl$size_selex_parms[c(8,10,11,12), "PHASE"] <- c(-5)
      }
    }

		# steepness
    if("steepness" %in% pars) {
      ctl$SRparm["SR_BH_steep", c("INIT", "PRIOR")] <- grid[row, "steepness"]
    }

    # M
    if("M" %in% pars) {
        if(grid[row, "M"] == 999) {
          ctl$natM[1,] <- lorenzen$femM
          ctl$natM[2,] <- lorenzen$malM
        }
        else
          ctl$natM[1:2,] <- grid[row, "M"]
    }

    # Growth + maturity
    if("growmat" %in% pars) {
      if(grid[row, 'growmat'] == "farley") {
        # Farley otolith female
        ctl$MG_parms[1:3,] <- rbind(
          # L_at_Amin_Fem_GP_1_
          c(70, 90, 78.70, 78.70, 0, 0.1, -2, 0, 0, 0, 0, 0, 0, 0),
          # L_at_Amax_Fem_GP_1_
          c(310, 340, 275.8123, 275.8123, 0, 0.1, -2, 0, 0, 0, 0, 0, 0, 0),
          # VonBert_K_Fem_GP_1_
          c(0.05, 0.2, 0.157, 0.157, 0, 0.1, -3, 0, 0, 0, 0, 0, 0, 0))
 
        # Farley otolith female
        ctl$MG_parms[6:8,] <- rbind(
          # L_at_Amin_Mal_GP_1_
          c(70, 90, 83.57, 83.57, 0, 0.1, -2, 0, 0, 0, 0, 0, 0, 0),
          # L_at_Amax_Mal_GP_1_
          c(200, 280, 213.7675, 213.7675, 0, 0.1, -2, 0, 0, 0, 0, 0, 0, 0),
          # VonBert_K_Mal_GP_1_
          c(0.07, 0.30, 0.235, 0.235, 0, 0.1, -3, 0, 0, 0, 0, 0, 0, 0))
        
        # Farley otolith maturity
        ctl$Age_Maturity[] <- c(0.001, 0.006, 0.027, 0.109, 0.354, 0.711, 0.917,
          0.98, 0.996, 0.999, rep(1, 21))

      } else if(grid[row, 'growmat'] == "wang") {
        ctl$MG_parms[1:3,] <- rbind(
          # Wang IO L_at_Amin_Fem_GP_1
          c(70, 90, 66.2, 66.2, 0, 0.1, -2, 0, 0, 0, 0, 0.5, 0, 0),
          # Wang IO L_at_Amax_Fem_GP_1
          c(310, 340, 274.9, 274.9, 0, 0.1, -2, 0, 0, 0, 0, 0.5, 0, 0),
          # Wang IO VonBert_K_Fem_GP_1
          c(0.05, 0.26, 0.138, 0.138, 0, 0.1, -3, 0, 0, 0, 0, 0.5, 0, 0))

        ctl$MG_parms[6:8,] <- rbind(
          # Wang IO L_at_Amin_Mal_GP_1
          c(70, 90, 72.1, 72.1, 0, 0.1, -2, 0, 0, 0, 0, 0.5, 0, 0),
          # Wang IO L_at_Amax_Mal_GP_1
          c(230, 280, 234, 234, 0, 0.1, -2, 0, 0, 0, 0, 0.5, 0, 0),
          # Wang IO VonBert_K_Mal_GP_1
          c(0.26, 0.28, 0.169, 0.169, 0, 0.1, -3, 0, 0, 0, 0, 0.5, 0, 0))

        # TWN/Hawai'i Maturity 50% age 4
        ctl$Age_Maturity[] <- c(0, 0, 0, 0, 0.02, 0.1, 0.5, 0.9, 0.98,
            rep(1, 22))
      }
    }

    # ESS obs
    if("ess" %in% pars) {
      #_mult_by_lencomp_N
      ctl$Variance_adjustments[4,] <- grid[row, "ess"] / 200
    }

    # CPUE scaling schemes
    if("scaling" %in% pars) {
      # GET object by name (flaky)
      cpue <- get(paste0("cpues_", grid[row, "scaling"]))
    }

    # CPUEs
    if("cpue" %in% pars) {
    # JAP late + PT in SW
      if(grid[row, "cpue"] == "jappt") {
        # 15 - UJPLL_SW
        dat$CPUE <- subset(cpue , index != 15 | year < 2000)[, 1:5]
        # SET lambdas$value = 0.001 for all Surv_ but c(13:16,21)
        ctl$lambdas[ctl$lambdas[,"fleet/survey"] %in% c(13:32), "value"] <- 0.001
        ctl$lambdas[ctl$lambdas[,"fleet/survey"] %in% c(13:16,21), "value"] <- 1.000
        # SET Q_setup for UTW_JP
        ctl$Q_setup[, "Q_type"] <- 2
        ctl$Q_setup[14:16,"Q_type"] <- -13
      }

    # JAP late
      else if(grid[row, "cpue"] == "jap") {
        dat$CPUE <- cpue[, 1:5]
        # SET lambdas$value = 0.001 for all but c(13:16)
        ctl$lambdas[ctl$lambdas[,"fleet/survey"] %in% c(13:32), "value"] <- 0.001
        ctl$lambdas[ctl$lambdas[,"fleet/survey"] %in% c(13:16), "value"] <- 1.000
        # SET Q_setup for UTW_JP
        ctl$Q_setup[, "Q_type"] <- 2
        ctl$Q_setup[14:16,"Q_type"] <- -13
      }

    # TWN late + PT in SW
      else if(grid[row, "cpue"] == "twnpt") {
        dat$CPUE <- subset(cpue , index != 19 | year < 2000)[, 1:5]
        # SET lambdas$value = 0.001 for all but c(17:21)
        ctl$lambdas[ctl$lambdas[,"fleet/survey"] %in% c(13:32), "value"] <- 0.001
        ctl$lambdas[ctl$lambdas[,"fleet/survey"] %in% c(17:21), "value"] <- 1.000
        # SET Q_setup for UTW_LL
        ctl$Q_setup[, "Q_type"] <- 2
        ctl$Q_setup[18:20,"Q_type"] <- -17
      }

      dat$N_cpue <- nrow(dat$CPUE)
    
    }

    # CPUE Q increases
    if("llq" %in% pars) {
      cpue <- data.table(dat$CPUE)
      dat$CPUE <- cpue[, obs:=obs / as.numeric(grid[row, "llq"]) ^ seq(0,
        length(obs) - 1), by=index]
    }

    # sigmaR, #_SRparm3
    if("sigmaR" %in% pars) {
      ctl$SRparm["SR_sigmaR", c("INIT", "PRIOR")] <- grid[row, "sigmaR"]
    }
		
    # CREATE dir
		dirname <- paste(dir, grid[row, "id"], sep='/')
		dir.create(dirname)

		# COPY unaltered files
		# starter.ss
		file.copy(paste(base, "starter.ss", sep="/"),
			paste(dirname, "starter.ss", sep="/"))
		
		# forecast.ss
		file.copy(paste(base, "forecast.ss", sep="/"),
			paste(dirname, "forecast.ss", sep="/"))

		# WRITE modified files
		# ctl
    r4ss::SS_writectl_3.24(ctl, file.path(dirname, paste0(name, ".ctl")), nseas=ctl$nseas)
		
    # dat
    r4ss::SS_writedat_3.24(dat, outfile=file.path(dirname, paste0(name, ".dat")))
	}
	invisible(grid)
} # }}}
