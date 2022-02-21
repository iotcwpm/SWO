
# utilities.R - DESC
# /utilities.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2

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

setioswogrid <- function(sce, cpues,
  dir=paste0("grid_", format(Sys.time(), "%Y%m%d")),
  base=system.file("ext-data/sa", package="ioswomse"), name='swo', from=1,
  write=TRUE, delete=TRUE) {
	
  # EXPAND grid from sce
  
  if(is(sce, "data.frame"))
    grid <- sce
  else
    grid <- nameGrid(expand.grid(sce, stringsAsFactors=FALSE), from=from)
  
  if(!write)
    return(grid)


  # SET ctl, dat full paths
  ctlf <- file.path(base, paste0(name, ".ctl"))
  datf <- file.path(base, paste0(name, ".dat"))
 	
  # READ source files
  dats <- r4ss::SS_readdat_3.30(datf, verbose=FALSE)
  ctls <- r4ss::SS_readctl_3.30(file=ctlf, use_datlist=T, datlist=dats, verbose=FALSE)

  # DEFAULT cpue
  cpue <- cpues_biomass

  # NAMES in grid
  pars <- names(grid)[!names(grid) %in% c("iter", "id")]

  # CREATE dir
  if(dir.exists(dir))
    if(delete) {
      unlink(dir, recursive = TRUE, force = TRUE)
      dir.create(dir)
    } else  
      stop(paste("folder", dir, "already exists. Delete first."))
  else
    dir.create(dir)
  
	# SETUP grid
  foreach (i=grid$iter, .errorhandling = "remove") %dopar% {

    dat <- dats
    ctl <- ctls

    row <- which(grid$iter == i)
 
   
    # M
    if("M" %in% pars) {
      if(grid[row, "M"] == 999) {
        ctl$natM[1,] <- lorenzen$femM
        ctl$natM[2,] <- lorenzen$malM
      }
      else
        ctl$natM[1:2,] <- grid[row, "M"]
    }
    
    # steepness
    if("steepness" %in% pars) {
      ctl$SR_parms["SR_BH_steep", c("INIT", "PRIOR")] <- grid[row, "steepness"]
    }
    
    # llsel
    if("llsel" %in% pars) {
      if(grid[row, "llsel"] == "Logistic") {
        # CHANGE fleet TWLL_NW, TWFL_NE, EUEL_SW
        ctl$size_selex_types[c(3,7,8),"Pattern"] <-1
        #Mimic logistic from JPLL
        ctl$size_selex_parms[c(16,17,28,29, 34,35),] <- ctl$size_selex_parms[c(14:15),]
        #remove lines that are not used
        ctl$size_selex_parms <- ctl$size_selex_parms[-c(18:21,30:33,36:39),]
      }
    }

    # ESS obs
    if("ess" %in% pars) {
      #_mult_by_lencomp_N
      ctl$Variance_adjustment_list[,"Value"] <- grid[row, "ess"] / 200
    }
    
    # sigmaR, #_SRparm3
    if("sigmaR" %in% pars) {
      ctl$SR_parms["SR_sigmaR", c("INIT", "PRIOR")] <- grid[row, "sigmaR"]
    }
    
    # CPUE scaling schemes
    if("scaling" %in% pars) {
      # GET object by name (flaky)
      cpue <- get(paste0("cpues_", grid[[row, "scaling"]]))
      dat$CPUE[dat$CPUE$index %in% 16:19,] <- as.data.frame(subset(cpue[index %in% 16:19], index != 18 | year < 2004)[, 1:5])
    }
    
    
    # CPUEs
    if("cpue" %in% pars) {
      # JAP late + PT in SW
      if(grid[row, "cpue"] == "jappt") {
        # 18 - UJPLL_SW
        dat$CPUE<- subset(dat$CPUE, !dat$CPUE$index %in% c(16:19,20:23))
        dat$CPUE<- as.data.frame(rbind(subset(cpue , index != 18 | year < 2004)[, 1:5],dat$CPUE))
        # SET lambdas$value = 0.001 for all Surv_ but c(16:19,24)
        ctl$lambdas[ctl$lambdas$like_comp==1, "value"] <- 0.001
        ctl$lambdas[ctl$lambdas[,"fleet"] %in% c(16:19,24), "value"] <- 1.000
        # # SET Q_setup for UTW_JP
        # ctl$Q_setup[, "Q_type"] <- 2
        # ctl$Q_setup[14:16,"Q_type"] <- -13
      }
      
      # JAP late
      else if(grid[row, "cpue"] == "jap") {
        dat$CPUE<- subset(dat$CPUE, !dat$CPUE$index %in% c(16:19,20:23))
        dat$CPUE <- as.data.frame(rbind(cpue[, 1:5],dat$CPUE))
        # SET lambdas$value = 0.001 for all but c(16:19)
        ctl$lambdas[ctl$lambdas$like_comp==1, "value"] <- 0.001
        ctl$lambdas[ctl$lambdas[,"fleet"] %in% c(16:19), "value"] <- 1.000
        # SET Q_setup for UTW_JP
        # ctl$Q_setup[, "Q_type"] <- 2
        # ctl$Q_setup[14:16,"Q_type"] <- -13
      }
      
      # TWN late + PT in SW
      else if(grid[row, "cpue"] == "twnpt") {
        # 22 - UTWLL_SW
        dat$CPUE<- subset(dat$CPUE, !dat$CPUE$index %in% c(16:19,20:23))
        dat$CPUE <- as.data.frame(rbind(subset(cpue , index != 22 | year < 2004)[, 1:5],dat$CPUE))
        # SET lambdas$value = 0.001 for all but c(20:24) TWLL and PRT
        ctl$lambdas[ctl$lambdas$like_comp==1, "value"] <- 0.001
        ctl$lambdas[ctl$lambdas[,"fleet"] %in% c(20:24), "value"] <- 1.000
        # SET Q_setup for UTW_LL
        ctl$Q_options[1:4, c("link", "link_info", "biasadj", "float")] <- rep(c(1,0,0,1), each=4)
        ctl$Q_options[5, "float"] <- 0
        ctl$Q_options[6:8, c("link","link_info","biasadj","float")] <- rep(c(2,20,1,0), each=3)
        
        ctl$Q_parms[1, "PHASE"] <--1
        ctl$Q_parms[2:4, "INIT"] <- c(-11.3086, -12.5867, -12.0548)
        ctl$Q_parms[5, "PHASE"] <- 1
        ctl$Q_parms[6:8, "INIT"] <- 0
      }
      
    }
    

    # CPUE Q increases
    if("llq" %in% pars) {
      cpue <- data.table(dat$CPUE)
      dat$CPUE <- as.data.frame(cpue[, obs:=obs / as.numeric(grid[row, "llq"]) ^ seq(0,
                                                                       length(obs) - 1), by=index])
    }
    

    # Growth + maturity
    if("growmat" %in% pars) {
      if(grid[row, 'growmat'] == "farley") {
        # Farley otolith female
        ctl$MG_parms[1:3,] <- rbind(
          # L_at_Amin_Fem_GP_1_
          c(70, 90, 78.70, 78.70, 0.1, 6, -2, 0, 0, 0, 0, 0, 0, 0,2),
          # L_at_Amax_Fem_GP_1_
          c(250, 340, 275.8123, 275.8123, 0.1, 6, -2, 0, 0, 0, 0, 0, 0, 0,2),
          # VonBert_K_Fem_GP_1_
          c(0.05, 0.2, 0.157, 0.157, 0.1,6, -3, 0, 0, 0, 0, 0, 0, 0,2))
 
        # Farley otolith male
        ctl$MG_parms[12:14,] <- rbind(
          # L_at_Amin_Mal_GP_1_
          c(70, 90, 83.57, 83.57, 0.1, 6, -2, 0, 0, 0, 0, 0, 0, 0,2),
          # L_at_Amax_Mal_GP_1_
          c(200, 280, 213.7675, 213.7675, 0.1, 6, -2, 0, 0, 0, 0, 0, 0, 0,2),
          # VonBert_K_Mal_GP_1_
          c(0.07, 0.30, 0.235, 0.235, 0.1, 6, -3, 0, 0, 0, 0, 0, 0, 0,2))
        
         # Farley otolith maturity
        ctl$Age_Maturity[] <- c(0.001, 0.006, 0.027, 0.109, 0.354, 0.711, 0.917,
          0.98, 0.996, 0.999, rep(1, 21))

      } else if(grid[row, 'growmat'] == "wang") {
        ctl$MG_parms[1:3,] <- rbind(
          # Wang IO L_at_Amin_Fem_GP_1
          c(60, 90, 66.2, 66.2, 0, 0.1, -2, 0, 0, 0, 0, 0.5, 0, 0,2),
          # Wang IO L_at_Amax_Fem_GP_1
          c(210, 340, 274.9, 274.9, 0, 0.1, -2, 0, 0, 0, 0, 0.5, 0, 0,2),
          # Wang IO VonBert_K_Fem_GP_1
          c(0.05, 0.26, 0.138, 0.138, 0, 0.1, -3, 0, 0, 0, 0, 0.5, 0, 0,2))

         ctl$MG_parms[12:14,] <- rbind(
          # Wang IO L_at_Amin_Mal_GP_1
          c(60, 90, 72.1, 72.1, 0, 0.1, -2, 0, 0, 0, 0, 0.5, 0, 0,2),
          # Wang IO L_at_Amax_Mal_GP_1
          c(200, 280, 234, 234, 0, 0.1, -2, 0, 0, 0, 0, 0.5, 0, 0,2),
          # Wang IO VonBert_K_Mal_GP_1
          c(0.26, 0.28, 0.169, 0.169, 0, 0.1, -3, 0, 0, 0, 0, 0.5, 0, 0,2))


        # TWN/Hawai'i Maturity 50% age 4
        ctl$Age_Maturity[] <- c(0, 0, 0, 0, 0.02, 0.1, 0.5, 0.9, 0.98,
            rep(1, 22))
      }
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
    r4ss::SS_writectl_3.30(ctl, file.path(dirname, paste0(name, ".ctl")),verbose=F)
		
    # dat
    r4ss::SS_writedat_3.30(dat, outfile=file.path(dirname, paste0(name, ".dat")))
	}
	invisible(grid)
} # }}}
