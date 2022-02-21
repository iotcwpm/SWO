# io4_h80_GoMf_r2_CL005
#V3.30.15.00-trans;_2020_03_26;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.0
#Stock Synthesis (SS) is a work of the U.S. Government and is not subject to copyright protection in the United States.
#Foreign copyrights may apply. See copyright.txt for more information.
#_user_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_user_info_available_at:https://vlab.ncep.noaa.gov/group/stock-synthesis
#_data_and_control_files: data.ss // control.ss
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns (Growth Patterns, Morphs, Bio Patterns, GP are terms used interchangeably in SS)
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Platoon_within/between_stdev_ratio (no read if N_platoons=1)
#_Cond  1 #vector_platoon_dist_(-1_in_first_val_gives_normal_approx)
#
2 # recr_dist_method for parameters:  2=main effects for GP, Settle timing, Area; 3=each Settle entity; 4=none, only when N_GP*Nsettle*pop==1
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
4 #  number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
 1 1 2 0
 1 1 3 0
 1 1 4 0
#
8 #_N_movement_definitions
0.6 # first age that moves (real age at begin of season, not integer)
# seas,GP,source_area,dest_area,minage,maxage
 1 1 1 2 9 30
 1 1 1 3 9 30
 1 1 2 1 9 30
 1 1 2 4 9 30
 1 1 3 1 9 30
 1 1 3 4 9 30
 1 1 4 2 9 30
 1 1 4 3 9 30
#
1 #_Nblock_Patterns
 1 #_blocks_per_pattern 
# begin and end years of blocks
 1949 1949
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#
# AUTOGEN
 1 1 1 1 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen time-varying parms of this category; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
#
#_Available timevary codes
#_Block types: 0: P_block=P_base*exp(TVP); 1: P_block=P_base+TVP; 2: P_block=TVP; 3: P_block=P_block(-1) + TVP
#_Block_trends: -1: trend bounded by base parm min-max and parms in transformed units (beware); -2: endtrend and infl_year direct values; -3: end and infl as fraction of base range
#_EnvLinks:  1: P(y)=P_base*exp(TVP*env(y));  2: P(y)=P_base+TVP*env(y);  3: null;  4: P(y)=2.0/(1.0+exp(-TVP1*env(y) - TVP2))
#_DevLinks:  1: P(y)*=exp(dev(y)*dev_se;  2: P(y)+=dev(y)*dev_se;  3: random walk;  4: zero-reverting random walk with rho;  21-24 keep last dev for rest of years
#
#_Prior_codes:  0=none; 6=normal; 1=symmetric beta; 2=CASAL's beta; 3=lognormal; 4=lognormal with biascorr; 5=gamma
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement 
#
3 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25
0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 0.25 #_no additional input for selected M option; read 1P per morph
#
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr; 5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation
0.01 #_Age(post-settlement)_for_L1;linear growth below this
999 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)
0  #_placeholder for future growth feature
#
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
#
3 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
#_Age_Maturity by growth pattern
 0.001 0.006 0.027 0.109 0.354 0.711 0.917 0.98 0.996 0.999 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
1 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_ LO HI INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn
# Sex: 1  BioPattern: 1  NatMort
#0.1 0.6 0.25 0.25 1 6 -8 0 0 0 0 0.5 0 0 # NatM_p_1_Fem_GP_1
# xxx M04 0.1 0.6 0.4 0.4 1 6 -8 0 0 0 0 0.5 0 0 # NatM_p_1_Fem_GP_1
# Sex: 1  BioPattern: 1  Growth
  70 90 78.7 78.7 0.1 6 -2 0 0 0 0 0 0 0 				# Farley Otolith L_at_Amin_Fem_GP_1
  250 340 275.812 275.812 0.1 6 -2 0 0 0 0 0 0 0 		# Farley Otolith L_at_Amax_Fem_GP_1
  0.05 0.2 0.157 0.157 0.1 6 -3 0 0 0 0 0 0 0			# Farley Otolith VonBert_K_Fem_GP_1
# xxx GtMf  60 90 66.2  66.2 0 0.1 -2 0 0 0 0 0.5 0 0	   		# Wang IO L_at_Amin_Fem_GP_1
# xxx GtMf  250 340 274.9 274.9 0 0.1 -2 0 0 0 0 0.5 0 0   		# Wang IO L_at_Amax_Fem_GP_1
# xxx GtMf  0.05 0.26 0.138 0.138 0 0.1 -3 0 0 0 0 0.5 0 0 		# Wang IO VonBert_K_Fem_GP_1
 0.05 0.25 0.15 0.15 0.15 6 -3 0 0 0 0 0.5 0 0 # CV_young_Fem_GP_1
 0.05 0.25 0.1 0.15 0.15 6 -3 0 0 0 0 0.5 0 0 # CV_old_Fem_GP_1
# Sex: 1  BioPattern: 1  WtLen
 -3 3 3.815e-006 3.815e-006 99 0 -3 0 0 0 0 0.5 0 0 # Wtlen_1_Fem
 -3 4 3.188 3.188 99 0 -3 0 0 0 0 0.5 0 0 # Wtlen_2_Fem
# Sex: 1  BioPattern: 1  Maturity&Fecundity
 35 73 55 55 99 0 -3 0 0 0 0 0 0 0 # Mat50%_Fem
 -3 3 -0.25 -0.25 99 0 -3 0 0 0 0 0 0 0 # Mat_slope_Fem
 -3 3 1 1 99 0 -3 0 0 0 0 0 0 0 # Eggs/kg_inter_Fem
 -3 3 0 0 99 0 -3 0 0 0 0 0 0 0 # Eggs/kg_slope_wt_Fem
# Sex: 2  BioPattern: 1  NatMort
#0.1 0.6 0.25 0.25 1 6 -8 0 0 0 0 0.5 0 0 # NatM_p_1_Mal_GP_1
# xxx M04  0.1 0.6 0.4 0.4 1 6 -8 0 0 0 0 0.5 0 0 # NatM_p_1_Mal_GP_1
# Sex: 2  BioPattern: 1  Growth
   70 90 83.57 83.57 0.1 6 -2 0 0 0 0 0 0 0 			# Farley Otolith  # L_at_Amin_Mal_GP_1
   200 280 213.768 213.768 0.1 6 -2 0 0 0 0 0 0 0 	# Farley Otolith  # L_at_Amax_Mal_GP_1
   0.07 0.3 0.235 0.235 0.1 6 -3 0 0 0 0 0 0 0 		# Farley Otolith  # VonBert_K_Mal_GP_1
# xxx GtMf  60 90 72.1 72.1 0 0.1 -2 0 0 0 0 0.5 0 0 # Wang IO L_at_Amin_Mal_GP_1
# xxx GtMf  200 280 234 234 0 0.1 -2 0 0 0 0 0.5 0 0 # Wang IO L_at_Amax_Mal_GP_1
# xxx GtMf  0.26 0.28 0.169 0.169 0 0.1 -3 0 0 0 0 0.5 0 0 # Wang IO VonBert_K_Mal_GP_1
 0.05 0.25 0.15 0.15 0.15 6 -3 0 0 0 0 0.5 0 0 # CV_young_Mal_GP_1
 0.05 0.25 0.1 0.15 0.15 6 -3 0 0 0 0 0.5 0 0 # CV_old_Mal_GP_1
# Sex: 2  BioPattern: 1  WtLen
 -3 3 3.815e-006 3.815e-006 99 0 -3 0 0 0 0 0.5 0 0 # Wtlen_1_Mal
 -3 4 3.188 3.188 99 0 -3 0 0 0 0 0.5 0 0 # Wtlen_2_Mal
# Hermaphroditism
#  Recruitment Distribution  
 -8 8 0 1 99 0 -3 0 0 0 0 0.5 0 0 # RecrDist_GP_1
 -8 8 0 1 99 0 -4 0 0 0 0 0.5 0 0 # RecrDist_Area_1
 -8 8 -0.509876 1 99 0 4 0 1 1965 2017 4 0 0 # RecrDist_Area_2
 -8 8 -0.295335 1 99 0 4 0 1 1965 2017 4 0 0 # RecrDist_Area_3
 -8 8 -0.187103 1 99 0 4 0 1 1965 2017 4 0 0 # RecrDist_Area_4
 -8 8 0 1 99 0 -7 0 0 0 0 0.5 0 0 # RecrDist_timing_1
#  Cohort growth dev base
 0.1 10 1 1 1 6 -1 0 0 0 0 0 0 0 # CohortGrowDev
#  Movement
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_1to_2
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_1to_2
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_1to_3
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_1to_3
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_2to_1
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_2to_1
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_2to_4
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_2to_4
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_3to_1
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_3to_1
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_3to_4
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_3to_4
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_4to_2
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_4to_2
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_4to_3
 -8 9 -7 -5 5 6 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_4to_3
#  Age Error from parameters
#  catch multiplier
#  fraction female, by GP
 0.000001 0.999999 0.5 0.5  0.5 0 -99 0 0 0 0 0 0 0 # FracFemale_GP_1
#
# timevary MG parameters 
#_ LO HI INIT PRIOR PR_SD PR_type  PHASE
 0.0001 2 0.9 0.9 0.5 6 -5 # RecrDist_Area_2_dev_se
 -0.99 0.99 0 0 0.5 6 -6 # RecrDist_Area_2_dev_autocorr
 0.0001 2 0.9 0.9 0.5 6 -5 # RecrDist_Area_3_dev_se
 -0.99 0.99 0 0 0.5 6 -6 # RecrDist_Area_3_dev_autocorr
 0.0001 2 0.9 0.9 0.5 6 -5 # RecrDist_Area_4_dev_se
 -0.99 0.99 0 0 0.5 6 -6 # RecrDist_Area_4_dev_autocorr
# info on dev vectors created for MGparms are reported with other devs after tag parameter section 
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
3 #_Spawner-Recruitment; Options: 1=NA; 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepherd_3Parm; 9=RickerPower_3parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name
             7            18       8.42702            11           100             0          3          0          0          0          0          0          0          0 # SR_LN(R0)
# xxx h70  0.2             1           0.7           0.7           0.1             1        -10          0          0          0          0          0          0          0 # SR_BH_steep
  0.2             1           0.8           0.8           0.1             1        -10          0          0          0          0          0          0          0 # SR_BH_steep
# xxx h90  0.2             1           0.9           0.9           0.1             1        -10          0          0          0          0          0          0          0 # SR_BH_steep
     0             2           0.2           0.2           0.8             0         -3          0          0          0          0          0          0          0 # SR_sigmaR
# xxx r4     0             2           0.4           0.4           0.8             0         -3          0          0          0          0          0          0          0 # SR_sigmaR
            -5             5             0             0             1             6         -4          0          0          0          0          0          0          0 # SR_regime
             0             0             0             0             0             0        -99          0          0          0          0          0          0          0 # SR_autocorr
#_no timevary SR parameters
1 #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty
1970 # first year of main recr_devs; early devs can preceed this era
2017 # last year of main recr_devs; forecast devs start in following year
6 #_recdev phase 
1 # (0/1) to read 13 advanced options
 0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 -5 #_recdev_early_phase
 5 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1970 #_last_yr_nobias_adj_in_MPD; begin of ramp
 1971 #_first_yr_fullbias_adj_in_MPD; begin of plateau
 2016 #_last_yr_fullbias_adj_in_MPD
 2017 #_end_yr_for_ramp_in_MPD (can be in forecast to shape ramp, but SS sets bias_adj to 0.0 for fcast yrs)
 1 #_max_bias_adj_in_MPD (typical ~0.8; -3 sets all years to 0.0; -2 sets all non-forecast yrs w/ estimated recdevs to 1.0; -1 sets biasadj=1.0 for all yrs w/ recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -6 #min rec_dev
 6 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  1950R 1951R 1952R 1953R 1954R 1955R 1956R 1957R 1958R 1959R 1960R 1961R 1962R 1963R 1964R 1965R 1966R 1967R 1968R 1969R 1970R 1971R 1972R 1973R 1974R 1975R 1976R 1977R 1978R 1979R 1980R 1981R 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013R 2014R 2015F 2016F 2017F 2018F 2019F 2020F 2021F 2022F 2023F 2024F 2025F
#  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# implementation error by year in forecast:  0 0 0 0 0 0 0 0 0 0
#
#Fishing Mortality info 
0.2 # F ballpark value in units of annual_F
2003 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
4 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
2  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; count = 0
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
#2025 2036
# F rates by fleet
# Yr:  1950 1951 1952 1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 2025
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# ALGI_NW 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 4 4 4 4 4 4 4 4 4
# JPLL_NW 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# TWLL_NW 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# ALGI_NE 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# JPLL_NE 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# TWLL_NE 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# TWFL_NE 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# EUEL_SW 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# ISEL_SW 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# JPLL_SW 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# TWLL_SW 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# AUEL_SE 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# EUEL_SE 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# JPLL_SE 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# TWLL_SE 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
#_Q_setup for fleets with cpue or survey data
#_1:  fleet number
#_2:  link type: (1=simple q, 1 parm; 2=mirror simple q, 1 mirrored parm; 3=q and power, 2 parm; 4=mirror with offset, 2 parm)
#_3:  extra input for link, i.e. mirror fleet# or dev index number
#_4:  0/1 to select extra sd parameter
#_5:  0/1 for biasadj or not
#_6:  0/1 to float
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
        16         1         0         0         0         0  #  UJPLL_NW
        17         2        16         0         1         0  #  UJPLL_NE
        18         2        16         0         1         0  #  UJPLL_SW
        19         2        16         0         1         0  #  UJPLL_SE
        20         1         0         0         0         1  #  UTWLL_NW
        21         1         0         0         0         1  #  UTWLL_NE
        22         1         0         0         0         1  #  UTWLL_SW
        23         1         0         0         0         1  #  UTWLL_SE
        24         1         0         0         0         1  #  UPOR_SW
        25         1         0         0         0         1  #  UESP_SW
        26         1         0         0         0         1  #  UZAF_SW
        27         1         0         0         0         1  #  UIND_NE
        28         1         0         0         0         1  #  UJPLL_NW_pre
        29         1         0         0         0         1  #  UJPLL_NE_pre
        30         1         0         0         0         1  #  UJPLL_SW_pre
        31         1         0         0         0         1  #  UJPLL_SE_pre
        32         1         0         0         0         1  #  UTWLL_NW_pre
        33         1         0         0         0         1  #  UTWLL_NE_pre
        34         1         0         0         0         1  #  UTWLL_SW_pre
        35         1         0         0         0         1  #  UTWLL_SE_pre
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
           -25            25      -11.3081             0             1             0         1          0          0          0          0          0          0          0  #  LnQ_base_UJPLL_NW(16)
           -25            25             0             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UJPLL_NE(17)
           -25            25             0             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UJPLL_SW(18)
           -25            25             0             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UJPLL_SE(19)
           -25            25      -11.3733             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UTWLL_NW(20)
           -25            25      -11.3086             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UTWLL_NE(21)
           -25            25      -12.5867             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UTWLL_SW(22)
           -25            25      -12.0548             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UTWLL_SE(23)
           -25            25      -14.6954             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UPOR_SW(24)
           -25            25      -12.9393             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UESP_SW(25)
           -25            25      -14.5541             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UZAF_SW(26)
           -25            25      -15.9842             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UIND_NE(27)
           -25            25      -11.9049             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UJPLL_NW_pre(28)
           -25            25      -11.9102             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UJPLL_NE_pre(29)
           -25            25      -12.0918             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UJPLL_SW_pre(30)
           -25            25      -12.7464             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UJPLL_SE_pre(31)
           -25            25      -10.9923             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UTWLL_NW_pre(32)
           -25            25      -10.3937             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UTWLL_NE_pre(33)
           -25            25      -10.3049             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UTWLL_SW_pre(34)
           -25            25      -10.7787             0             1             0         -1          0          0          0          0          0          0          0  #  LnQ_base_UTWLL_SE_pre(35)
#_no timevary Q parameters
#
#_size_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for all sizes
#Pattern:_1; parm=2; logistic; with 95% width specification
#Pattern:_5; parm=2; mirror another size selex; PARMS pick the min-max bin to mirror
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_6; parm=2+special; non-parm len selex
#Pattern:_43; parm=2+special+2;  like 6, with 2 additional param for scaling (average over bin range)
#Pattern:_8; parm=8; New doublelogistic with smooth transitions and constant above Linf option
#Pattern:_9; parm=6; simple 4-parm double logistic with starting length; parm 5 is first length; parm 6=1 does desc as offset
#Pattern:_21; parm=2+special; non-parm len selex, read as pairs of size, then selex
#Pattern:_22; parm=4; double_normal as in CASAL
#Pattern:_23; parm=6; double_normal where final value is directly equal to sp(6) so can be >1.0
#Pattern:_24; parm=6; double_normal with sel(minL) and sel(maxL), using joiners
#Pattern:_25; parm=3; exponential-logistic in size
#Pattern:_27; parm=3+special; cubic spline 
#Pattern:_42; parm=2+special+3; // like 27, with 2 additional param for scaling (average over bin range)
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 27 0 0 5 # 1 ALGI_NW
 1 0 0 0 # 2 JPLL_NW
 24 0 0 0 # 3 TWLL_NW
 5 0 0 1 # 4 ALGI_NE
 5 0 0 2 # 5 JPLL_NE
 5 0 0 3 # 6 TWLL_NE
 24 0 0 0 # 7 TWFL_NE
 24 0 0 0 # 8 EUEL_SW
 5 0 0 8 # 9 ISEL_SW
 5 0 0 2 # 10 JPLL_SW
 5 0 0 3 # 11 TWLL_SW
 5 0 0 8 # 12 AUEL_SE
 5 0 0 8 # 13 EUEL_SE
 5 0 0 2 # 14 JPLL_SE
 5 0 0 3 # 15 TWLL_SE
 5 0 0 2 # 16 UJPLL_NW
 5 0 0 2 # 17 UJPLL_NE
 5 0 0 2 # 18 UJPLL_SW
 5 0 0 2 # 19 UJPLL_SE
 5 0 0 3 # 20 UTWLL_NW
 5 0 0 3 # 21 UTWLL_NE
 5 0 0 3 # 22 UTWLL_SW
 5 0 0 3 # 23 UTWLL_SE
 5 0 0 8 # 24 UPOR_SW
 5 0 0 8 # 25 UESP_SW
 5 0 0 8 # 26 UZAF_SW
 5 0 0 7 # 27 UIND_NE
 5 0 0 2 # 28 UJPLL_NW_pre
 5 0 0 2 # 29 UJPLL_NE_pre
 5 0 0 2 # 30 UJPLL_SW_pre
 5 0 0 2 # 31 UJPLL_SE_pre
 5 0 0 3 # 32 UTWLL_NW_pre
 5 0 0 2 # 33 UTWLL_NE_pre
 5 0 0 2 # 34 UTWLL_SW_pre
 5 0 0 2 # 35 UTWLL_SE_pre
#
#_age_selex_patterns
#Pattern:_0; parm=0; selex=1.0 for ages 0 to maxage
#Pattern:_10; parm=0; selex=1.0 for ages 1 to maxage
#Pattern:_11; parm=2; selex=1.0  for specified min-max age
#Pattern:_12; parm=2; age logistic
#Pattern:_13; parm=8; age double logistic
#Pattern:_14; parm=nages+1; age empirical
#Pattern:_15; parm=0; mirror another age or length selex
#Pattern:_16; parm=2; Coleraine - Gaussian
#Pattern:_17; parm=nages+1; empirical as random walk  N parameters to read can be overridden by setting special to non-zero
#Pattern:_41; parm=2+nages+1; // like 17, with 2 additional param for scaling (average over bin range)
#Pattern:_18; parm=8; double logistic - smooth transition
#Pattern:_19; parm=6; simple 4-parm double logistic with starting age
#Pattern:_20; parm=6; double_normal,using joiners
#Pattern:_26; parm=3; exponential-logistic in age
#Pattern:_27; parm=3+special; cubic spline in age
#Pattern:_42; parm=2+special+3; // cubic spline; with 2 additional param for scaling (average over bin range)
#Age patterns entered with value >100 create Min_selage from first digit and pattern from remainder
#_Pattern Discard Male Special
 0 0 0 0 # 1 ALGI_NW
 0 0 0 0 # 2 JPLL_NW
 0 0 0 0 # 3 TWLL_NW
 0 0 0 0 # 4 ALGI_NE
 0 0 0 0 # 5 JPLL_NE
 0 0 0 0 # 6 TWLL_NE
 0 0 0 0 # 7 TWFL_NE
 0 0 0 0 # 8 EUEL_SW
 0 0 0 0 # 9 ISEL_SW
 0 0 0 0 # 10 JPLL_SW
 0 0 0 0 # 11 TWLL_SW
 0 0 0 0 # 12 AUEL_SE
 0 0 0 0 # 13 EUEL_SE
 0 0 0 0 # 14 JPLL_SE
 0 0 0 0 # 15 TWLL_SE
 0 0 0 0 # 16 UJPLL_NW
 0 0 0 0 # 17 UJPLL_NE
 0 0 0 0 # 18 UJPLL_SW
 0 0 0 0 # 19 UJPLL_SE
 0 0 0 0 # 20 UTWLL_NW
 0 0 0 0 # 21 UTWLL_NE
 0 0 0 0 # 22 UTWLL_SW
 0 0 0 0 # 23 UTWLL_SE
 0 0 0 2 # 24 UPOR_SW
 0 0 0 2 # 25 UESP_SW
 0 0 0 2 # 26 UZAF_SW
 0 0 0 2 # 27 UIND_NE
 0 0 0 2 # 28 UJPLL_NW_pre
 0 0 0 2 # 29 UJPLL_NE_pre
 0 0 0 2 # 30 UJPLL_SW_pre
 0 0 0 2 # 31 UJPLL_SE_pre
 0 0 0 2 # 32 UTWLL_NW_pre
 0 0 0 2 # 33 UTWLL_NE_pre
 0 0 0 2 # 34 UTWLL_SW_pre
 0 0 0 2 # 35 UTWLL_SE_pre
#
#_          LO            HI          INIT         PRIOR         PR_SD       PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name
# 1   ALGI_NW LenSelex
             0             0             0             0             0             0        -99          0          0          0          0        0.5          0          0  #  SizeSpline_Code_ALGI_NW(1)
        -0.001             1      0.616361             0         0.001             1          3          0          0          0          0        0.5          0          0  #  SizeSpline_GradLo_ALGI_NW(1)
            -1         0.001   0.000663187             0         0.001             1          3          0          0          0          0        0.5          0          0  #  SizeSpline_GradHi_ALGI_NW(1)
            20           300            15             0             0             0        -99          0          0          0          0       0.05          0          0  #  SizeSpline_Knot_1_ALGI_NW(1)
            20           300       86.0625             0             0             0        -99          0          0          0          0       0.05          0          0  #  SizeSpline_Knot_2_ALGI_NW(1)
            20           300         139.5             0             0             0        -99          0          0          0          0       0.05          0          0  #  SizeSpline_Knot_3_ALGI_NW(1)
            20           300       192.938             0             0             0        -99          0          0          0          0       0.05          0          0  #  SizeSpline_Knot_4_ALGI_NW(1)
            20           300       246.375             0             0             0        -99          0          0          0          0       0.05          0          0  #  SizeSpline_Knot_5_ALGI_NW(1)
            -9             7       -8.9974             0         0.001             1         -2          0          0          0          0       0.05          0          0  #  SizeSpline_Val_1_ALGI_NW(1)
            -9             7      0.958733             0         0.001             1          2          0          0          0          0       0.05          0          0  #  SizeSpline_Val_2_ALGI_NW(1)
            -9             7            -1             0             0             0        -99          0          0          0          0       0.05          0          0  #  SizeSpline_Val_3_ALGI_NW(1)
            -9             7      -2.71927             0         0.001             1          2          0          0          0          0       0.05          0          0  #  SizeSpline_Val_4_ALGI_NW(1)
            -9             7      -2.97974             0         0.001             1          2          0          0          0          0       0.05          0          0  #  SizeSpline_Val_5_ALGI_NW(1)
# 2   JPLL_NW LenSelex
            50           200           150           150            20             1          1          0          0          0          0          0          0          0  #  SizeSel_P1_JPLL_NW(2)
         10.01           100            20            20            20             1          3          0          0          0          0          0          0          0  #  SizeSel_P2_JPLL_NW(2)
# 3   TWLL_NW LenSelex
            50           200       142.278           150            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P1_TWLL_NW(3)
            -6             4     -0.316252            -3            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P2_TWLL_NW(3)
            -1             9       6.97936           8.3            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P3_TWLL_NW(3)
            -1             9       5.26149             4            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P4_TWLL_NW(3)
           -15            -5           -10            -1            99             1         -3          0          0          0          0        0.5          0          0  #  SizeSel_P5_TWLL_NW(3)
            -5             9      -1.57659            -1            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P6_TWLL_NW(3)
# 4   ALGI_NE LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_ALGI_NE(4)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_ALGI_NE(4)
# 5   JPLL_NE LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_JPLL_SW(10)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_JPLL_SW(10)
# 6   TWLL_NE LenSelex
           -1            200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_TWLL_SW(11)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_TWLL_SW(11)
# 7   TWFL_NE LenSelex
            50           200       142.278           150            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P1_TWFL_NE(7)
            -6             4     -0.316252            -3            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P2_TWFL_NE(7)
            -1             9       6.97936           8.3            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P3_TWFL_NE(7)
            -1             9       5.26149             4            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P4_TWFL_NE(7)
           -15            -5           -10            -1            99             1         -3          0          0          0          0        0.5          0          0  #  SizeSel_P5_TWFL_NE(7)
            -5             9      -1.57659            -1            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P6_TWFL_NE(7)
# 8   EUEL_SW LenSelex
            50           200       142.278           150            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P1_EUEL_SW(8)
            -6             4     -0.316252            -3            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P2_EUEL_SW(8)
            -1             9       6.97936           8.3            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P3_EUEL_SW(8)
            -1             9       5.26149             4            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P4_EUEL_SW(8)
           -15            -5           -10            -1            99             1         -3          0          0          0          0        0.5          0          0  #  SizeSel_P5_EUEL_SW(8)
            -5             9      -1.57659            -1            99             1          3          0          0          0          0        0.5          0          0  #  SizeSel_P6_EUEL_SW(8)
# 9   ISEL_SW LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_ISEL_SW(9)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_ISEL_SW(9)
# 10   JPLL_SW LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_JPLL_SW(10)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_JPLL_SW(10)
# 11   TWLL_SW LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_TWLL_SW(11)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_TWLL_SW(11)
# 12   AUEL_SE LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_AUEL_SE(12)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_AUEL_SE(12)
# 13   EUEL_SE LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_EUEL_SE(13)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_EUEL_SE(13)
# 14   JPLL_SE LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_JPLL_SE(14)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_JPLL_SE(14)
# 15   TWLL_SE LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_TWLL_SE(15)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_TWLL_SE(15)
# 16   UJPLL_NW LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UJPLL_NW(16)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UJPLL_NW(16)
# 17   UJPLL_NE LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UJPLL_NE(17)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UJPLL_NE(17)
# 18   UJPLL_SW LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UJPLL_SW(18)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UJPLL_SW(18)
# 19   UJPLL_SE LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UJPLL_SE(19)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UJPLL_SE(19)
# 20   UTWLL_NW LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UTWLL_NW(20)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UTWLL_NW(20)
# 21   UTWLL_NE LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UTWLL_NE(21)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UTWLL_NE(21)
# 22   UTWLL_SW LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UTWLL_SW(22)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UTWLL_SW(22)
# 23   UTWLL_SE LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UTWLL_SE(23)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UTWLL_SE(23)
# 24   UPOR_SW LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UPOR_SW(24)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UPOR_SW(24)
# 25   UESP_SW LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UESP_SW(25)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UESP_SW(25)
# 26   UZAF_SW LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UZAF_SW(26)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UZAF_SW(26)
# 27   UIND_NE LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UIND_NE(27)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UIND_NE(27)
# 28   UJPLL_NW_pre LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UJPLL_NW_pre(28)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UJPLL_NW_pre(28)
# 29   UJPLL_NE_pre LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UJPLL_NE_pre(29)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UJPLL_NE_pre(29)
# 30   UJPLL_SW_pre LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UJPLL_SW_pre(30)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UJPLL_SW_pre(30)
# 31   UJPLL_SE_pre LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UJPLL_SE_pre(31)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UJPLL_SE_pre(31)
# 32   UTWLL_NW_pre LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UTWLL_NW_pre(32)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UTWLL_NW_pre(32)
# 33   UTWLL_NE_pre LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UTWLL_NE_pre(33)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UTWLL_NE_pre(33)
# 34   UTWLL_SW_pre LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UTWLL_SW_pre(34)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UTWLL_SW_pre(34)
# 35   UTWLL_SE_pre LenSelex
            -1           200            -1            -1          0.01             1         -2          0          0          0          0          0          0          0  #  SizeSel_P1_UTWLL_SE_pre(35)
            -1           200            -1            -1          0.01             1         -3          0          0          0          0          0          0          0  #  SizeSel_P2_UTWLL_SE_pre(35)
# 1   ALGI_NW AgeSelex
# 2   JPLL_NW AgeSelex
# 3   TWLL_NW AgeSelex
# 4   ALGI_NE AgeSelex
# 5   JPLL_NE AgeSelex
# 6   TWLL_NE AgeSelex
# 7   TWFL_NE AgeSelex
# 8   EUEL_SW AgeSelex
# 9   ISEL_SW AgeSelex
# 10   JPLL_SW AgeSelex
# 11   TWLL_SW AgeSelex
# 12   AUEL_SE AgeSelex
# 13   EUEL_SE AgeSelex
# 14   JPLL_SE AgeSelex
# 15   TWLL_SE AgeSelex
# 16   UJPLL_NW AgeSelex
# 17   UJPLL_NE AgeSelex
# 18   UJPLL_SW AgeSelex
# 19   UJPLL_SE AgeSelex
# 20   UTWLL_NW AgeSelex
# 21   UTWLL_NE AgeSelex
# 22   UTWLL_SW AgeSelex
# 23   UTWLL_SE AgeSelex
# 24   UPOR_SW AgeSelex
# 25   UESP_SW AgeSelex
# 26   UZAF_SW AgeSelex
# 27   UIND_NE AgeSelex
# 28   UJPLL_NW_pre AgeSelex
# 29   UJPLL_NE_pre AgeSelex
# 30   UJPLL_SW_pre AgeSelex
# 31   UJPLL_SE_pre AgeSelex
# 32   UTWLL_NW_pre AgeSelex
# 33   UTWLL_NE_pre AgeSelex
# 34   UTWLL_SW_pre AgeSelex
# 35   UTWLL_SE_pre AgeSelex#_no timevary selex parameters
#
0   #  use 2D_AR1 selectivity(0/1)
#_no 2D_AR1 selex offset used
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read and autogen if tag data exist; 1=read
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# deviation vectors for timevary parameters
#  base   base first block   block  env  env   dev   dev   dev   dev   dev
#  type  index  parm trend pattern link  var  vectr link _mnyr  mxyr phase  dev_vector
#      1    23     1     0     0     2     0     1     1  1965  2014     4      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0
#      1    24     3     0     0     2     0     2     1  1965  2014     4      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0
#      1    25     5     0     0     2     0     3     1  1965  2014     4      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0      0
     #
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#_Factor  Fleet  Value
# xxx CL020      4      1       0.1
# xxx CL020      4      2       0.1
# xxx CL020      4      3       0.1
# xxx CL020      4      4       0.1
# xxx CL020      4      5       0.1
# xxx CL020      4      6       0.1
# xxx CL020      4      7       0.1
# xxx CL020      4      8       0.1
# xxx CL020      4      9       0.1
# xxx CL020      4     10       0.1
# xxx CL020      4     11       0.1
# xxx CL020      4     12       0.1
# xxx CL020      4     13       0.1
# xxx CL020      4     14       0.1
# xxx CL020      4     15       0.1
      4      1       0.025
      4      2       0.025
      4      3       0.025
      4      4       0.025
      4      5       0.025
      4      6       0.025
      4      7       0.025
      4      8       0.025
      4      9       0.025
      4     10       0.025
      4     11       0.025
      4     12       0.025
      4     13       0.025
      4     14       0.025
      4     15       0.025
 -9999   1    0  # terminator
#
4 #_maxlambdaphase
1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter
# read 35 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark; 18=initEQregime
#like_comp fleet  phase  value  sizefreq_method
 1 16 1 1 1
 1 17 1 1 1
 1 18 1 1 1
 1 19 1 1 1
 1 20 1 0.001 1
 1 21 1 0.001 1
 1 22 1 0.001 1
 1 23 1 0.001 1
 1 24 1 1 1
 1 25 1 0.001 1
 1 26 1 0.001 1
 1 27 1 0.001 1
 1 28 1 0.001 1
 1 29 1 0.001 1
 1 30 1 0.001 1
 1 31 1 0.001 1
 1 32 1 0.001 1
 1 33 1 0.001 1
 1 34 1 0.001 1
 1 35 1 0.001 1
 4 1 1 1 1
 4 2 1 1 1
 4 3 1 1 1
 4 4 1 1 1
 4 5 1 1 1
 4 6 1 1 1
 4 7 1 1 1
 4 8 1 1 1
 4 9 1 1 1
 4 10 1 1 1
 4 11 1 1 1
 4 12 1 1 1
 4 13 1 1 1
 4 14 1 1 1
 4 15 1 1 1
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#  0 0 0 0 #_CPUE/survey:_1
#  0 0 0 0 #_CPUE/survey:_2
#  0 0 0 0 #_CPUE/survey:_3
#  0 0 0 0 #_CPUE/survey:_4
#  0 0 0 0 #_CPUE/survey:_5
#  0 0 0 0 #_CPUE/survey:_6
#  0 0 0 0 #_CPUE/survey:_7
#  0 0 0 0 #_CPUE/survey:_8
#  0 0 0 0 #_CPUE/survey:_9
#  0 0 0 0 #_CPUE/survey:_10
#  0 0 0 0 #_CPUE/survey:_11
#  0 0 0 0 #_CPUE/survey:_12
#  0 0 0 0 #_CPUE/survey:_13
#  0 0 0 0 #_CPUE/survey:_14
#  0 0 0 0 #_CPUE/survey:_15
#  1 1 1 1 #_CPUE/survey:_16
#  1 1 1 1 #_CPUE/survey:_17
#  1 1 1 1 #_CPUE/survey:_18
#  1 1 1 1 #_CPUE/survey:_19
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_20
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_21
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_22
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_23
#  1 1 1 1 #_CPUE/survey:_24
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_25
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_26
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_27
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_28
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_29
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_30
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_31
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_32
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_33
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_34
#  0.001 0.001 0.001 0.001 #_CPUE/survey:_35
#  0 0 0 0 #_lencomp:_1
#  1 1 1 1 #_lencomp:_2
#  1 1 1 1 #_lencomp:_3
#  1 1 1 1 #_lencomp:_4
#  1 1 1 1 #_lencomp:_5
#  1 1 1 1 #_lencomp:_6
#  1 1 1 1 #_lencomp:_7
#  1 1 1 1 #_lencomp:_8
#  1 1 1 1 #_lencomp:_9
#  1 1 1 1 #_lencomp:_10
#  1 1 1 1 #_lencomp:_11
#  1 1 1 1 #_lencomp:_12
#  1 1 1 1 #_lencomp:_13
#  1 1 1 1 #_lencomp:_14
#  1 1 1 1 #_lencomp:_15
#  0 0 0 0 #_lencomp:_16
#  0 0 0 0 #_lencomp:_17
#  0 0 0 0 #_lencomp:_18
#  0 0 0 0 #_lencomp:_19
#  0 0 0 0 #_lencomp:_20
#  0 0 0 0 #_lencomp:_21
#  0 0 0 0 #_lencomp:_22
#  0 0 0 0 #_lencomp:_23
#  0 0 0 0 #_lencomp:_24
#  0 0 0 0 #_lencomp:_25
#  0 0 0 0 #_lencomp:_26
#  0 0 0 0 #_lencomp:_27
#  0 0 0 0 #_lencomp:_28
#  0 0 0 0 #_lencomp:_29
#  0 0 0 0 #_lencomp:_30
#  0 0 0 0 #_lencomp:_31
#  0 0 0 0 #_lencomp:_32
#  0 0 0 0 #_lencomp:_33
#  0 0 0 0 #_lencomp:_34
#  0 0 0 0 #_lencomp:_35
#  1 1 1 1 #_init_equ_catch
#  1 1 1 1 #_recruitments
#  1 1 1 1 #_parameter-priors
#  1 1 1 1 #_parameter-dev-vectors
#  1 1 1 1 #_crashPenLambda
#  1 1 1 1 # F_ballpark_lambda
0 # (0/1/2) read specs for more stddev reporting: 0 = skip, 1 = read specs for reporting stdev for selectivity, size, and numbers, 2 = mortality in addition to values in option 1
 # 0 2 0 0 # Selectivity: (1) fleet, (2) 1=len/2=age/3=both, (3) year, (4) N selex bins
 # 0 0 # Growth: (1) growth pattern, (2) growth ages
 # 0 0 0 # Numbers-at-age: (1) area(-1 for all), (2) year, (3) N ages
 # -1 # list of bin #'s for selex std (-1 in first bin to self-generate)
 # -1 # list of ages for growth std (-1 in first bin to self-generate)
 # -1 # list of ages for NatAge std (-1 in first bin to self-generate)
999
