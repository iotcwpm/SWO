# io4_NTP_GaMf_r2_h75_CL020
#V3.21a
#_data_and_control_files: swo.dat // swo.ctl
#_SS-V3.21a-opt;_04/23/2011;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB
1  #_N_Growth_Patterns
1  #_N_Morphs_Within_GrowthPattern 
#_Cond  1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
 4 #  number of recruitment assignments (overrides GP*area*seas parameter values) 
 0 # recruitment interaction requested
#GP seas area for each recruitment assignment
   1 1 1
   1 1 2
   1 1 3
   1 1 4
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
0 #_Nblock_Patterns
#_Cond 0 #_blocks_per_pattern 
# begin and end years of blocks
#
0.5 #_fracfemale 
3 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2
0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=not implemented; 4=not implemented
0.01 #_Growth_Age_for_L1
999 #_Growth_Age_for_L2 (999 to use as Linf)
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
3 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=read fec and wt from wtatage.ss
#_Age_Maturity by growth pattern
 0.00325603 0.007115515 0.014569273 0.028013449 0.050623862 0.085939643 0.136874216 0.20429726 0.285874625 0.376104793 0.467854808 0.554549562 0.631738337 0.69745447 0.751682329 0.795546622 0.830633963 0.858568952 0.880806505 0.898561132 0.912807328 0.912807328  0.912807328  0.912807328  0.912807328  0.912807328  0.912807328  0.912807328  0.912807328  0.912807328  0.912807328  # CSIRO maturity 50% age 10  
# xxx GtMf 0 0 0 0 0.02 0.1 0.5 0.9 0.98 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 # TWN/Hawai'i Maturity 50% age 4
# xxx GhMf 0 0 0.02 0.1 0.5 0.9 0.98 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 # NMFS L_at_Amin_Fem_GP_1_
# xxx GrMf 0.001 0.005 0.025 0.110 0.373 0.741 0.932 0.985 0.997 0.999  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 # Farley 2016 Ray
# xxx GoMf 0.001 0.006 0.027 0.109 0.354 0.711 0.917 0.98 0.996 0.999   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  # Farley 2016 Ootlith
1 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=age-specific fxn
1 #_parameter_offset_approach (1=none  2= M  G  CV_G as offset from female-GP1  3=like SS2 V1.x)
1 #_env/block/dev_adjust_method (1=standard; 2=logistic transform keeps in base parm bounds; 3=standard w/ no bound check)
#
#_growth_parms
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
#  0.1 0.6 0.2 0.2 0 1 -8 0 0 0 0 0.5 0 0   # NatM_p_1_Fem_GP_1
# xxx GtMf  0.1 0.6 0.25 0.25 0 1 -8 0 0 0 0 0.5 0 0 # NatM_p_1_Fem_GP_1
# xxx GhMf  0.1 0.6 0.4 0.25 0 1 -8 0 0 0 0 0.5 0 0  # NatM_p_1_Fem_GP_1
# xxx GrMf  0.1 0.6 0.4 0.25 0 1 -8 0 0 0 0 0.5 0 0  # NatM_p_1_Fem_GP_1
# xxx GoMf  0.1 0.6 0.4 0.25 0 1 -8 0 0 0 0 0.5 0 0  # NatM_p_1_Fem_GP_1
  70 90 78.5 78.5 0 0.1 -2 0 0 0 0 0.5 0 0          # CSIRO L_at_Amin_Fem_GP_1_
  310 340 323.4 323.4 0 0.1 -2 0 0 0 0 0.5 0 0      # CSIRO L_at_Amax_Fem_GP_1_
  0.05 0.1 0.08148 0.08148 0 0.1 -3 0 0 0 0 0.5 0 0 # CSIRO VonBert_K_Fem_GP_1_
# xxx GtMf  70 90 66.2     66.2 0 0.1 -2 0 0 0 0 0.5 0 0   # Wang IO L_at_Amin_Fem_GP_1
# xxx GtMf  310 340 274.9 274.9 0 0.1 -2 0 0 0 0 0.5 0 0   # Wang IO L_at_Amax_Fem_GP_1
# xxx GtMf  0.05 0.26 0.138 0.138 0 0.1 -3 0 0 0 0 0.5 0 0 # Wang IO VonBert_K_Fem_GP_1
# xxx GhMf  70 90 72.6 72.6 0 0.1 -2 0 0 0 0 0.5 0 0          # NMFS L_at_Amin_Fem_GP_1_
# xxx GhMf  250 340 255.3 255.3 0 0.1 -2 0 0 0 0 0.5 0 0      # NMFS L_at_Amax_Fem_GP_1_
# xxx GhMf  0.24 0.26 0.246 0.246 0 0.1 -3 0 0 0 0 0.5 0 0    # NMFS VonBert_K_Fem_GP_1_
# xxx GrMf   70 90 83.22 83.22  0 0.1 -2 0 0 0 0 0 0 0            # Farley Ray L_at_Amin_Fem_GP_1_
# xxx GrMf   300 340 308.0713 308.0713 0 0.1 -2 0 0 0 0 0 0 0     # Farley Ray Ray L_at_Amax_Fem_GP_1_
# xxx GrMf   0.05 0.2 0.123 0.123 0 0.1 -3 0 0 0 0 0 0 0          # Farley Ray Ray VonBert_K_Fem_GP_1_
 
# xxx GoMf   70 90 78.70 78.70   0 0.1 -2 0 0 0 0 0 0 0           # Farley Otolith L_at_Amin_Fem_GP_1_
# xxx GoMf   310 340 275.8123 275.8123 0 0.1 -2 0 0 0 0 0 0 0     # Farley Otolith Ray L_at_Amax_Fem_GP_1_
# xxx GoMf   0.05 0.2 0.157 0.157 0 0.1 -3 0 0 0 0 0 0 0          # Farley Otolith Ray VonBert_K_Fem_GP_1_
 
 0.05 0.25 0.15 0.15 0 0.15 -3 0 0 0 0 0.5 0 0 # CV_young_Fem_GP_1
 0.05 0.25 0.1 0.15 0 0.15 -3 0 0 0 0 0.5 0 0 # CV_old_Fem_GP_1
 
#  0.1 0.6 0.2 0.2 0 1 -8 0 0 0 0 0.5 0 0   # NatM_p_1_Mal_GP_1
# xxx GtMf  0.1 0.6 0.25 0.25 0 1 -8 0 0 0 0 0.5 0 0 # NatM_p_1_Mal_GP_1
# xxx GhMf  0.1 0.6 0.4 0.25 0 1 -8 0 0 0 0 0.5 0 0  # NatM_p_1_Mal_GP_1
# xxx GrMf  0.1 0.6 0.4 0.25 0 1 -8 0 0 0 0 0.5 0 0  # NatM_p_1_Mal_GP_1
# xxx GoMf  0.1 0.6 0.4 0.25 0 1 -8 0 0 0 0 0.5 0 0  # NatM_p_1_Mal_GP_1
 
 
   70 90 80.6 80.6 0 0.1 -2 0 0 0 0 0.5 0 0          # CSIRO L_at_Amin_Mal_GP_1_
   240 280 260.47 260.47 0 0.1 -2 0 0 0 0 0.5 0 0    # CSIRO L_at_Amax_Mal_GP_1_
   0.07 0.13 0.1096 0.1096 0 0.1 -3 0 0 0 0 0.5 0 0  # CSIRO VonBert_K_Mal_GP_1_
 
# xxx GtMf  70 90 72.1 72.1 0 0.1 -2 0 0 0 0 0.5 0 0 # Wang IO L_at_Amin_Mal_GP_1
# xxx GtMf  230 280 234 234 0 0.1 -2 0 0 0 0 0.5 0 0 # Wang IO L_at_Amax_Mal_GP_1
# xxx GtMf  0.26 0.28 0.169 0.169 0 0.1 -3 0 0 0 0 0.5 0 0 # Wang IO VonBert_K_Mal_GP_1
 
# xxx GhMf  70 90 77.1 77.1 0 0.1 -2 0 0 0 0 0.5 0 0          # NMFS L_at_Amin_Mal_GP_1_
# xxx GhMf  230 280 232.04 232.04 0 0.1 -2 0 0 0 0 0.5 0 0    # NMFS L_at_Amax_Mal_GP_1_
# xxx GhMf  0.26 0.28 0.271 0.271 0 0.1 -3 0 0 0 0 0.5 0 0    # NMFS VonBert_K_Mal_GP_1_
 
# xxx GrMf 70 90 86.17 86.17 0 0.1 -2 0 0 0 0 0 0 0                 # Farley Ray L_at_Amin_Mal_GP_1_
# xxx GrMf 200 280 237.3166 237.3166 0 0.1 -2 0 0 0 0 0 0 0         # Farley Ray L_at_Amax_Mal_GP_1_
# xxx GrMf 0.07 0.30 0.197 0.197 0 0.1 -3 0 0 0 0 0 0 0             # Farley Ray VonBert_K_Mal_GP_1_
 
# xxx GoMf 70 90 83.57 83.57 0 0.1 -2 0 0 0 0 0 0 0                 # Farley Otolith L_at_Amin_Mal_GP_1_
# xxx GoMf 200 280 213.7675 213.7675 0 0.1 -2 0 0 0 0 0 0 0         # Farley Otolith L_at_Amax_Mal_GP_1_
# xxx GoMf 0.07 0.30 0.235 0.235 0 0.1 -3 0 0 0 0 0 0 0             # Farley Otolith VonBert_K_Mal_GP_1_
  
 0.05 0.25 0.15 0.15 0 0.15 -3 0 0 0 0 0.5 0 0 # CV_young_Mal_GP_1
  0.05 0.25 0.1 0.15 0 0.15 -3 0 0 0 0 0.5 0 0 # CV_old_Mal_GP_1
 -3 3 3.815e-006 3.815e-006 -1 99 -3 0 0 0 0 0.5 0 0 # Wtlen_1_Fem
 -3 4 3.188 3.188 -1 99 -3 0 0 0 0 0.5 0 0 # Wtlen_2_Fem
 35 73 55 55 -1 99 -3 0 0 0 0 0 0 0 # Mat50%_Fem
 -3 3 -0.25 -0.25 -1 99 -3 0 0 0 0 0 0 0 # Mat_slope_Fem
 -3 3 1 1 -1 99 -3 0 0 0 0 0 0 0 # Eggs/kg_inter_Fem
 -3 3 0 0 -1 99 -3 0 0 0 0 0 0 0 # Eggs/kg_slope_wt_Fem
 -3 3 3.815e-006 3.815e-006 -1 99 -3 0 0 0 0 0.5 0 0 # Wtlen_1_Mal
 -3 4 3.188 3.188 -1 99 -3 0 0 0 0 0.5 0 0 # Wtlen_2_Mal
 -8 8 0 1 -1 99 -3 0 0 0 0 0.5 0 0 # RecrDist_GP_1
 -8 8 0 1 -1 99 -4 0 0 0 0 0.5 0 0 # RecrDist_Area_1
  -8 8 -0.509876 1 -1 99 4 0 1 1965 2014 0.9 0 0 # RecrDist_Area_2
  -8 8 -0.295335 1 -1 99 4 0 1 1965 2014 0.9 0 0 # RecrDist_Area_3
  -8 8 -0.187103 1 -1 99 4 0 1 1965 2014 0.9 0 0 # RecrDist_Area_4
 -8 8 0 1 -1 99 -7 0 0 0 0 0.5 0 0 # RecrDist_Seas_1
 1 1 1 1 -1 99 -3 0 0 0 0 0 0 0 # CohortGrowDev
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_1to_2
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_1to_2
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_1to_3
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_1to_3
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_2to_1
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_2to_1
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_2to_4
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_2to_4
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_3to_1
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_3to_1
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_3to_4
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_3to_4
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_4to_2
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_4to_2
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_A_seas_1_GP_1from_4to_3
   -8 9 -7 -5 0 5 -9 0 0 0 0 0 0 0 # MoveParm_B_seas_1_GP_1from_4to_3
#
#_Cond 0  #custom_MG-env_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-environ parameters
#
#_Cond 0  #custom_MG-block_setup (0/1)
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no MG-block parameters
#_Cond No MG parm trends 
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1 femwtlen2 mat1 mat2 fec1 fec2 Malewtlen1 malewtlen2 L1 K
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
 4 #_MGparm_Dev_Phase
#
#_Spawner-Recruitment
3 #_SR_function: 1=B-H_flattop; 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=Shepard_3Parm
#_LO HI INIT PRIOR PR_type SD PHASE
 7 18 8.42702 11 -1 100 3 # SR_R0
# xxx h55 0.2 1 0.55 0.55 1 0.1 -10 # SR_steep
 0.2 1 0.75 0.75 1 0.1 -10 # SR_steep
# xxx h95 0.2 1 0.95 0.95 1 0.1 -10 # SR_steep
# xxx r0 0 2 0.01 0.01 -1 0.8 -3 # SR_sigmaR
 0 2 0.2 0.2 -1 0.8 -3 # SR_sigmaR
# xxx r4 0 2 0.4 0.4 -1 0.8 -3 # SR_sigmaR
 -5 5 0.1 0 0 1 -3 # SR_envlink
 -5 5 0 0 0 1 -4 # SR_R1_offset
 0 0 0 0 -1 0 -99 # SR_autocorr
0 #_SR_env_link
0 #_SR_env_target_0=none;1=devs;_2=R0;_3=steepness
# xxx r4 1 #do_recdev:  0=none; 1=devvector; 2=simple deviations
 1 #do_recdev:  0=none; 1=devvector; 2=simple deviations
# xxx r0 0 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1950 # first year of main recr_devs; early devs can preceed this era
2013 # last year of main recr_devs; forecast devs start in following year
6 #_recdev phase 
1 # (0/1) to read 13 advanced options
0 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
-5 #_recdev_early_phase
5 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
1 #_lambda for Fcast_recr_like occurring before endyr+1
1970 #_last_early_yr_nobias_adj_in_MPD
1971 #_first_yr_fullbias_adj_in_MPD
2001 #_last_yr_fullbias_adj_in_MPD
2002 #_first_recent_yr_nobias_adj_in_MPD
1 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
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
#
#Fishing Mortality info 
0.2 # F ballpark for tuning early phases
2003 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
4 # max F or harvest rate  depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
2  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms
#_LO HI INIT PRIOR PR_type SD PHASE
 0 1 0 0.01 0 99 -1 # InitF_1GI_NE
 0 1 0 0.01 0 99 -1 # InitF_2LL_NE
 0 1 0 0.01 0 99 -1 # InitF_3GI_NW
 0 1 0 0.01 0 99 -1 # InitF_4LL_NW
 0 1 0 0.01 0 99 -1 # InitF_5GI_SE
 0 1 0 0.01 0 99 -1 # InitF_6LL_SE
 0 1 0 0.01 0 99 -1 # InitF_7ALGI_SW
 0 1 0 0.01 0 99 -1 # InitF_8EUEL_SW
 0 1 0 0.01 0 99 -1 # InitF_9ISEL_SW
 0 1 0 0.01 0 99 -1 # InitF_10JPLL_SW
 0 1 0 0.01 0 99 -1 # InitF_11TWFL_SW
 0 1 0 0.01 0 99 -1 # InitF_12TWLL_SW
#
#_Q_setup
 # Q_type options:  <0=mirror  0=median_float  1=mean_float  2=parameter  3=parm_w_random_dev  4=parm_w_randwalk  5=mean_unbiased_float_assign_to_parm
 #_Den-dep  env-var  extra_se  Q_type
 0 0 0 2 # 1 GI_NE
 0 0 0 2 # 2 LL_NE
 0 0 0 2 # 3 GI_NW
 0 0 0 2 # 4 LL_NW
 0 0 0 2 # 5 GI_SE
 0 0 0 2 # 6 LL_SE
 0 0 0 2 # 7 ALGI_SW
 0 0 0 2 # 8 EUEL_SW
 0 0 0 2 # 9 ISEL_SW
 0 0 0 2 # 10 JPLL_SW
 0 0 0 2 # 11 TWFL_SW
 0 0 0 2 # 12 TWLL_SW
 0 0 0 2 # 13 UJPLL_NW
 0 0 0 -13 # 14 UJPLL_NE
 0 0 0 -13 # 15 UJPLL_SW
 0 0 0 -13 # 16 UJPLL_SE
 0 0 0 2 # 17 UTWLL_NW
 0 0 0 2 # 18 UTWLL_NE
 0 0 0 2 # 19 UTWLL_SW
 0 0 0 2 # 20 UTWLL_SE
 0 0 0 2 # 21 UPOR_SW
 0 0 0 2 # 22 UESP_SW
 0 0 0 2 # 23 URELL_SW
 0 0 0 2 # 24 UIND_NE
 0 0 0 2 # 25 UJPLL_NW_pre
 0 0 0 2 # 26 UJPLL_NE_pre
 0 0 0 2 # 27 UJPLL_SW_pre
 0 0 0 2 # 28 UJPLL_SE_pre
 0 0 0 2 # 29 UTWLL_NW_pre
 0 0 0 2 # 30 UTWLL_NE_pre
 0 0 0 2 # 31 UTWLL_SW_pre
 0 0 0 2 # 32 UTWLL_SE_pre
#_Cond 0 #_If q has random component  then 0=read one parm for each fleet with random q; 1=read a parm for each year of index
#_Q_parms(if_any)
# LO HI INIT PRIOR PR_type SD PHASE
 -10 10 -0.494066 0 0 99 -1 # Q_base_1_GI_NE
 -10 10 -0.494066 0 0 99 -1 # Q_base_2_LL_NE
 -10 10 -0.494066 0 0 99 -1 # Q_base_3_GI_NW
 -10 10 -0.494066 0 0 99 -1 # Q_base_4_LL_NW
 -10 10 -0.494066 0 0 99 -1 # Q_base_5_GI_SE
 -10 10 -0.494066 0 0 99 -1 # Q_base_6_LL_SE
 -10 10 -0.494066 0 0 99 -1 # Q_base_7_ALGI_SW
 -10 10 -0.494066 0 0 99 -1 # Q_base_8_EUEL_SW
 -10 10 -0.494066 0 0 99 -1 # Q_base_9_ISEL_SW
 -10 10 -0.494066 0 0 99 -1 # Q_base_10_JPLL_SW
 -10 10 -0.494066 0 0 99 -1 # Q_base_11_TWFL_SW
 -10 10 -0.494066 0 0 99 -1 # Q_base_12_TWLL_SW
 -20 10 -7.41213  0 0 99   1 # Q_base_13_UJPLL_NW 
 -20 10 -9.53595 0 0 99    1 # Q_base_17_UTWLL_NW
 -20 10 -9.0215 0 0 99     1 # Q_base_18_UTWLL_NE
 -20 10 -7.26104 0 0 99    1 # Q_base_19_UTWLL_SW
 -20 10 -10.4321 0 0 99    1 # Q_base_20_UTWLL_SE
 -20 10 -11.6308 0 0 99    1 # Q_base_21_UPOR_SW
 -20 10 -7.81646 0 0 99    1 # Q_base_22_UESP_SW
 -20 10 -11.6308 0 0 99    1 # Q_base_23_URELL_SW
 -20 10 -11.6308 0 0 99    1 # Q_base_24_UIND_NE
 -20 10 -7.41213  0 0 99   1 # Q_base_25_UJPLL_NW_pre 
 -20 10 -7.41213  0 0 99   1 # Q_base_26_UJPLL_NE_pre
 -20 10 -7.41213  0 0 99   1 # Q_base_27_UJPLL_SW_pre 
 -20 10 -7.41213  0 0 99   1 # Q_base_28_UJPLL_SE_pre 
 -20 10 -7.41213  0 0 99   1 # Q_base_29_UTWLL_NW_pre 
 -20 10 -7.41213  0 0 99   1 # Q_base_30_UTWLL_NE_pre
 -20 10 -7.41213  0 0 99   1 # Q_base_31_UTWLL_SW_pre 
 -20 10 -7.41213  0 0 99   1 # Q_base_32_UTWLL_SE_pre 
#_size_selex_types
#_Pattern Discard Male Special
  24 0 0 0 # 1 GI_NE
  24 0 0 0 # 2 LL_NE
  5 0 0 1 # 3 GI_NW
  5 0 0 2 # 4 LL_NW
  5 0 0 1 # 5 GI_SE
  5 0 0 2 # 6 LL_SE
  5 0 0 1 # 7 ALGI_SW
  5 0 0 2 # 8 EUEL_SW
  5 0 0 2 # 9 ISEL_SW
  5 0 0 2 # 10 JPLL_SW
  5 0 0 2 # 11 TWFL_SW
  5 0 0 2 # 12 TWLL_SW
  5 0 0 2 # 13 UJPLL_NW
  5 0 0 2 # 14 UJPLL_NE
  5 0 0 2 # 15 UJPLL_SW
  5 0 0 2 # 16 UJPLL_SE
  5 0 0 2 # 17 UTWLL_NW
  5 0 0 2 # 18 UTWLL_NE
  5 0 0 2 # 19 UTWLL_SW
  5 0 0 2 # 20 UTWLL_SE
  5 0 0 2 # 21 UPOR_SW
  5 0 0 2 # 22 UESP_SW
  5 0 0 2 # 23 URELL_SW
  5 0 0 2 # 24 UIND_NE
  5 0 0 2 # 25 UJPLL_NW_pre
  5 0 0 2 # 26 UJPLL_NE_pre
  5 0 0 2 # 27 UJPLL_SW_pre
  5 0 0 2 # 28 UJPLL_SE_pre
  5 0 0 2 # 29 UTWLL_NW_pre
  5 0 0 2 # 30 UTWLL_NE_pre
  5 0 0 2 # 31 UTWLL_SW_pre
  5 0 0 2 # 32 UTWLL_SE_pre
#_age_selex_types
#_Pattern ___ Male Special
  11 0 0 0 # 1 GI_NE
  11 0 0 0 # 2 LL_NE
  11 0 0 0 # 3 GI_NW
  11 0 0 0 # 4 LL_NW
  11 0 0 0 # 5 GI_SE
  11 0 0 0 # 6 LL_SE
  11 0 0 0 # 7 ALGI_SW
  11 0 0 0 # 8 EUEL_SW
  11 0 0 0 # 9 ISEL_SW
  11 0 0 0 # 10 JPLL_SW
  11 0 0 0 # 11 TWFL_SW
  11 0 0 0 # 12 TWLL_SW
  11 0 0 0 # 13 UJPLL_NW
  11 0 0 0 # 14 UJPLL_NE
  11 0 0 0 # 15 UJPLL_SW
  11 0 0 0 # 16 UJPLL_SE
  11 0 0 0 # 17 UTWLL_NW
  11 0 0 0 # 18 UTWLL_NE
  11 0 0 0 # 19 UTWLL_SW
  11 0 0 0 # 20 UTWLL_SE
  11 0 0 2 # 21 UPOR_SW
  11 0 0 2 # 22 UESP_SW
  11 0 0 2 # 23 URELL_SW
  11 0 0 2 # 24 UIND_NE
  11 0 0 2 # 25 UJPLL_NW
  11 0 0 2 # 26 UJPLL_NE
  11 0 0 2 # 27 UJPLL_SW
  11 0 0 2 # 28 UJPLL_SE
  11 0 0 2 # 29 UTWLL_NW
  11 0 0 2 # 30 UTWLL_NE
  11 0 0 2 # 31 UTWLL_SW
  11 0 0 2 # 32 UTWLL_SE 
#_LO HI INIT PRIOR PR_type SD PHASE env-var use_dev dev_minyr dev_maxyr dev_stddev Block Block_Fxn
  50 200 91.86 150 1 99 3 0 0 0 0 0.5 0 0 # SizeSel_1P_1_GI_NE
  -6 4 -1.061 -3 1 99 3 0 0 0 0 0.5 0 0 # SizeSel_1P_2_GI_NE
  -1 9 4.714 8.3 1 99 3 0 0 0 0 0.5 0 0 # SizeSel_1P_3_GI_NE
  -1 9 4.00 4 1 99 3 0 0 0 0 0.5 0 0 # SizeSel_1P_4_GI_NE
  -15 -5 -10 -1 1 99 -3 0 0 0 0 0.5 0 0 # SizeSel_1P_5_GI_NE
  -5 9 -0.730581 -1 1 99 3 0 0 0 0 0.5 0 0 # SizeSel_1P_6_GI_NE
  50 200 142.278 150 1 99 3 0 0 0 0 0.5 0 0 # SizeSel_2P_1_LL_NE
  -6 4 -0.316252 -3 1 99 3 0 0 0 0 0.5 0 0 # SizeSel_2P_2_LL_NE
  -1 9 6.97936 8.3 1 99 3 0 0 0 0 0.5 0 0 # SizeSel_2P_3_LL_NE
  -1 9 5.26149 4 1 99 3 0 0 0 0 0.5 0 0 # SizeSel_2P_4_LL_NE
  -15 -5 -10 -1 1 99 -3 0 0 0 0 0.5 0 0 # SizeSel_2P_5_LL_NE
  -5 9 -1.57659 -1 1 99 3 0 0 0 0 0.5 0 0 # SizeSel_2P_6_LL_NE
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_3P_1_GI_NW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_3P_2_GI_NW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_4P_1_LL_NW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_4P_2_LL_NW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_5P_1_GI_SE
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_5P_2_GI_SE
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_6P_1_LL_SE
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_6P_2_LL_SE
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_7P_1_ALGI_SW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_7P_2_ALGI_SW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_8P_1_EUEL_SW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_8P_2_EUEL_SW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_9P_1_ISEL_SW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_9P_2_ISEL_SW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_10P_1_JPLL_SW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_10P_2_JPLL_SW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_11P_1_TWFL_SW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_11P_2_TWFL_SW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_12P_1_TWLL_SW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_12P_2_TWLL_SW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_13P_1_UJPLL_NW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_13P_2_UJPLL_NW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_14P_1_UJPLL_NE
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_14P_2_UJPLL_NE
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_15P_1_UJPLL_SW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_15P_2_UJPLL_SW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_16P_1_UJPLL_SE
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_16P_2_UJPLL_SE
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_17P_1_UTWLL_NW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_17P_2_UTWLL_NW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_18P_1_UTWLL_NE
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_18P_2_UTWLL_NE
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_19P_1_UTWLL_SW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_19P_2_UTWLL_SW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_20P_1_UTWLL_SE
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_20P_2_UTWLL_SE
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_21P_1_UPOR_SW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_21P_2_UPOR_SW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_22P_1_UESP_SW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_22P_2_UESP_SW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_23P_1_URELL_SW
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_23P_2_URELL_SW
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_24P_1_UIND_NE
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_24P_2_UIND_NE
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_25P_1_UJPLL_NW_pre
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_25P_2_UJPLL_NW_pre
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_26P_1_UJPLL_NE_pre
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_26P_2_UJPLL_NE_pre
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_27P_1_UJPLL_SW_pre
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_27P_2_UJPLL_SW_pre
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_28P_1_UJPLL_SE_pre
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_28P_2_UJPLL_SE_pre
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_29P_1_UTWLL_NW_pre
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_29P_2_UTWLL_NW_pre
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_30P_1_UTWLL_NE_pre
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_30P_2_UTWLL_NE_pre
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_31P_1_UTWLL_SW_pre
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_31P_2_UTWLL_SW_pre
  -5 3 1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_32P_1_UTWLL_SE_pre
  -5 3 -1 -4 1 0.05 -3 0 0 0 0 0.5 0 0 # SizeSel_32P_2_UTWLL_SE_pre
0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 0 5 -1 99 -1 0 0 0 0 0 0 0
 0 30 30 5 -1 99 -1 0 0 0 0 0 0 0
#_Cond 0 #_custom_sel-env_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no enviro fxns
#_Cond 0 #_custom_sel-blk_setup (0/1) 
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no block usage
#_Cond No selex parm trends 
#_Cond -4 # placeholder for selparm_Dev_Phase
#_Cond 0 #_env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds; 3=standard w/ no bound check)
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
1 #_Variance_adjustments_to_input_values
 #_fleet:  1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 #_add_to_survey_CV
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 #_add_to_discard_stddev
 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 #_add_to_bodywt_CV
 # xxx CL002  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.01 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 #_mult_by_lencomp_N
   0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 #_mult_by_lencomp_N
 # xxx CL200  1. 1. 1. 1. 1. 1. 1. 1.  1.  1. 1. 1. 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 #_mult_by_lencomp_N
 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 #_mult_by_agecomp_N
 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 #_mult_by_size-at-age_N
#
4 #_maxlambdaphase
1 #_sd_offset
#
 32 # number of changes to make to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 
# 9=init_equ_catch; 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin
# lambdas like_comp fleet/survey  phase  value  sizefreq_method
 # xxx NT0 1 13 1 1 1
 # xxx NT0 1 14 1 1 1
 # xxx NT0 1 15 1 1 1
 # xxx NT0 1 16 1 1 1
 # xxx NT0 1 17 1 0.001 1
 # xxx NT0 1 18 1 0.001 1
 # xxx NT0 1 19 1 0.001 1
 # xxx NT0 1 20 1 0.001 1
 # xxx NT0 1 21 1 0.001 1
 # xxx NT0 1 22 1 0.001 1
 # xxx NT0 1 23 1 0.001 1
 # xxx NT0 1 24 1 0.001 1
 # xxx NT0 1 25 1 0.001 1
 # xxx NT0 1 26 1 0.001 1
 # xxx NT0 1 27 1 0.001 1
 # xxx NT0 1 28 1 0.001 1
 # xxx NT0 1 29 1 0.001 1
 # xxx NT0 1 30 1 0.001 1
 # xxx NT0 1 31 1 0.001 1
 # xxx NT0 1 32 1 0.001 1
 # xxx NT1 1 13 1 1 1
 # xxx NT1 1 14 1 1 1
 # xxx NT1 1 15 1 1 1
 # xxx NT1 1 16 1 1 1
 # xxx NT1 1 17 1 0.001 1
 # xxx NT1 1 18 1 0.001 1
 # xxx NT1 1 19 1 0.001 1
 # xxx NT1 1 20 1 0.001 1
 # xxx NT1 1 21 1 0.001 1
 # xxx NT1 1 22 1 0.001 1
 # xxx NT1 1 23 1 0.001 1
 # xxx NT1 1 24 1 0.001 1
 # xxx NT1 1 25 1 1 1
 # xxx NT1 1 26 1 1 1
 # xxx NT1 1 27 1 1 1
 # xxx NT1 1 28 1 1 1
 # xxx NT1 1 29 1 0.001 1
 # xxx NT1 1 30 1 0.001 1
 # xxx NT1 1 31 1 0.001 1
 # xxx NT1 1 32 1 0.001 1
 # xxx TW0 1 13 1 0.001 1
 # xxx TW0 1 14 1 0.001 1
 # xxx TW0 1 15 1 0.001 1
 # xxx TW0 1 16 1 0.001 1
 # xxx TW0 1 17 1 1 1
 # xxx TW0 1 18 1 1 1
 # xxx TW0 1 19 1 1 1
 # xxx TW0 1 20 1 1 1
 # xxx TW0 1 21 1 0.001 1
 # xxx TW0 1 22 1 0.001 1
 # xxx TW0 1 23 1 0.001 1
 # xxx TW0 1 24 1 0.001 1
 # xxx TW0 1 25 1 0.001 1
 # xxx TW0 1 26 1 0.001 1
 # xxx TW0 1 27 1 0.001 1
 # xxx TW0 1 28 1 0.001 1
 # xxx TW0 1 29 1 0.001 1
 # xxx TW0 1 30 1 0.001 1
 # xxx TW0 1 31 1 0.001 1
 # xxx TW0 1 32 1 0.001 1
 # xxx TW1 1 13 1 0.001 1
 # xxx TW1 1 14 1 0.001 1
 # xxx TW1 1 15 1 0.001 1
 # xxx TW1 1 16 1 0.001 1
 # xxx TW1 1 17 1 1 1
 # xxx TW1 1 18 1 1 1
 # xxx TW1 1 19 1 1 1
 # xxx TW1 1 20 1 1 1
 # xxx TW1 1 21 1 0.001 1
 # xxx TW1 1 22 1 0.001 1
 # xxx TW1 1 23 1 0.001 1
 # xxx TW1 1 24 1 0.001 1
 # xxx TW1 1 25 1 0.001 1
 # xxx TW1 1 26 1 0.001 1
 # xxx TW1 1 27 1 0.001 1
 # xxx TW1 1 28 1 0.001 1
 # xxx TW1 1 29 1 1 1
 # xxx TW1 1 30 1 1 1
 # xxx TW1 1 31 1 1 1
 # xxx TW1 1 32 1 1 1
  1 13 1 1 1
  1 14 1 1 1
  1 15 1 1 1
  1 16 1 1 1
  1 17 1 0.001 1
  1 18 1 0.001 1
  1 19 1 0.001 1
  1 20 1 0.001 1
  1 21 1 1 1
  1 22 1 0.001 1
  1 23 1 0.001 1
  1 24 1 0.001 1
  1 25 1 0.001 1
  1 26 1 0.001 1
  1 27 1 0.001 1
  1 28 1 0.001 1
  1 29 1 0.001 1
  1 30 1 0.001 1
  1 31 1 0.001 1
  1 32 1 0.001 1
 # xxx TWP 1 13 1 1 1
 # xxx TWP 1 14 1 1 1
 # xxx TWP 1 15 1 1 1
 # xxx TWP 1 16 1 1 1
 # xxx TWP 1 17 1 0.001 1
 # xxx TWP 1 18 1 0.001 1
 # xxx TWP 1 19 1 0.001 1
 # xxx TWP 1 20 1 0.001 1
 # xxx TWP 1 21 1 1 1
 # xxx TWP 1 22 1 0.001 1
 # xxx TWP 1 23 1 0.001 1
 # xxx TWP 1 24 1 0.001 1
 # xxx TWP 1 25 1 0.001 1
 # xxx TWP 1 26 1 0.001 1
 # xxx TWP 1 27 1 0.001 1
 # xxx TWP 1 28 1 0.001 1
 # xxx TWP 1 29 1 0.001 1
 # xxx TWP 1 30 1 0.001 1
 # xxx TWP 1 31 1 0.001 1
 # xxx TWP 1 32 1 0.001 1
 # xxx A1 1 13 1 1 1
 # xxx A1 1 14 1 1 1
 # xxx A1 1 15 1 1 1
 # xxx A1 1 16 1 1 1
 # xxx A1 1 17 1 0.25 1 
 # xxx A1 1 18 1 0.25 1
 # xxx A1 1 19 1 0.25 1 
 # xxx A1 1 20 1 0.25 1 
 # xxx A1 1 21 1 1 1
 # xxx A1 1 22 1 1 1
 # xxx A1 1 23 1 1 1
 # xxx A1 1 24 1 0.001 1
 # xxx A1 1 25 1 0.001 1
 # xxx A1 1 26 1 0.001 1
 # xxx A1 1 27 1 0.001 1
 # xxx A1 1 28 1 0.001 1
 # xxx A1 1 29 1 0.001 1
 # xxx A1 1 30 1 0.001 1
 # xxx A1 1 31 1 0.001 1
 # xxx A1 1 32 1 0.001 1
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
#
0 # (0/1) read specs for more stddev reporting 
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type  len/age  year  N selex bins  Growth pattern  N growth ages  NatAge_area(-1 for all)  NatAge_yr  N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999
