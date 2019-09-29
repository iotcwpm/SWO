# io4_NTP_GaMf_r2_h75_CL020
#V3.21b - not correct, just want it to run without failure first...
#
# for all year entries except rebuilder; enter either: actual year, -999 for styr, 0 for endyr, neg number for rel. endyr
1 # Benchmarks: 0=skip; 1=calc F_spr,F_btgt,F_msy 
2 # MSY: 1= set to F(SPR); 2=calc F(MSY); 3=set to F(Btgt); 4=set to F(endyr) 
0.4 # SPR target (e.g. 0.40)
0.4 # Biomass target (e.g. 0.40)
#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF (enter actual year, or values of 0 or -integer to be rel. endyr)
 0 0 0 0 0 0
#  2015 2015 2015 2015 2015 2015 # after processing 
1 #Bmark_relF_Basis: 1 = use year range; 2 = set relF same as forecast below
#
0 # Forecast: 0=none; 1=F(SPR); 2=F(MSY) 3=F(Btgt); 4=Ave F (uses first-last relF yrs); 5=input annual F scalar
10 # N forecast years 
0.2 # F scalar (only used for Do_Forecast==5)
#_Fcast_years:  beg_selex, end_selex, beg_relF, end_relF  (enter actual year, or values of 0 or -integer to be rel. endyr)
 0 0 0 0
#  1995 2001 1991 2001 # after processing 
0 # Control rule method (1=catch=f(SSB) west coast; 2=F=f(SSB) ) 
0.4 # Control rule Biomass level for constant F (as frac of Bzero, e.g. 0.40) 
0.1 # Control rule Biomass level for no F (as frac of Bzero, e.g. 0.10) 
0.75 # Control rule target as fraction of Flimit (e.g. 0.75) 
3 #_N forecast loops (1-3) (fixed at 3 for now)
3 #_First forecast loop with stochastic recruitment
0 #_Forecast loop control #3 (reserved for future bells&whistles) 
0 #_Forecast loop control #4 (reserved for future bells&whistles) 
0 #_Forecast loop control #5 (reserved for future bells&whistles) 
2500  #FirstYear for caps and allocations (should be after years with fixed inputs) 
0. # stddev of log(realized catch/target catch) in forecast (set value>0.0 to cause active impl_error)
0 # Do West Coast gfish rebuilder output (0/1) 
1999 # Rebuilder:  first year catch could have been set to zero (Ydecl)(-1 to set to 1999)
-1 # Rebuilder:  year for current age structure (Yinit) (-1 to set to endyear+1)
1 # fleet relative F:  1=use first-last alloc year; 2=read seas(row) x fleet(col) below
# Note that fleet allocation is used directly as average F if Do_Forecast=4 
2 # basis for fcast catch tuning and for fcast catch caps and allocation  (2=deadbio; 3=retainbio; 5=deadnum; 6=retainnum)
# Conditional input if relative F choice = 2
# Fleet relative F:  rows are seasons, columns are fleets
#_Fleet:  FISHERY1 FISHERY2 FISHERY3
#  0.109302 0.109302 0.109302
#  0.110482 0.110482 0.110482
#  0.113549 0.113549 0.113549
# max totalcatch by fleet (-1 to have no max) must enter value for each fleet
 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 
# max totalcatch by area (-1 to have no max); must enter value for each fleet ...area?
 -1 -1 -1 -1
# fleet assignment to allocation group (enter group ID# for each fleet, 0 for not included in an alloc group)
 0 0 0 0 0 0 0 0 0 0 0 0
#_Conditional on >1 allocation group
# allocation fraction for each of: 2 allocation groups
# 0.7 0.3 0.1 0.1
#160 # Number of forecast catch levels to input (else calc catch from forecast F) 
#2 # basis for input Fcast catch:  2=dead catch; 3=retained catch; 99=input Hrate(F) (units are from fleetunits; note new codes in SSV3.20)
# Input fixed catch values
###Year Seas Fleet Catch
 110  # Number of forecast catch levels to input (rest calc catch from forecast F
2 # basis for input Fcast catch:  2=dead catch; 3=retained catch; 99=input Hrate(F) (units are from fleetunits; note new codes in SSV3.20)
##
##Year Seas Fleet Catch
 2016	1	1	1442
 2016	1	2	11460
 2016	1	3	2414
 2016	1	4	8221
 2016	1	5	0
 2016	1	6	4886
 2016	1	8	2107
 2016	1	9	780
 2016	1	10	210
 2016	1	11	465
 2016	1	12	144
 2017	1	1	1442
 2017	1	2	11460
 2017	1	3	2414
 2017	1	4	8221
 2017	1	5	0
 2017	1	6	4886
 2017	1	8	2107
 2017	1	9	780
 2017	1	10	210
 2017	1	11	465
 2017	1	12	144
 2018	1	1	1442
 2018	1	2	11460
 2018	1	3	2414
 2018	1	4	8221
 2018	1	5	0
 2018	1	6	4886
 2018	1	8	2107
 2018	1	9	780
 2018	1	10	210
 2018	1	11	465
 2018	1	12	144
 2019	1	1	1442
 2019	1	2	11460
 2019	1	3	2414
 2019	1	4	8221
 2019	1	5	0
 2019	1	6	4886
 2019	1	8	2107
 2019	1	9	780
 2019	1	10	210
 2019	1	11	465
 2019	1	12	144
 2020	1	1	1442
 2020	1	2	11460
 2020	1	3	2414
 2020	1	4	8221
 2020	1	5	0
 2020	1	6	4886
 2020	1	8	2107
 2020	1	9	780
 2020	1	10	210
 2020	1	11	465
 2020	1	12	144
 2021	1	1	1442
 2021	1	2	11460
 2021	1	3	2414
 2021	1	4	8221
 2021	1	5	0
 2021	1	6	4886
 2021	1	8	2107
 2021	1	9	780
 2021	1	10	210
 2021	1	11	465
 2021	1	12	144
 2022	1	1	1442
 2022	1	2	11460
 2022	1	3	2414
 2022	1	4	8221
 2022	1	5	0
 2022	1	6	4886
 2022	1	8	2107
 2022	1	9	780
 2022	1	10	210
 2022	1	11	465
 2022	1	12	144
 2023	1	1	1442
 2023	1	2	11460
 2023	1	3	2414
 2023	1	4	8221
 2023	1	5	0
 2023	1	6	4886
 2023	1	8	2107
 2023	1	9	780
 2023	1	10	210
 2023	1	11	465
 2023	1	12	144
 2024	1	1	1442
 2024	1	2	11460
 2024	1	3	2414
 2024	1	4	8221
 2024	1	5	0
 2024	1	6	4886
 2024	1	8	2107
 2024	1	9	780
 2024	1	10	210
 2024	1	11	465
 2024	1	12	144
 2025	1	1	1442
 2025	1	2	11460
 2025	1	3	2414
 2025	1	4	8221
 2025	1	5	0
 2025	1	6	4886
 2025	1	8	2107
 2025	1	9	780
 2025	1	10	210
 2025	1	11	465
 2025	1	12	144
999 # verify end of input
