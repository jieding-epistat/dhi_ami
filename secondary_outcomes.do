//*Secondary Outcome: Emergency Department Visits
//* Analysis restricted to patients admitted to JHH or JHBMC (n=908)
//* Cox regression analysis of association with 30-day ED visit based on imputed data sets 
    
///*load data
use "S:\Corrie\Jennifer's Files\MICORE_final_2020\Final Data\redcap_controls_1064_f_s_04102020_como_noimput.dta", clear

///*patients admitted to JHH/JHBMC	
keep if hospital_gp_index_4==1|hospital_gp_index_4==2 
	
///*derive new ED visit variable, which only focused on the visits that did not result in subsequent hospitalization
gen ed_status_no_hosp=0
replace ed_status_no_hosp=1 if readmi_status_30==0 &  ed_status==1
replace ed_status_no_hosp=1 if !missing(days_ed_readmit) & days_ed_readmit<=-5


///*Imputation phase using chained equations/MICE (Aka the fully conditional specification or sequential generalized regression)
              mi set flong      
              mi register imputed white smok_status_2 married comobid_no_i_3 ln_los_days_2
              mi stset, clear
              mi impute chained (logit) white smok_status_2 married (mlogit) comobid_no_i_3 (regress) ln_los_days_2 = age sex STEMI_diag index_primary_payer_2_grp dis_destin_mis_2_grp grp_case_control ed_status_no_hosp index_CABG_PCI_adm_2 hospital_gp_index_4, add(10) rseed (53421) savetrace(trace2,replace)     
		             ////* Imputation Diagnostics
					     //////* using the means and standard deviations of imputed values from each iteration saved in a Stata dataset named “trace1”.
                    use "S:\Corrie\Jennifer's Files\MICORE_final_2020\Final Data\trace2.dta", clear
					           //////* wide format
                    reshape wide *mean *sd, i(iter) j(m)  
					          //////* Trace plots of summaries of imputed values 
tsset iter
tsline white_mean*, name(mice1,replace)legend(off) ytitle("Mean of White")
tsline white_sd*, name(mice2, replace) legend(off) ytitle("SD of White")
graph combine mice1 mice2, xcommon cols(1) title(Trace plots of summaries of imputed values)	

tsline smok_status_2_mean*, name(mice1,replace)legend(off) ytitle("Mean of Current smoker")
tsline smok_status_2_sd*, name(mice2, replace) legend(off) ytitle("SD of Current smoker")
graph combine mice1 mice2, xcommon cols(1) title(Trace plots of summaries of imputed values)	

tsline married_mean*, name(mice1,replace)legend(off) ytitle("Mean of Married")
tsline married_sd*, name(mice2, replace) legend(off) ytitle("SD of Married")
graph combine mice1 mice2, xcommon cols(1) title(Trace plots of summaries of imputed values)	

tsline comobid_no_i_3_mean*, name(mice1,replace)legend(off) ytitle("Mean of comobid burden")
tsline comobid_no_i_3_sd*, name(mice2, replace) legend(off) ytitle("SD of comobid burden")
graph combine mice1 mice2, xcommon cols(1) title(Trace plots of summaries of imputed values)	
					
					
///*Analytical phase 		
					
			////* Calculation of propensity score		
mi estimate, saving(miest,replace): logistic grp_case_control age sex white STEMI_diag index_CABG_PCI_adm_2 smok_status_2 index_primary_payer_2_grp married dis_destin_mis_2_grp i.comobid_no_i_3 ln_los_days_2	
		       ////*MI estimates of linear predictions for all (11) data sets 
 mi predict xb_mi_all using miest, storecompleted 
               ////*apply the inverse-logit transformation to obtain the probabilities
 quietly mi xeq: generate propensity_i = invlogit(xb_mi_all) 
              ////* create quintiles of propensity score  
quietly mi xeq: xtile pq_5_i=propensity_i,n(5)           
			  ////*generate dummy variable at each dataset
 quietly mi xeq: tabulate comobid_no_i_3, generate(dum)
             ////*check the balance of baseline characteristics according to 5 strata
 mpbalchk grp_case_control age sex white STEMI_diag index_CABG_PCI_adm_2 smok_status_2 index_primary_payer_2_grp married dis_destin_mis_2_grp dum2 dum3 ln_los_days_2, strata(pq_5_i)
          ////* Cox regression
 mi stset daystoedvisit, failure(ed_status_no_hosp)
 mi estimate, hr: stcox grp_case_control i.hospital_gp_index_4 i.pq_5_i
 
 
 
 
//*Secondary Outcome: Corrie user group analysis
//* Analysis restricted to patients in the Corrie user group (n=200)
 
 ///*load data 
 use "S:\Corrie\Jennifer's Files\MICORE_final_2020\Final Data\redcap_200.dta", clear
 
 ///*Attendance of Follow-up Appointments and Cardiac Rehabilitation*///
 tab readmission_complete //*Corrie patients who completed the 30-day survey on attending follow-up appointments (n=104)
  Complete? |      Freq.     Percent        Cum.
------------+-----------------------------------
 Incomplete |         96       48.00       48.00
   Complete |        104       52.00      100.00
------------+-----------------------------------
      Total |        200      100.00
///*
“followup___1” = primary care provider
“followup___2” = cardiologist
“followup___3” = cardiac rehab *///

tab followup___1 //*primary care provider
In the past |
    30 days |
   have you |
 attended a |
  follow-up |
appointment |
with any of |
     the fo |      Freq.     Percent        Cum.
------------+-----------------------------------
  Unchecked |        116       58.00       58.00
    Checked |         84       42.00      100.00
------------+-----------------------------------
      Total |        200      100.00
	  
gen followup___1_mis=followup___1
replace followup___1_mis=. if readmission_complete==0
tab followup___1_mis, mis
followup___ |
      1_mis |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |         20       10.00       10.00
          1 |         84       42.00       52.00
          . |         96       48.00      100.00
------------+-----------------------------------
      Total |        200      100.00



tab followup___2 //*cardiologist

In the past |
    30 days |
   have you |
 attended a |
  follow-up |
appointment |
with any of |
     the fo |      Freq.     Percent        Cum.
------------+-----------------------------------
  Unchecked |        114       57.00       57.00
    Checked |         86       43.00      100.00
------------+-----------------------------------
      Total |        200      100.00

gen followup___2_mis=followup___2
replace followup___2_mis=. if readmission_complete==0
tab followup___2_mis, mis	  
followup___ |
      2_mis |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |         18        9.00        9.00
          1 |         86       43.00       52.00
          . |         96       48.00      100.00
------------+-----------------------------------
      Total |        200      100.00
	  
	  
tab followup___3 //*cardiac rehab

In the past |
    30 days |
   have you |
 attended a |
  follow-up |
appointment |
with any of |
     the fo |      Freq.     Percent        Cum.
------------+-----------------------------------
  Unchecked |        153       76.50       76.50
    Checked |         47       23.50      100.00
------------+-----------------------------------
      Total |        200      100.00

gen followup___3_mis=followup___3
replace followup___3_mis=. if readmission_complete==0
tab followup___3_mis, mis
followup___ |
      3_mis |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |         57       28.50       28.50
          1 |         47       23.50       52.00
          . |         96       48.00      100.00
------------+-----------------------------------
      Total |        200      100.00


gen followup_1_2_3_z=followup___1_mis + followup___2_mis + followup___3_mis
tab followup_1_2_3_z

followup_1_ |
      2_3_z |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |          3        2.88        2.88
          1 |         19       18.27       21.15
          2 |         48       46.15       67.31
          3 |         34       32.69      100.00
------------+-----------------------------------
      Total |        104      100.00


///*Perceived app usability*///  

///* calculate 3-day post discharge
////* Select those admitted after Nov. 16, 2017
gen index_perceived_app_usability=0
replace index_perceived_app_usability=1 if (ad_date>=td(16nov2017)) & !missing(ad_date)

count if (ad_date>=td(16nov2017)) & !missing(ad_date)
134

codebook like_app app_complexity easy_use technical_support inconsistency  well_integrated  learn_quickly  cumbersome_use confidence_app  learning_curve if index_perceived_app_usability==1

--------------------------------------------------------------------------------------------------------------
like_app                                                I think I would like to use the Corrie app frequently.
--------------------------------------------------------------------------------------------------------------

                  type:  numeric (byte)
                 label:  like_app_

                 range:  [2,5]                        units:  1
         unique values:  4                        missing .:  57/134

            tabulation:  Freq.   Numeric  Label
                             5         2  Disagree
                            17         3  Neither disagree nor agree
                            36         4  Agree
                            19         5  Strongly agree
                            57         .  

--------------------------------------------------------------------------------------------------------------
app_complexity                                                    I find the Corrie app unnecessarily complex.
--------------------------------------------------------------------------------------------------------------

                  type:  numeric (byte)
                 label:  app_complexity_

                 range:  [1,5]                        units:  1
         unique values:  5                        missing .:  56/134

            tabulation:  Freq.   Numeric  Label
                            11         1  Strongly disagree
                            28         2  Disagree
                            21         3  Neither disagree nor agree
                            17         4  Agree
                             1         5  Strongly agree
                            56         .  

--------------------------------------------------------------------------------------------------------------
easy_use                                                                I think the Corrie app is easy to use.
--------------------------------------------------------------------------------------------------------------

                  type:  numeric (byte)
                 label:  easy_use_

                 range:  [2,5]                        units:  1
         unique values:  4                        missing .:  55/134

            tabulation:  Freq.   Numeric  Label
                            12         2  Disagree
                            17         3  Neither disagree nor agree
                            39         4  Agree
                            11         5  Strongly agree
                            55         .  

--------------------------------------------------------------------------------------------------------------
technical_support             I think that I would need the support of a technical person to be able to use th
--------------------------------------------------------------------------------------------------------------

                  type:  numeric (byte)
                 label:  technical_support_

                 range:  [1,5]                        units:  1
         unique values:  5                        missing .:  55/134

            tabulation:  Freq.   Numeric  Label
                            17         1  Strongly disagree
                            28         2  Disagree
                            19         3  Neither disagree nor agree
                            13         4  Agree
                             2         5  Strongly agree
                            55         .  

--------------------------------------------------------------------------------------------------------------
inconsistency                                       I think there is too much inconsistency in the Corrie app.
--------------------------------------------------------------------------------------------------------------

                  type:  numeric (byte)
                 label:  inconsistency_

                 range:  [1,5]                        units:  1
         unique values:  5                        missing .:  55/134

            tabulation:  Freq.   Numeric  Label
                            15         1  Strongly disagree
                            31         2  Disagree
                            21         3  Neither disagree nor agree
                             9         4  Agree
                             3         5  Strongly agree
                            55         .  

--------------------------------------------------------------------------------------------------------------
well_integrated                          I find the various functions in the Corrie app to be well integrated.
--------------------------------------------------------------------------------------------------------------

                  type:  numeric (byte)
                 label:  well_integrated_

                 range:  [2,5]                        units:  1
         unique values:  4                        missing .:  58/134

            tabulation:  Freq.   Numeric  Label
                             8         2  Disagree
                            18         3  Neither disagree nor agree
                            40         4  Agree
                            10         5  Strongly agree
                            58         .  

--------------------------------------------------------------------------------------------------------------
learn_quickly                 I would imagine that most people would learn to use the Corrie app very quickly.
--------------------------------------------------------------------------------------------------------------

                  type:  numeric (byte)
                 label:  learn_quickly_

                 range:  [1,5]                        units:  1
         unique values:  5                        missing .:  55/134

            tabulation:  Freq.   Numeric  Label
                             1         1  Strongly disagree
                             8         2  Disagree
                            18         3  Neither disagree nor agree
                            45         4  Agree
                             7         5  Strongly agree
                            55         .  

--------------------------------------------------------------------------------------------------------------
cumbersome_use                                                   I find the Corrie app very cumbersome to use.
--------------------------------------------------------------------------------------------------------------

                  type:  numeric (byte)
                 label:  cumbersome_use_

                 range:  [1,5]                        units:  1
         unique values:  5                        missing .:  56/134

            tabulation:  Freq.   Numeric  Label
                            14         1  Strongly disagree
                            28         2  Disagree
                            21         3  Neither disagree nor agree
                            13         4  Agree
                             2         5  Strongly agree
                            56         .  

--------------------------------------------------------------------------------------------------------------
confidence_app                                                     I feel very confident using the Corrie app.
--------------------------------------------------------------------------------------------------------------

                  type:  numeric (byte)
                 label:  confidence_app_

                 range:  [1,5]                        units:  1
         unique values:  5                        missing .:  55/134

            tabulation:  Freq.   Numeric  Label
                             1         1  Strongly disagree
                            10         2  Disagree
                            13         3  Neither disagree nor agree
                            42         4  Agree
                            13         5  Strongly agree
                            55         .  

--------------------------------------------------------------------------------------------------------------
learning_curve                 I needed to learn a lot of things before I could get going with the Corrie app.
--------------------------------------------------------------------------------------------------------------

                  type:  numeric (byte)
                 label:  learning_curve_

                 range:  [1,5]                        units:  1
         unique values:  5                        missing .:  55/134

            tabulation:  Freq.   Numeric  Label
                            13         1  Strongly disagree
                            29         2  Disagree
                            14         3  Neither disagree nor agree
                            20         4  Agree
                             3         5  Strongly agree
                            55         .  



codebook like_app app_complexity easy_use technical_support inconsistency  well_integrated  learn_quickly  cumbersome_use confidence_app  learning_curve


Freq.   Numeric  Label
                             5         2  Disagree
                            17         3  Neither disagree nor agree
                            36         4  Agree
 
                           19         5  Strongly agree
                            57         .  
////* assigning the contribution to each item
////*Each item's score contribution will range from 0 to 4. 
////*For items 1,3,5,7,and 9 the score contribution is the scale position minus 1. 
////*For items 2,4,6,8 and 10, the contribution is 5 minus the scale position. 
////*Multiply the sum of the scores by 2.5 to obtain the overall value of SU.
														
gen item_1=.
replace item_1=like_app-1
gen item_2=.
replace item_2=5-app_complexity 
gen item_3=.
replace item_3=easy_use -1
gen item_4=.
replace item_4=5-technical_support
gen item_5=.
replace item_5=inconsistency -1
gen item_6=.
replace item_6=5-well_integrated 
gen item_7=.
replace item_7=learn_quickly-1
gen item_8=.
replace item_8=5-cumbersome_use 
gen item_9=.
replace item_9=confidence_app-1
gen item_10=.
replace item_10=5-learning_curve


////* calculating the count of available values per patient
////* 1st generating an index of count of nomissing values
gen count_day_3=0
foreach var of varlist item_1-item_10 {
gen nomis_`var'=.
replace nomis_`var'=1 if !missing(`var')
replace count_day_3=count_day_3+nomis_`var' if inlist(nomis_`var',1)
}

tab count_day_3

count_day_3 |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |        113       56.50       56.50
          8 |          1        0.50       57.00
          9 |          5        2.50       59.50
         10 |         81       40.50      100.00
------------+-----------------------------------
      Total |        200      100.00


////*create scored SUS scale for those with at least 8 out of 10 items completed
////* for those with some values, they all had at least 8 out of 10 items completed
////* Sum up the row total directly 

egen totalscore_day_3=rowtotal(item_1 item_2 item_3 item_4 item_5 item_6 item_7 item_8 item_9 item_10)

tab totalscore_day_3

totalscore_ |
      day_3 |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |        113       56.50       56.50
         12 |          1        0.50       57.00
         14 |          1        0.50       57.50
         15 |          1        0.50       58.00
         16 |          4        2.00       60.00
         17 |          4        2.00       62.00
         18 |          3        1.50       63.50
         19 |          5        2.50       66.00
         20 |          2        1.00       67.00
         21 |          6        3.00       70.00
         22 |          7        3.50       73.50
         23 |          9        4.50       78.00
         24 |          6        3.00       81.00
         25 |         13        6.50       87.50
         26 |          4        2.00       89.50
         27 |          4        2.00       91.50
         28 |          5        2.50       94.00
         29 |          2        1.00       95.00
         30 |          4        2.00       97.00
         31 |          2        1.00       98.00
         32 |          3        1.50       99.50
         33 |          1        0.50      100.00
------------+-----------------------------------
      Total |        200      100.00

replace totalscore_day_3=. if totalscore_day_3==0

gen s_totalscore_day_3=totalscore_day_3*2.5
summarize s_totalscore_day_3
summarize s_totalscore_day_3, detail

                     s_totalscore_day_3
-------------------------------------------------------------
      Percentiles      Smallest
 1%           30             30
 5%           40             35
10%         42.5           37.5       Obs                  87
25%         52.5             40       Sum of Wgt.          87

50%           60                      Mean           58.59195
                        Largest       Std. Dev.      11.48622
75%           65             80
90%           75             80       Variance       131.9333
95%         77.5             80       Skewness      -.1066052
99%         82.5           82.5       Kurtosis       2.579821





///* calculate 30-day post discharge
////* assigning the contribution to each item
////*Each item's score contribution will range from 0 to 4. 
////*For items 1,3,5,7,and 9 the score contribution is the scale position minus 1. 
////*For items 2,4,6,8 and 10, the contribution is 5 minus the scale position. 
////*Multiply the sum of the scores by 2.5 to obtain the overall value of SU.
							
gen item_1_v2=.
replace item_1_v2=like_app_v2-1
gen item_2_v2=.
replace item_2_v2=5-app_complexity_v2 
gen item_3_v2=.
replace item_3_v2=easy_use_v2 -1
gen item_4_v2=.
replace item_4_v2=5-technical_support_v2
gen item_5_v2=.
replace item_5_v2=inconsistency_v2 -1
gen item_6_v2=.
replace item_6_v2=5-well_integrated_v2 
gen item_7_v2=.
replace item_7_v2=learn_quickly_v2-1
gen item_8_v2=.
replace item_8_v2=5-cumbersome_use_v2 
gen item_9_v2=.
replace item_9_v2=confidence_app_v2-1
gen item_10_v2=.
replace item_10_v2=5-learning_curve_v2


////* calculate the count of available values per patient
////* 1st generating an index of count of nomissing values
gen count_day_30=0
foreach var of varlist item_1_v2-item_10_v2 {
gen nomis_`var'=.
replace nomis_`var'=1 if !missing(`var')
replace count_day_30=count_day_30+nomis_`var' if inlist(nomis_`var',1)
}

tab count_day_30


count_day_3 |
          0 |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |         96       48.00       48.00
          8 |          1        0.50       48.50
          9 |          4        2.00       50.50
         10 |         99       49.50      100.00
------------+-----------------------------------
      Total |        200      100.00


////*create scored SUS scale for those with at least 8 out of 10 items completed
////* for those with some values, they all had at least 8 out of 10 items completed
////* Sum up the row total directly 

egen totalscore_day_30=rowtotal(item_1_v2 item_2_v2 item_3_v2 item_4_v2 item_5_v2 item_6_v2 item_7_v2 item_8_v2 item_9_v2 item_10_v2)

 tab totalscore_day_30
totalscore_ |
     day_30 |      Freq.     Percent        Cum.
------------+-----------------------------------
          0 |         96       48.00       48.00
         13 |          3        1.50       49.50
         14 |          1        0.50       50.00
         15 |          2        1.00       51.00
         16 |          4        2.00       53.00
         17 |          5        2.50       55.50
         18 |          5        2.50       58.00
         19 |          3        1.50       59.50
         20 |         10        5.00       64.50
         21 |          6        3.00       67.50
         22 |          8        4.00       71.50
         23 |          7        3.50       75.00
         24 |          5        2.50       77.50
         25 |          8        4.00       81.50
         26 |         13        6.50       88.00
         27 |          4        2.00       90.00
         28 |          5        2.50       92.50
         29 |          8        4.00       96.50
         30 |          3        1.50       98.00
         31 |          3        1.50       99.50
         32 |          1        0.50      100.00
------------+-----------------------------------
      Total |        200      100.00


replace totalscore_day_30=. if totalscore_day_30==0

gen s_totalscore_day_30=totalscore_day_30*2.5
summarize s_totalscore_day_30, detail

                     s_totalscore_day_30
-------------------------------------------------------------
      Percentiles      Smallest
 1%         32.5           32.5
 5%         37.5           32.5
10%         42.5           32.5       Obs                 104
25%           50             35       Sum of Wgt.         104

50%         57.5                      Mean           57.57212
                        Largest       Std. Dev.       11.6349
75%           65           77.5
90%         72.5           77.5       Variance        135.371
95%           75           77.5       Skewness      -.2146675
99%         77.5             80       Kurtosis       2.249349









