//*stata codes for "A Digital Health Intervention in Acute Myocardial Infarction"

///*load data
use "S:\Corrie\Jennifer's Files\MICORE_final_2020\Final Data\redcap_controls_1064_f_s_04102020_como_noimput.dta", clear

///*setup 1: Send Stata output to MS Word
ssc install asdoc

///*setup 2: install “propensity”  & "pbalchk" command
net from http://personalpages.manchester.ac.uk/staff/mark.lunt



///*Table 1. Baseline Characteristics: Corrie and Historical Control Groups
   
   ////*independent t-test or Mann-Whitney test for continuous variables
foreach var of varlist age bmi_mean{
asdoc by grp_case_control: summarize `var'
asdoc ttest `var', by(grp_case_control)
}	  
 
bysort grp_case_control: asdoc tabstat LOS_Days_2, stat(N mean sd min max p25 p50 p75) 
asdoc ranksum LOS_Days_2, by(grp_case_control)
 
 
  ////*Chi-square or Fisher exact test for binary and categorical variables 
foreach var of varlist sex white index_primary_payer_4 married smok_status_4 hyp_all obes_all dm_all chf_all lung_all pvd_all rf_all cancer_all depres_all comobid_no_i_3 hospital_gp_index_4 STEMI_diag index_cabg_pci_adm_4 dis_destin_mis{
asdoc tabulate `var', col 
asdoc tabulate `var' grp_case_control, col chi2 
asdoc, text(Chi2 = `r(chi2)'  Pr = `r(p)')
}	

asdoc tabulate index_cabg_pci_adm_4 grp_case_control, col exact
asdoc tabulate dis_destin_mis grp_case_control, col exact


///*Figure 4 (Panel B)
///*Nelson-Aalen estimates of the cumulative proportion of patients with 30-day readmission post discharge

stset DaystoReAdmit_2, failure(readmi_status_30)
sts graph, xlabel(0(5)30) by(grp_case_control) cumhaz risktable (, order(1 "Historical control" 2 "Corrie")) legend(ring(0) position(3) rows(2))


///* Cox regression analysis of association with 30-day readmission based on imputed data sets
          ////* Step 1. Preparing to conduct multiple imputation
		           /////* natural log transformation of index hospitalization length of stay
                      gen ln_los_days_2=ln(LOS_Days_2)
		  /*Examine the number and proportion of missing values*/ 
		  mdesc white smok_status_2 married comobid_no_i_3 ln_los_days_2
		  /*Examine Missing Data Pattern among the 5 variables*/
		  mi set mlong
		  mi misstable summarize white smok_status_2 married comobid_no_i_3 ln_los_days_2
          mi misstable patterns white smok_status_2 married comobid_no_i_3 ln_los_days_2
         /*Identify potential auxiliary variables associated with missingness*/
		  tab comobid_no_i_3, gen(comobid_no_i_3_c)
          pwcorr white smok_status_2 married comobid_no_i_3_c1 comobid_no_i_3_c2 ln_los_days_2 age STEMI_diag, obs 

		  
		  
	    ///*reload data
use "S:\Corrie\Jennifer's Files\MICORE_final_2020\Final Data\redcap_controls_1064_f_s_04102020_como_noimput.dta", clear	  
		////* Step 2. Imputation phase using chained equations/MICE (Aka the fully conditional specification or sequential generalized regression)
              mi set flong      
              mi register imputed white smok_status_2 married comobid_no_i_3 ln_los_days_2
              mi stset, clear
              mi impute chained (logit) white smok_status_2 married (mlogit) comobid_no_i_3 (regress) ln_los_days_2 = age sex STEMI_diag index_primary_payer_2_grp dis_destin_mis_2_grp grp_case_control readmi_status_30 index_CABG_PCI_adm_2 hospital_gp_index_4, add(10) rseed (53421) savetrace(trace1,replace)     
		             ////* Imputation Diagnostics
					     //////* using the means and standard deviations of imputed values from each iteration saved in a Stata dataset named “trace1”.
                    use S:\Corrie\Jennifer's Files\MICORE_final_2020\Final Data\trace1.dta", clear
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
					

					
	////* Step 3. Analytical phase 		
					
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
 mi stset DaystoReAdmit_2, failure(readmi_status_30)
 mi estimate, hr: stcox grp_case_control i.hospital_gp_index_4 i.pq_5_i
 

 
 
 
 
///* Cox regression analysis of association with 30-day readmission: complete case analysis using original dataset

       ////*load data
use "S:\Corrie\Jennifer's Files\MICORE_final_2020\Final Data\redcap_controls_1064_f_s_04102020_como_noimput.dta", clear
      
	  
	  ////* Calculation of propensity score
 logistic grp_case_control age sex white STEMI_diag index_CABG_PCI_adm_2 smok_status_2 index_primary_payer_2_grp married dis_destin_mis_2_grp i.comobid_no_i_3 ln_los_days_2
           /////* diagnositics for the propensity score
          /////* Hosmer-Lemeshow test: non-significance indicating the logistic regress model fits the data well
      estat gof, group(5) table 
	     /////* given good model fit, saving propensity score & linear predictor from the model
    predict ps
    predict lp, xb			  
	    /////* plot with the log odds of the propensity scores
graph tw kdensity lp if grp_case_control==0 || kdensity lp if grp_case_control==1, legend(label(1 "control")label(2 "treated"))xtitle("propensity score")			  
       /////* create quintiles of propensity score 
xtile pq=ps,n(5)
tab pq grp_case_control, ro
      /////*generate dummy var for comobidity 3 categories
 tabulate comobid_no_i_3, generate(dum)
     /////*check balance
pbalchk grp_case_control age sex white STEMI_diag index_CABG_PCI_adm_2 smok_status_2 index_primary_payer_2_grp married dis_destin_mis_2_grp dum2 dum3 ln_los_days_2, strata(pq)   
		   
 
   ////* Cox regression
 stset DaystoReAdmit_2, failure(readmi_status_30)
     /////*Univariate analyses  
sts test grp_case_control, logrank   /*logrank test of equality*/
sts graph, by(grp_case_control) 

sts test hospital_gp_index_4, logrank
sts graph, by(hospital_gp_index_4) 

sts test pq, logrank
sts graph, by(pq) 
    /////*main model
stcox grp_case_control i.hospital_gp_index_4 i.pq

   /////*test for interaction
stcox i.grp_case_control#i.hospital_gp_index_4 i.pq, nohr
stcox i.grp_case_control i.hospital_gp_index_4#i.pq, nohr
stcox i.grp_case_control#i.pq i.hospital_gp_index_4 , nohr

  /////*Proportionality Assumption
  ////*use the Schoenfeld and scaled Schoenfeld residuals 
 quietly stcox grp_case_control i.hospital_gp_index_4 i.pq, schoenfeld(sch*) scaledsch(sca*)
 stphtest, detail //*p value >0.05; no violation of PH assumption

  stphtest, plot(grp_case_control) msym(oh) //*A horizontal line in the graph of the scaled Schoenfeld assumption; no violation of PH
  stphtest, plot(2.hospital_gp_index_4) msym(oh)
  stphtest, plot(3.hospital_gp_index_4) msym(oh)
  stphtest, plot(4.hospital_gp_index_4) msym(oh)
  stphtest, plot(2.pq) msym(oh)
  stphtest, plot(3.pq) msym(oh)
  stphtest, plot(4.pq) msym(oh)
  stphtest, plot(5.pq) msym(oh)
  
  stphplot, by(grp_case_control) plot1(msym(oh)) plot2(msym(th)) //* log-log plots 
  stphplot, by(hospital_gp_index_4) plot1(msym(oh)) plot2(msym(th))
  stphplot, by(pq) plot1(msym(oh)) plot2(msym(th))

  drop sch1-sch10 sca1-sca10

  
  /////*Goodness of Fit of the Final Model

quietly stcox grp_case_control i.hospital_gp_index_4 i.pq, nohr mgale(mg)
predict cs, csnell

stset cs, failure(readmi_status_30)
sts generate H = na
line H cs cs, sort xlab(0 1 to 4) ylab(0 1 to 4)
drop mg
 
 
 
 
 
