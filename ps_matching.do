//*senstivity analysis: Propensity score-matched analysis using imputed datasets

///*load data
use "S:\Corrie\Jennifer's Files\MICORE_final_2020\Final Data\redcap_controls_1064_f_s_04102020_como_noimput.dta", clear	  

///*Imputation phase using chained equations/MICE (Aka the fully conditional specification or sequential generalized regression)
 mi set flong      
 mi register imputed white smok_status_2 married comobid_no_i_3 ln_los_days_2
 mi stset, clear
 mi impute chained (logit) white smok_status_2 married (mlogit) comobid_no_i_3 (regress) ln_los_days_2 = age sex STEMI_diag index_primary_payer_2_grp dis_destin_mis_2_grp grp_case_control readmi_status_30 index_CABG_PCI_adm_2 hospital_gp_index_4, add(10) rseed (53421) savetrace(trace1,replace)     

///* Calculation of propensity score
mi estimate, saving(miest,replace): logistic grp_case_control age sex white STEMI_diag index_CABG_PCI_adm_2 smok_status_2 index_primary_payer_2_grp married dis_destin_mis_2_grp i.comobid_no_i_3 ln_los_days_2
 
///*MI estimates of linear predictions for all data sets 
mi predict xb_mi_all using miest, storecompleted 

///*greedy matching ratio 2:1 with a caliper at each dataset
///*maximum caliber equal to 0.2 times the pooled standard deviation of the logit of propensity score
quietly mi xeq: gmatch grp_case_control xb_mi_all, set (set1) maxc(2) diff(diff1) cal(0.3) 


///*create dummy variable at each dataset
 quietly mi xeq: tabulate comobid_no_i_3, generate(dum)
 
 
///* check the balance
 mpbalchk grp_case_control age sex white STEMI_diag index_CABG_PCI_adm_2 smok_status_2 index_primary_payer_2_grp married dis_destin_mis_2_grp dum2 dum3 ln_los_days_2, strata(set1)


///* marginal Cox				  
mi stset DaystoReAdmit_2 , failure(readmi_status_30)
mi estimate, esampvaryok hr: stcox i.grp_case_control, vce(cluster set1)			  
		  
				  
	