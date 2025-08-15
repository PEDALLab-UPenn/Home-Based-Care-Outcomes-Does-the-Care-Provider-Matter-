
********************************************************************************
* This file checks balance for IVs- Appendix Table 1 * 
* Code written by CS *
********************************************************************************

/* GENERAL NOTES: 

- This do-file generates standardized differences

INPUT: 	"$createddata\regressiondata_fnl_multi_nomedhh_sibkidsiv.dta"

*/

clear
clear matrix
capture log close
set more off
set scheme s1color
set mem 1000m



********************************************************************************
						*CHECK BALANCE* 
********************************************************************************
use "$createddata\regressiondata_fnl_multi_nomedhh_sibkidsiv.dta", clear
keep if t_sample==1 

ta age_band, gen(dage_band)
ta marital_stat, gen(dmarital_stat)
ta networth_pctl, gen(dnetworth_pctl)
ta rshlt, gen(drshlt)
ta rarelig, gen(drarelig)
ta drink_cat, gen(ddrink_cat)
ta bmi_cat, gen(dbmi_cat)


foreach var of varlist educ_m_yrs female dage_band* dmarital_stat* first_marriage us_born dnetworth_pctl* adl_tot_any adl_tot_nowalk iadl_tot_any drshlt* medicare medicaid priv_insurance ltc_insur demented_tm1 ever_stroke_tm1 ever_cancr_tm1 drarelig* dbmi_cat* ddrink_cat* {
	summ `var', meanonly
	gl mn=r(mean)
	reg `var' i.race i.hispanic i.educ_cat
	predict res, r 
	gen adj`var'=res+$mn
	drop res
}



*****************
* numkids_all *
*****************
g numkids_all_group = .
    replace numkids_all_group=1 if inlist(numkids_all,0,1,2,3)
	replace numkids_all_group=0 if numkids_all>3

	stddiff adjeduc_m_yrs-adjddrink_cat8 if t_sample==1, by(numkids_all_group) cohen abs
	
	matrix T1 = r(output) 
	

	
*****************
* sib_cnt *
*****************
g numsib_all_group = .
    replace numsib_all_group=1 if inlist(sib_cnt,0,1,2,3)
	replace numsib_all_group=0 if sib_cnt>3

	stddiff adjeduc_m_yrs-adjddrink_cat8 if t_sample==1, by(numsib_all_group) cohen abs
	
	matrix T2 = r(output) 

 

	
	
	
	
	
	
	
	
	
	
	