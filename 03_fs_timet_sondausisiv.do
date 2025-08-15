

********************************************************************************
* This file runs the first stage for Informal Care Aim2                        *
* IVs: son + daughter + sister                                                 *
* Created on: 1/12/2023                                                        *
* Written by CSUN                                                              *
********************************************************************************



clear
clear matrix
capture log close
set more off
set scheme s1color
set mem 1000m




********************************************************************************
                             * GENERATE SAMPLE *
********************************************************************************

	eststo clear
	use "$createddata/regressiondata_fnl.dta", clear

	egen sample = rowmiss(multi_nomedhh numson numdaugh sib_sis_cnt ///
                      stateid wave adl_tot_any iadl_tot_any hatotb ///
                      female age_band educ_cat marital_stat race educ_m_yrs hispanic ///
					  medicare medicaid priv_insurance ltc_insur ///
					  us_born first_marriage ///
					  depression_binary rshlt demented_tm1 ever_stroke_tm1 rarelig bmi_cat drink_cat) 
					  
	recode sample (0=1) (else=0)
	tab sample, m 
	
	* Receiving LTC and over 50: 1+ADL/IADL limitation
	g s_ltc_1adl_50 = age_band>=2 & age_band<=9 & ltc_nomedhh==1 & rnhmliv!=1 & fc_combo!=1 & limit_tot >= 1 & !mi(limit_tot) & sample==1 
	
	* Sample restriction 
	keep if s_ltc_1adl_50==1

	* Generate wealth categorical variables
	xtile networth_pctl = hatotb, n(5)
	ta networth_pctl, m
	lab define wlabel 	///
		1 "Wealth- 1st Quintile" ///
		2 "Wealth- 2nd Quintile" ///
		3 "Wealth- 3rd Quintile" ///
		4 "Wealth- 4th Quintile" ///
		5 "Wealth- 5th Quintile" 
		
	lab values networth_pctl 	wlabel
	lab var networth_pctl      "Networth"

sa "$createddata/regressiondata_fnl_multi_nomedhh_sondausisiv.dta", replace




********************************************************************************
                      * ESTABLISH A CONSISTENT SAMPLE * 
********************************************************************************

use "$createddata/regressiondata_fnl_multi_nomedhh_sondausisiv.dta", clear

gl controls "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 


* FIRST STAGE * 
mlogit multi_nomedhh numson numdaugh sib_sis_cnt $controls [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)
	keep hhidpn wave s1_sample
	merge 1:1 hhidpn wave using "$createddata/regressiondata_fnl_multi_nomedhh_sondausisiv.dta", nogen
			
			
* SECOND STAGE *
keep if s1_sample==1

	probit depression_binary i.multi_nomedhh $controls [pw=rwtresp] if s_ltc_1adl_50==1, vce(cluster stateid)
		cap drop tempsample
		cap qui gen tempsample=e(sample)

	mlogit multi_nomedhh numson numdaugh sib_sis_cnt $controls [pw=rwtresp] if tempsample, vce (cluster stateid) base(1)
	
												
		** Changed at 1/17/2025 based on the latest email exchange with Dr. Anirban Basu and Norma.				
	    predict xb2 if e(sample), xb outcome(2)
	    predict xb3 if e(sample), xb outcome(3)
		
		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		
	    gen Xuhat_2 = res2*logisticden(xb2)/(p2*(1-p2))
	    gen Xuhat_3 = res3*logisticden(xb3)/(p3*(1-p3))
		
		cap drop tempsample
		cap qui gen tempsample=e(sample)
		

	probit depression_binary i.multi_nomedhh Xuhat_2 Xuhat_3 $controls [pw=rwtresp] if tempsample, vce(cluster stateid)

	g s1_sample2 = e(sample)
	keep hhidpn wave s1_sample2
	merge 1:1 hhidpn wave using "$createddata/regressiondata_fnl_multi_nomedhh_sondausisiv.dta", nogen
	
sa "$createddata/regressiondata_fnl_multi_nomedhh_sondausisiv.dta", replace


* DOUBLE BACK *
keep if s1_sample2==1

    mlogit multi_nomedhh numson numdaugh sib_sis_cnt $controls [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)
	g t_sample = e(sample)

	keep hhidpn wave t_sample
	merge 1:1 hhidpn wave using "$createddata/regressiondata_fnl_multi_nomedhh_sondausisiv.dta", nogen

sa "$createddata/regressiondata_fnl_multi_nomedhh_sondausisiv.dta", replace




********************************************************************************
				               * FIRST STAGE * 
********************************************************************************

    use "$createddata/regressiondata_fnl_multi_nomedhh_sondausisiv.dta", clear
    keep if t_sample==1
	
	
    loc controls1 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
    loc controls2 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_nowalk iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"  
	
	forv i=3/4 {
    loc controls`i' "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"   
	}
	
	
	local outcomes1    "shealth_binary mob_binary depression_binary pos_affect" 
	
	local listoftables "1"
	di `listoftables'

	loc count=1

foreach i of numlist `listoftables' {  /*start i loop */
    di "table `i'"
		
    foreach var of local outcomes`i' {  /*start var loop*/				
	    di "outcome `var'"	

	
		* Set sample for first stage calc
			di "THIS IS THE OUTCOME VAR FOR FIRST STAGE MLOGIT FOR SAMPLE: `var'"
			cap qui probit `var' i.multi_nomedhh `controls`count'' [pw=rwtresp], vce(cluster stateid)		
			di "cap qui probit `var' i.multi_nomedhh `controls`count'' [pw=rwtresp], vce(cluster stateid)"
			cap drop tempsample
			cap qui gen tempsample=e(sample)
			
	
		* Run FS regression 	
	        eststo pro`i'_`count': mlogit multi_nomedhh numson numdaugh sib_sis_cnt `controls`count'' [pw=rwtresp] if tempsample, vce (cluster stateid) base(1)
			
			sum multi_nomedhh if e(sample)
			local pse_r2 = e(r2_p) 
			local n_obs = r(N)	
			
			
			*BEGIN Chi2 CALCULATION*
			test ([2]_b[numson]=0)([2]_b[numdaugh]=0)
			local chi2_sondau  = r(chi2) 
	
			test ([3]_b[sib_sis_cnt]=0)
			local chi2_sis  = r(chi2) 
	
			test ([2]_b[numson]=0)([2]_b[numdaugh]=0)([3]_b[sib_sis_cnt]=0)
			local chi2_sondausis  = r(chi2) 	
            *END FSTAT CALCULATION*
			
			
            *BEGIN MARGFX CALCULATION*
            margins [aweight=rwtresp], dydx(numson) 
            matrix T_son=r(b)
            scalar fsMFX_son2 = T_son[1,2]
            di "The first stage probit marginal effect for son is " fsMFX_son2
            scalar fsMFX_son3 = T_son[1,3]
            di "The first stage probit marginal effect for son is " fsMFX_son3
	
            matrix T_son_V = r(V)
            matlist T_son_V
            scalar fsSE_son2 = sqrt(T_son_V[2,2])
            scalar fsSE_son3 = sqrt(T_son_V[3,3])	
	
	
            margins [aweight=rwtresp], dydx(numdaugh) 
            matrix T_dau=r(b)
            scalar fsMFX_dau2 = T_dau[1,2]
            di "The first stage probit marginal effect for dau is " fsMFX_dau2
            scalar fsMFX_dau3 = T_dau[1,3]
            di "The first stage probit marginal effect for dau is " fsMFX_dau3
	
            matrix T_dau_V = r(V)
            matlist T_dau_V
            scalar fsSE_dau2 = sqrt(T_dau_V[2,2])
            scalar fsSE_dau3 = sqrt(T_dau_V[3,3])	
			
			
            margins [aweight=rwtresp], dydx(sib_sis_cnt) 
            matrix T_sis=r(b)
            scalar fsMFX_sis2 = T_sis[1,2]
            di "The first stage probit marginal effect for sis is " fsMFX_sis2
            scalar fsMFX_sis3 = T_sis[1,3]
            di "The first stage probit marginal effect for sis is " fsMFX_sis3
	
            matrix T_sis_V = r(V)
            matlist T_sis_V
            scalar fsSE_sis2 = sqrt(T_sis_V[2,2])
            scalar fsSE_sis3 = sqrt(T_sis_V[3,3])						
            *END MARGFX CALCULATION*
			
			
			*BEGIN CI CALCULATION*
			estadd scalar son2_lb = fsMFX_son2 - 1.96*fsSE_son2
			estadd scalar son2_ub = fsMFX_son2 + 1.96*fsSE_son2
			local l_son2_lb: di %9.3f `=e(son2_lb)'
			local l_son2_ub: di %9.3f `=e(son2_ub)'						
			estadd local conf_int_son2 = "[`l_son2_lb' ; `l_son2_ub']"
			
			estadd scalar son3_lb = fsMFX_son3 - 1.96*fsSE_son3
			estadd scalar son3_ub = fsMFX_son3 + 1.96*fsSE_son3
			local l_son3_lb: di %9.3f `=e(son3_lb)'
			local l_son3_ub: di %9.3f `=e(son3_ub)'						
			estadd local conf_int_son3 = "[`l_son3_lb' ; `l_son3_ub']"		
			

			estadd scalar dau2_lb = fsMFX_dau2 - 1.96*fsSE_dau2
			estadd scalar dau2_ub = fsMFX_dau2 + 1.96*fsSE_dau2
			local l_dau2_lb: di %9.3f `=e(dau2_lb)'
			local l_dau2_ub: di %9.3f `=e(dau2_ub)'						
			estadd local conf_int_dau2 = "[`l_dau2_lb' ; `l_dau2_ub']"
			
			estadd scalar dau3_lb = fsMFX_dau3 - 1.96*fsSE_dau3
			estadd scalar dau3_ub = fsMFX_dau3 + 1.96*fsSE_dau3
			local l_dau3_lb: di %9.3f `=e(dau3_lb)'
			local l_dau3_ub: di %9.3f `=e(dau3_ub)'						
			estadd local conf_int_dau3 = "[`l_dau3_lb' ; `l_dau3_ub']"	
			
			
			estadd scalar sis2_lb = fsMFX_sis2 - 1.96*fsSE_sis2
			estadd scalar sis2_ub = fsMFX_sis2 + 1.96*fsSE_sis2
			local l_sis2_lb: di %9.3f `=e(sis2_lb)'
			local l_sis2_ub: di %9.3f `=e(sis2_ub)'						
			estadd local conf_int_sis2 = "[`l_sis2_lb' ; `l_sis2_ub']"
			
			estadd scalar sis3_lb = fsMFX_sis3 - 1.96*fsSE_sis3
			estadd scalar sis3_ub = fsMFX_sis3 + 1.96*fsSE_sis3
			local l_sis3_lb: di %9.3f `=e(sis3_lb)'
			local l_sis3_ub: di %9.3f `=e(sis3_ub)'						
			estadd local conf_int_sis3 = "[`l_sis3_lb' ; `l_sis3_ub']"				
			*END CI CALCULATION*			
			
			
			* Create new scalar to restrict to one output: first stage ME and Chi2
			qui estadd local Chi2_sondau `chi2_sondau'               /* FS chi-stat */	
			qui estadd local Chi2_sis `chi2_sis'                     /* FS Chi-stat */	
			qui estadd local Chi2_sondausis `chi2_sondausis'         /* FS chi-stat */	
			
			qui estadd local PseudoR2 `pse_r2' 
			qui estadd local N_OBS `n_obs'
			qui estadd scalar MFX_son2 fsMFX_son2 /* FS marginal effect */
			qui estadd scalar MFX_dau2 fsMFX_dau2 /* FS marginal effect */
			qui estadd scalar MFX_sis2 fsMFX_sis2 /* FS marginal effect */			
			qui estadd scalar MFX_son3 fsMFX_son3 /* FS marginal effect */
			qui estadd scalar MFX_dau3 fsMFX_dau3 /* FS marginal effect */
			qui estadd scalar MFX_sis3 fsMFX_sis3 /* FS marginal effect */	
			qui estadd scalar FSSE_son2 fsSE_son2  /* FS se */
			qui estadd scalar FSSE_dau2 fsSE_dau2  /* FS se */
			qui estadd scalar FSSE_sis2 fsSE_sis2  /* FS se */
			qui estadd scalar FSSE_son3 fsSE_son3  /* FS se */
			qui estadd scalar FSSE_dau3 fsSE_dau3  /* FS se */
			qui estadd scalar FSSE_sis3 fsSE_sis3  /* FS se */			
			estadd local sname = "`var'"
	
	loc count=`count'+1
	
    } 
	
    estout pro`i'_* using "$output/sondausisiv/Appendix Table 3 FS.tex", cells(b(fmt(3) star) se(par)) ///
	stats(sname MFX_son2 FSSE_son2 conf_int_son2 MFX_dau2 FSSE_dau2 conf_int_dau2 MFX_sis2 FSSE_sis2 conf_int_sis2 MFX_son3 FSSE_son3 conf_int_son3 MFX_dau3 FSSE_dau3 conf_int_dau3 MFX_sis3 FSSE_sis3 conf_int_sis3 Chi2_sondau Chi2_sis Chi2_sondausis PseudoR2 N_OBS, labels("Sample" "Marginal Effect SON2" "FSSE SON2" "FSCI SON2" "Marginal Effect DAU2" "FSSE DAU2" "FSCI DAU2" "Marginal Effect SIS2" "FSSE SIS2" "FSCI SIS2" "Marginal Effect SON3" "FSSE SON3" "FSCI SON3" "Marginal Effect DAU3" "FSSE DAU3" "FSCI DAU3" "Marginal Effect SIS3" "FSSE SIS3" "FSCI SIS3" "Chi2_SonDau" "Chi2_Sis" "Chi2_SonDauSis" "Pseudo R2" "Observations")) ///
	style(tex) nolegend label replace starlevels(* 0.10 ** 0.05 *** 0.01) ///
	keep(numson numdaugh sib_sis_cnt) collabels(,none) mlabels(,none)  numbers
}


eststo clear
constraint drop _all
