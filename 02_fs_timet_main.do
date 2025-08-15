

********************************************************************************
* This file runs the summary statistics and first stage for Informal Care Aim2 *
* Main sample: Receiving LTC and over 50, 1+ADL/IADL Limitations               *
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

	egen sample = rowmiss(multi_nomedhh sib_cnt numkids_all ///
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

sa "$createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv.dta", replace




********************************************************************************
                      * ESTABLISH A CONSISTENT SAMPLE * 
********************************************************************************

use "$createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv.dta", clear

gl controls "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"  


* FIRST STAGE * 
mlogit multi_nomedhh sib_cnt numkids_all $controls [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)
	keep hhidpn wave s1_sample
	merge 1:1 hhidpn wave using "$createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv.dta", nogen
			
			
* SECOND STAGE *
keep if s1_sample==1

	probit depression_binary i.multi_nomedhh $controls [pw=rwtresp] if s_ltc_1adl_50==1, vce(cluster stateid)
		cap drop tempsample
		cap qui gen tempsample=e(sample)

	mlogit multi_nomedhh sib_cnt numkids_all $controls [pw=rwtresp] if tempsample, vce (cluster stateid) base(1)
						
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
	merge 1:1 hhidpn wave using "$createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv.dta", nogen
	
sa "$createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv.dta", replace


* DOUBLE BACK *
keep if s1_sample2==1

    mlogit multi_nomedhh sib_cnt numkids_all $controls [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)
	g t_sample = e(sample)

	keep hhidpn wave t_sample
	merge 1:1 hhidpn wave using "$createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv.dta", nogen

sa "$createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv.dta", replace




********************************************************************************
						* SUMMARY STATISTICS * 
********************************************************************************

use "$createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv.dta", clear
keep if t_sample==1 


* Generate control cat variables
tab female,          g (dfemale) 
tab hispanic,        g (dhispanic)
tab age_band, 	     g (dage_band)
tab marital_stat, 	 g (dmarital_stat)
tab educ_cat, 	     g (deduc_cat)
tab networth_pctl,   g (dnetworth_pctl)
tab race, 	         g (drace)
tab rshlt,           g (drshlt)
tab medicare, 	     g (dmedicare)
tab medicaid, 	     g (dmedicaid)
tab priv_insurance,  g (dpriv_insurance)
tab ltc_insur, 	     g (dltc_insur)
tab us_born,         g (dus_born)
tab first_marriage,  g (dfirst_marriage)
tab multi_nomedhh,   g (dmulti_nomedhh)

tab demented_tm1,    g (ddemented_tm1) 
tab ever_stroke_tm1, g (dever_stroke_tm1)
tab ever_cancr_tm1,  g (dever_cancr_tm1)
tab rarelig,         g (drarelig)

tab shealth_binary,  g (dshealth_binary)
tab pos_affect,      g (dpos_affect)
tab depression_binary, g (ddepression_binary)
tab demented,        g (ddemented)
tab stroke,          g (dstroke)
tab cancr,           g (dcancr)
tab mob_binary,      g (dmob_binary)
tab drink_cat,       g (ddrink_cat)
tab bmi_cat,         g (dbmi_cat)



********************************************************************************

loc iv	 				    "sib_cnt numkids_all"
loc outcomes_sumstat 		"dshealth_binary* dmob_binary* ddepression_binary* dpos_affect* dcancr*"		
loc controls_sumstat 		"dage_band* dfemale* drace* dhispanic* dmarital_stat* deduc_cat* dus_born* dfirst_marriage* dnetworth_pctl* adl_tot_any iadl_tot_any adl_tot_nowalk dmedicare* dmedicaid* dpriv_insurance* dltc_insur* educ_m_yrs ddemented_tm1* dever_stroke_tm1* dever_cancr_tm1* drarelig* dbmi_cat* ddrink_cat*" 


********************************************************************************

estpost su dmulti_nomedhh* `controls_sumstat' `iv' [w=rwtresp]
	est store A
estpost su dmulti_nomedhh* `controls_sumstat' `iv' [w=rwtresp] if multi_nomedhh==1 
	est store B 
estpost su dmulti_nomedhh* `controls_sumstat' `iv' [w=rwtresp] if multi_nomedhh==2 
	est store C
estpost su dmulti_nomedhh* `controls_sumstat' `iv' [w=rwtresp] if multi_nomedhh==3 
	est store D
	
estpost su `outcomes_sumstat' [w=rwtresp]
	est store E
estpost su `outcomes_sumstat' [w=rwtresp] if multi_nomedhh==1 
	est store F
estpost su `outcomes_sumstat' [w=rwtresp] if multi_nomedhh==2 
	est store G
estpost su `outcomes_sumstat' [w=rwtresp] if multi_nomedhh==3
	est store H

	
esttab A B C D using "$output/main/Table 2.tex", replace ///
		mtitle("Full Sample" "IC only" "FC only" "Both") ///
		cells(mean(fmt(2))) label booktabs nonum collabels(none) f gaps noobs ///
		stats(N, fmt(%18.0g) labels("Observations")) ///
			prehead(/begin{tabular}{l*{@M}{c}} /hline /hline) posthead(/hline) prefoot(//) postfoot(/hline /hline /end{tabular})
			
esttab E F G H using "$output/main/Table 3.tex", replace ///
		mtitle("Full Sample" "IC only" "FC only" "Both") ///
		cells(mean(fmt(2))) label booktabs nonum collabels(none) f gaps noobs ///
		stats(N, fmt(%18.0g) labels("Observations")) ///
			prehead(/begin{tabular}{l*{@M}{c}} /hline /hline) posthead(/hline) prefoot(//) postfoot(/hline /hline /end{tabular})

eststo clear



********************************************************************************
* Start the KWallis calculation 

loc kruskal 		"dage_band1 dage_band2 dage_band3 dage_band4 dage_band5 dage_band6 dage_band7 dage_band8 dfemale2 drace1 drace2 drace3 dhispanic2 dmarital_stat1 dmarital_stat2 dmarital_stat3 dmarital_stat4 deduc_cat1 deduc_cat2 deduc_cat3 deduc_cat4 dus_born2 dfirst_marriage2 dnetworth_pctl1 dnetworth_pctl2 dnetworth_pctl3 dnetworth_pctl4 dnetworth_pctl5 adl_tot_any iadl_tot_any adl_tot_nowalk dmedicare2 dmedicaid2 dpriv_insurance2 dltc_insur2 educ_m_yrs ddemented_tm12 dever_stroke_tm12 dever_cancr_tm12 drarelig1 drarelig2 drarelig3 drarelig4 drarelig5 dbmi_cat1 dbmi_cat2 dbmi_cat3 ddrink_cat1 ddrink_cat2 ddrink_cat3 ddrink_cat4 ddrink_cat5 ddrink_cat6 ddrink_cat7 ddrink_cat8 sib_cnt numkids_all dshealth_binary2 dmob_binary2 ddepression_binary2 dpos_affect2 dcancr2"  


loc n: word count `kruskal'
loc i=1
foreach var of local kruskal {
	kwallis `var', by(multi_nomedhh)
	loc pvalue: di %9.3f chi2tail(r(df), r(chi2))
	di `pvalue'
	mat r`i'=`pvalue'
	loc ++i
}

forv j=2/`n'{
	loc all "`all' \r`j'"
}		
	
mat R=r1`all'
mat rownames R=`kruskal'	
mat colnames R="P-value"
esttab mat(R) using "$output/Table 2&3 Pvalue.rtf", l nomtitle replace




********************************************************************************
				               * FIRST STAGE * 
********************************************************************************

use "$createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv.dta", clear
keep if t_sample==1
	
	
    loc controls1 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
    loc controls2 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_nowalk iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"  
	
	forv i=3/4 {
    loc controls`i' "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"   
	}
	
    loc controls5 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(1).ever_cancr_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
	
	local outcomes1    "shealth_binary mob_binary depression_binary pos_affect cancr" 
	
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
			di "cap qui probit `var' i.multi_nomedhh $`controls`count'' [pw=rwtresp], vce(cluster stateid)"
			cap drop tempsample
			cap qui gen tempsample=e(sample)
			
	
		* Run FS regression 	
	        eststo pro`i'_`count': mlogit multi_nomedhh sib_cnt numkids_all `controls`count'' [pw=rwtresp] if tempsample, vce (cluster stateid) base(1)
			
			su multi_nomedhh if e(sample)
			local pse_r2 = e(r2_p) 
			local n_obs = r(N)	
			
			
			*BEGIN Chi2 CALCULATION*
			test ([2]_b[numkids_all]=0)
			local chi2_kids  = r(chi2) 
	
			test ([3]_b[sib_cnt]=0)
			local chi2_sib  = r(chi2) 
	
			test ([2]_b[numkids_all]=0)([3]_b[sib_cnt]=0)
			local chi2_sibkids  = r(chi2) 
            *END Chi2 CALCULATION*
			
			
            *BEGIN MARGFX CALCULATION*
            margins [aweight=rwtresp], dydx(numkids_all) 
            matrix T_kids=r(b)
			matlist T_kids
            scalar fsMFX_k2 = T_kids[1,2]
            di "The first stage probit marginal effect for kids is " fsMFX_k2
            scalar fsMFX_k3 = T_kids[1,3]
            di "The first stage probit marginal effect for kids is " fsMFX_k3
	
            matrix T_kids_V = r(V)
            matlist T_kids_V
            scalar fsSE_k2 = sqrt(T_kids_V[2,2])
            scalar fsSE_k3 = sqrt(T_kids_V[3,3])	
	
	
            margins [aweight=rwtresp], dydx(sib_cnt) 
            matrix T_sib=r(b)
            matlist T_sib	
            scalar fsMFX_s2 = T_sib[1,2]
            di "The first stage probit marginal effect for siblings is " fsMFX_s2	
            scalar fsMFX_s3 = T_sib[1,3]
            di "The first stage probit marginal effect for siblings is " fsMFX_s3		
	
            matrix T_sib_V = r(V)
            matlist T_sib_V
            scalar fsSE_s2 = sqrt(T_sib_V[2,2])
            scalar fsSE_s3 = sqrt(T_sib_V[3,3])	
            *END MARGFX CALCULATION*
			
			
			*BEGIN CI CALCULATION*
			estadd scalar k2_lb = fsMFX_k2 - 1.96*fsSE_k2
			estadd scalar k2_ub = fsMFX_k2 + 1.96*fsSE_k2
			local l_k2_lb: di %9.3f `=e(k2_lb)'
			local l_k2_ub: di %9.3f `=e(k2_ub)'						
			estadd local conf_int_k2 = "[`l_k2_lb' ; `l_k2_ub']"
			
			estadd scalar k3_lb = fsMFX_k3 - 1.96*fsSE_k3
			estadd scalar k3_ub = fsMFX_k3 + 1.96*fsSE_k3
			local l_k3_lb: di %9.3f `=e(k3_lb)'
			local l_k3_ub: di %9.3f `=e(k3_ub)'						
			estadd local conf_int_k3 = "[`l_k3_lb' ; `l_k3_ub']"				

			estadd scalar s2_lb = fsMFX_s2 - 1.96*fsSE_s2
			estadd scalar s2_ub = fsMFX_s2 + 1.96*fsSE_s2
			local l_s2_lb: di %9.3f `=e(s2_lb)'
			local l_s2_ub: di %9.3f `=e(s2_ub)'						
			estadd local conf_int_s2 = "[`l_s2_lb' ; `l_s2_ub']"
			
			estadd scalar s3_lb = fsMFX_s3 - 1.96*fsSE_s3
			estadd scalar s3_ub = fsMFX_s3 + 1.96*fsSE_s3
			local l_s3_lb: di %9.3f `=e(s3_lb)'
			local l_s3_ub: di %9.3f `=e(s3_ub)'						
			estadd local conf_int_s3 = "[`l_s3_lb' ; `l_s3_ub']"	
			*END CI CALCULATION*			
			
			
			* Create new scalar to restrict to one output: first stage ME and Chi2
			qui estadd local Chi2_kids `chi2_kids'               /* FS chi-stat */
			qui estadd local Chi2_sib `chi2_sib'                     /* FS Chi-stat */		
			qui estadd local Chi2_sibkids `chi2_sibkids'         /* FS chi-stat */	

			qui estadd local PseudoR2 `pse_r2' 
			qui estadd local N_OBS `n_obs'
			qui estadd scalar MFX_k2 fsMFX_k2 /* FS marginal effect */
			qui estadd scalar MFX_s2 fsMFX_s2 /* FS marginal effect */
			qui estadd scalar MFX_k3 fsMFX_k3 /* FS marginal effect */
			qui estadd scalar MFX_s3 fsMFX_s3 /* FS marginal effect */
			qui estadd scalar FSSE_k2 fsSE_k2  /* FS se */
			qui estadd scalar FSSE_s2 fsSE_s2  /* FS se */
			qui estadd scalar FSSE_k3 fsSE_k3  /* FS se */
			qui estadd scalar FSSE_s3 fsSE_s3  /* FS se */	
			
			estadd local sname = "`var'"
	
	loc count=`count'+1
	
    } 
	

    estout pro`i'_* using "$output/main/Table 5 FS.tex", cells(b(fmt(3) star) se(par)) ///
	stats(sname MFX_s2 FSSE_s2 conf_int_s2 MFX_k2 FSSE_k2 conf_int_k2 MFX_s3 FSSE_s3 conf_int_s3 MFX_k3 FSSE_k3 conf_int_k3 Chi2_kids Chi2_sib Chi2_sibkids PseudoR2 N_OBS, labels("Sample" "Marginal Effect Sib2" "FSSE Sib2" "FSCI Sib2" "Marginal Effect Kids2" "FSSE Kids2" "FSCI Kids2" "Marginal Effect Sib3" "FSSE Sib3" "FSCI Sib3" "Marginal Effect Kids3" "FSSE Kids3" "FSCI Kids3" "Chi2_Kids" "Chi2_Sib" "Chi2_SibKids" "Pseudo R2" "Observations" )) ///
	style(tex) nolegend label replace starlevels(* 0.10 ** 0.05 *** 0.01) ///
	keep(sib_cnt numkids_all) collabels(,none) mlabels(,none) numbers 
}

eststo clear
constraint drop _all
