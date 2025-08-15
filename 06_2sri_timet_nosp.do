 
********************************************************************************
							* SECOND STAGE * 
********************************************************************************

/* GENERAL NOTES: 

- This do-file runs the second stage (2sri & generalized residuals)

INPUT: 	$createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv_nosp.dta

OUTPUT: Main Table 7 Panel A. Unpartnered care recipients

*/

clear
clear matrix
clear mata
capture log close
set more off
set scheme s1color
set maxvar 20000
set mem 1000m
cap ssc install estout
	
log using "$log/2sri_main_table7A_unpartnered.log", replace



********************************************************************************
				*** SECOND STAGE REGRESSIONS:  PROBIT/2SRI ***
********************************************************************************
use "$createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv_nosp.dta", clear

tostring hhidpn, gen(HHIDPN)
gen hhid= substr(HHIDPN, 1, length(HHIDPN) - 3)
destring hhid, replace

keep if t_sample==1

********************************************************************************
		
	local outcomes1		"shealth_binary mob_binary depression_binary pos_affect" 
	
	local listoftables "1"
	di `listoftables'

	foreach i of numlist `listoftables' { 
	  local alloutcomes "`alloutcomes' `outcomes`i''"
	}

	local countvar: word count `alloutcomes'
	di `countvar' " variables"
	
	
    loc controls1 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
    loc controls2 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_nowalk iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"  
	
	forv i=3/4 {
    loc controls`i' "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"   
	}
	
	
********************************************************************************

set seed 1980

* Number of iterations in bootstrap
loc iterations 1000

********************************************************************************
   * Generate frequency weights
    g fw=.
	
	* Create matrix to store bootstrap results
	matrix margfxmatrixa=J(`iterations',`countvar',.)  
	matrix margfxmatrixb=J(`iterations',`countvar',.)

	* First bootstrap to get standard errors; save in matrix

		forv iter=1(1)`iterations' {  /* start iter loop */				
		di "iteration `iter'"
						  
		preserve
						  
		count
		bsample if s_ltc_1adl_50, weight(fw)
		ta fw, m
		expand fw
		ta fw, m
		
		** Generate new weight for BS
		gen rwtresp_bs=rwtresp/fw


		local counter = 1
						
						
		foreach var of local alloutcomes { /*start var loop */	

		* Set sample for first stage calc
			di "THIS IS THE OUTCOME VAR FOR FIRST STAGE MLOGIT FOR SAMPLE: `var'"
			qui probit `var' i.multi_nomedhh `controls`counter'' [pw=rwtresp_bs] if fw>=1, vce(cluster stateid)		
			di "qui probit `var' i.multi_nomedhh `controls`counter'' [pw=rwtresp_bs] if fw>=1, vce(cluster stateid)"
			cap drop tempsample
			cap qui gen tempsample=e(sample)
									  
		* First stage calc										  										
			di "BEGIN CALCULATION OF MLOGIT"
				qui mlogit multi_nomedhh numkids_all sib_cnt `controls`counter'' [pw=rwtresp_bs] if tempsample, vce (cluster stateid) base(1) iter(300)				
				di "qui mlogit multi_nomedhh numkids_all sib_cnt `controls`counter'' [pw=rwtresp_bs] if tempsample, vce (cluster stateid) base(1) iter(300)"											
				
				** Changed at 1/17/2025 based on the latest email exchange with Dr. Anirban Basu and Norma.				
				predict xb2 if e(sample), xb outcome(2)
				predict xb3 if e(sample), xb outcome(3)
		
				predict p2 if e(sample), outcome(2)
				predict p3 if e(sample), outcome(3)
				
				gen res2=(multi_nomedhh==2)-p2
				gen res3=(multi_nomedhh==3)-p3
		
				gen Xuhat_2 = res2*logisticden(xb2)/(p2*(1-p2))
				gen Xuhat_3 = res3*logisticden(xb3)/(p3*(1-p3))
				     
				drop tempsample xb2 xb3 p2 p3 res2 res3
				qui gen tempsample=e(sample)
			di "END CALCULATION OF MLOGIT"		
				
		* Second stage calc	
			di "BEGIN CALCULATION OF PROBIT"
			    qui probit `var' i.multi_nomedhh Xuhat_2 Xuhat_3 `controls`counter'' [pw=rwtresp_bs] if tempsample, vce(cluster stateid)					
			    di "qui probit `var' i.multi_nomedhh Xuhat_2 Xuhat_3 `controls`counter'' [pw=rwtresp_bs] if tempsample, vce(cluster stateid)"
			    predictnl margfx2 = normal(xb() - _b[2.multi_nomedhh]*2.multi_nomedhh + _b[2.multi_nomedhh]) - normal(xb() - _b[2.multi_nomedhh]*2.multi_nomedhh)
			    sum margfx2 if e(sample) 
			    matrix margfxmatrixa[`=`iter'',`=`counter'']=r(mean)
			    cap drop margfx2
			    predictnl margfx3 = normal(xb() - _b[3.multi_nomedhh]*3.multi_nomedhh + _b[3.multi_nomedhh]) - normal(xb() - _b[3.multi_nomedhh]*3.multi_nomedhh)
			    sum margfx3 if e(sample) 
			    matrix margfxmatrixb[`=`iter'',`=`counter'']=r(mean)
			    cap drop margfx3	
	    	di "END CALCULATION OF PROBIT"	
		
		
			local counter = `counter'+1
			cap drop Xuhat_2 Xuhat_3
			cap drop tempsample		
		
	    } /* end var loop */
							  
	restore
						  
	} /*end iter loop*/
	  
	local count = 1
	estimates clear
	
	matlist margfxmatrixa
	matlist margfxmatrixb

	
* Next run main results and access bootstrapped standard error from matrix
			
	foreach i of numlist `listoftables' {  /*start i loop */
		di "table `i'"
		foreach var of local outcomes`i' {  /*start var loop*/				
			di "outcome `var'"						
			
			
        * Probit - no instruments		
        qui probit `var' i.multi_nomedhh `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce(cluster stateid)		
        di "qui probit `var' i.multi_nomedhh `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce(cluster stateid)"
		
		    *** BEGIN MARGFX ***
		    predictnl margfx2_noiv = normal(xb() - _b[2.multi_nomedhh]*2.multi_nomedhh + _b[2.multi_nomedhh]) - normal(xb() - _b[2.multi_nomedhh]*2.multi_nomedhh)
		    predictnl margfx3_noiv = normal(xb() - _b[3.multi_nomedhh]*3.multi_nomedhh + _b[3.multi_nomedhh]) - normal(xb() - _b[3.multi_nomedhh]*3.multi_nomedhh)
								
		    sum margfx2_noiv if e(sample) 
		    scalar 	me_probit2 = r(mean)
		    sum margfx3_noiv if e(sample) 
		    scalar 	me_probit3 = r(mean)
			*** END MARGFX ***
	
		    *** BEGIN CI ***								
		    * Margins - get SE - use analytic weight for SE
		    margins [aweight=rwtresp], dydx(multi_nomedhh) 
		    matrix T = r(V)
		    scalar se_probit2 = sqrt(T[2,2])
		    di se_probit2
		    scalar se_probit3 = sqrt(T[3,3])
		    di se_probit3		
		    matrix drop T
		    scalar 	prob_lb2 = me_probit2 - 1.96*se_probit2
		    scalar 	prob_ub2 = me_probit2 + 1.96*se_probit2
		    scalar 	prob_lb3 = me_probit3 - 1.96*se_probit3
		    scalar 	prob_ub3 = me_probit3 + 1.96*se_probit3		
			
			
			estadd scalar me_probit2		
	        estadd scalar me_probit3
			estadd scalar se_probit2		
	        estadd scalar se_probit3
			
			scalar probitpvalue_fc = 2*ttail(e(df_m),abs(e(me_probit2)/e(se_probit2)))
			di "The Probit FC pv is" `=e(probitpvalue_fc)'
			
			scalar probitpvalue_both = 2*ttail(e(df_m),abs(e(me_probit3)/e(se_probit3)))
			di "The Probit BOTH pv is" `=e(probitpvalue_both)'
		    *** END CI ***
	
	
        * Set sample for first stage calc
        qui probit `var' i.multi_nomedhh `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce(cluster stateid)		
        di "qui probit `var' i.multi_nomedhh `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce(cluster stateid)"
		
            cap drop tempsample
            qui gen tempsample=e(sample)
	
	
		* First stage calc: mlogit																
		qui mlogit multi_nomedhh numkids_all sib_cnt `controls`count'' [pw=rwtresp] if tempsample, vce (cluster stateid) base(1)					
		di "qui mlogit multi_nomedhh numkids_all sib_cnt `controls`count'' [pw=rwtresp] if tempsample, vce (cluster stateid) base(1)"
												
				** Changed at 1/17/2025 based on the email exchange with Dr. Anirban Basu and Norma.				
				predict xb2 if e(sample), xb outcome(2)
				predict xb3 if e(sample), xb outcome(3)
		
				predict p2 if e(sample), outcome(2)
				predict p3 if e(sample), outcome(3)
				
				gen res2=(multi_nomedhh==2)-p2
				gen res3=(multi_nomedhh==3)-p3
		
				gen Xuhat_2 = res2*logisticden(xb2)/(p2*(1-p2))
				gen Xuhat_3 = res3*logisticden(xb3)/(p3*(1-p3))
		
			drop tempsample xb2 xb3 p2 p3 res2 res3
			qui gen tempsample=e(sample)			
			
			
				
		* Second stage calc: probit
		eststo tsrib`i'_`count': probit `var' i.multi_nomedhh Xuhat_2 Xuhat_3 `controls`count'' [pw=rwtresp] if tempsample, vce(cluster stateid)
		di "eststo tsrib`i'_`count': probit `var' i.multi_nomedhh Xuhat_2 Xuhat_3 `controls`count'' [pw=rwtresp] if tempsample, vce(cluster stateid)"
			
			
	        *** BEGIN CI ***
	        * Get me for CI
			predictnl margfx2_iv = normal(xb() - _b[2.multi_nomedhh]*2.multi_nomedhh + _b[2.multi_nomedhh]) - normal(xb() - _b[2.multi_nomedhh]*2.multi_nomedhh) 
	        sum margfx2_iv if e(sample) 
	        estadd scalar		me_tsri_v2 = r(mean)	
	        di "The FC 2SRI ME is" `=e(me_tsri_v2)'
            
			predictnl margfx3_iv = normal(xb() - _b[3.multi_nomedhh]*3.multi_nomedhh + _b[3.multi_nomedhh]) - normal(xb() - _b[3.multi_nomedhh]*3.multi_nomedhh) 
	        sum margfx3_iv if e(sample) 
	        estadd scalar		me_tsri_v3 = r(mean)	
	        di "The BOTH 2SRI ME is" `=e(me_tsri_v3)'
	
	        * Get se for CI	
			matrix margfxmatrixba`count' = margfxmatrixa[1...,colnumb(margfxmatrixa,"c`count'")]
			svmat double margfxmatrixba`count', name(margfxa`count')
			sum margfxa`count'
	        estadd scalar bootstrse_v2 = r(sd)
	        di "The FC bootsrapped se is" `=e(bootstrse_v2)'
			estadd scalar pvalue2 = 2*ttail(e(df_m),abs(e(me_tsri_v2)/e(bootstrse_v2)))
			di "The FC bootsrapped pv is" `=e(pvalue2)'
	        local 	      n_pvalue2: di %9.3f `=e(pvalue2)'
	        di "The FC bootsrapped new pv is" `n_pvalue2'	
			estadd local pvalue_fc = "`n_pvalue2'"

			matrix margfxmatrixb`count' = margfxmatrixb[1...,colnumb(margfxmatrixb,"c`count'")]
			svmat double margfxmatrixb`count', name(margfxb`count')
			sum margfxb`count'
	        estadd scalar bootstrse_v3 = r(sd)
	        di "The BOTH bootsrapped se is" `=e(bootstrse_v3)'	
	        estadd scalar pvalue3 = 2*ttail(e(df_m),abs(e(me_tsri_v3)/e(bootstrse_v3)))
			di "The BOTH bootsrapped pv is" `=e(pvalue3)'
	        local 	      n_pvalue3: di %9.3f `=e(pvalue3)'
	        di "The BOTH bootsrapped new pv is" `n_pvalue3'	
			estadd local pvalue_both = "`n_pvalue3'"
	        									
	        * Access distribution
	        estadd scalar 	tsri_lb2 = e(me_tsri_v2) - 1.96*e(bootstrse_v2)
	        di "The FC 2SRI lower bound is" `=e(tsri_lb2)'
	        estadd scalar 	tsri_ub2 = e(me_tsri_v2) + 1.96*e(bootstrse_v2)
	        di "The FC 2SRI upper bound is" `=e(tsri_ub2)'
	        local 			l_tsri_lb2: di %9.3f `=e(tsri_lb2)'
	        di "The FC 2SRI CI lower bound is"`l_tsri_lb2'
	        local 			l_tsri_ub2: di %9.3f `=e(tsri_ub2)'
	        di "The FC 2SRI CI upper bound is"`l_tsri_ub2'
	        estadd local conf_int_tsri2 = "[`l_tsri_lb2' ; `l_tsri_ub2']"

	        estadd scalar 	tsri_lb3 = e(me_tsri_v3) - 1.96*e(bootstrse_v3)
	        di "The BOTH 2SRI lower bound is" `=e(tsri_lb3)'
	        estadd scalar 	tsri_ub3 = e(me_tsri_v3) + 1.96*e(bootstrse_v3)
	        di "The BOTH 2SRI upper bound is" `=e(tsri_ub3)'
	        local 			l_tsri_lb3: di %9.3f `=e(tsri_lb3)'
	        di "The BOTH 2SRI CI lower bound is"`l_tsri_lb3'
	        local 			l_tsri_ub3: di %9.3f `=e(tsri_ub3)'
	        di "The BOTH 2SRI CI upper bound is"`l_tsri_ub3'
	        estadd local conf_int_tsri3 = "[`l_tsri_lb3' ; `l_tsri_ub3']"	
	        *** END CI *** 	
		
		* Create new scalar to restrict to one output: notes
		qui estadd local indfe = "No"
		qui estadd local weights = "Yes"
									
		* Create new scalar to restrict to one output: dv mean
		qui sum `var' if e(sample) & multi_nomedhh==1
		estadd scalar dvmean_comb = r(mean)

		* Create new scalar to restrict to one output: N(obs)
		qui sum `var' if e(sample)
		estadd scalar N_obs_comb = r(N)
									
		* Create new scalar to restrict to one output: N(clust)
		estadd scalar N_clust_comb = e(N_clust) 

		* Create new scalar to restrict to one output: avg(wave) in sample	
		qui gen sampletag=e(sample)
		qui bys hhid sampletag: gen tag2=cond(_N==1,1,_n)
		qui bys hhid sampletag: egen num_wave2=max(tag2) 
		qui sum num_wave2 if e(sample) 
		estadd scalar N_wave2=r(mean)
		qui drop tag2 num_wave2
	 
		* Create new scalar to restrict to one output: N(fam)	
		bys hhid sampletag: gen nvals=_n==1 
		count if nvals==1 & sampletag==1
		estadd scalar N_fam=r(N)
		drop nvals sampletag
	
	    estadd local space = ""
									
	    * Add probit results							
	    estadd scalar me_probit2		
	    estadd scalar me_probit3	
	    estadd scalar prob_lb2
	    estadd scalar prob_lb3
	    local l_prob_lb2: di %9.3f `=e(prob_lb2)'
	    estadd scalar prob_ub2
	    local l_prob_ub2 : di %9.3f `=e(prob_ub2)'						
	    estadd local conf_int_prob2 = "[`l_prob_lb2' ; `l_prob_ub2']"
	    local l_prob_lb3: di %9.3f `=e(prob_lb3)'
	    estadd scalar prob_ub3
	    local l_prob_ub3: di %9.3f `=e(prob_ub3)'						
	    estadd local conf_int_prob3 = "[`l_prob_lb3' ; `l_prob_ub3']"
		estadd scalar probitpvalue_fc
		estadd scalar probitpvalue_both
		
	
    cap drop margfx*
	cap drop me_probit*
	cap drop prob_lb*
	cap drop prob_ub*
	cap drop prob_lb_red* 
	cap drop prob_ub_red* 
	cap drop me_tsri*
	cap drop tsri_lb* 
	cap drop tsri_ub* 		
	cap drop margfxFS* 
	cap drop tempsample* 
	cap drop Xuhat_2 Xuhat_3 
	cap drop df_num
								
								 
							
	local count = `count'+1
	
	
} /*end var loop*/


loc j = `i'	
	estout tsrib`i'_* using "$output/Table 7A.tex", cells(none) ///
		stats(me_probit2 conf_int_prob2 probitpvalue_fc me_probit3 conf_int_prob3 probitpvalue_both space space me_tsri_v2 conf_int_tsri2 pvalue_fc me_tsri_v3 conf_int_tsri3 pvalue_both space dvmean_comb N_clust_comb N_obs_comb N_wave2 N_fam indfe weights, layout(@ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @ @) fmt(%9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3g %9.3gc %9.3gc %9.3gc %9.3gc) labels("Probit ME FC Only" "Probit CI FC Only" "Probit P Value FC Only" "Probit ME Both" "Probit CI Both" "Probit P Value Both" " " "/hline" "2SRI ME FC Only" "2SRI CI FC Only" "P Value FC Only" "2SRI ME Both" "2SRI CI Both" "P Value Both" "/hline" "Mean Control DV" "Clusters" "Observations" "Waves" "Families" "Ind FE" "Weights")) ///
		style(tex) nolegend label replace starlevels(* 0.10 ** 0.05 *** 0.01) ///
		collabels(,none) mlabels(,depvars) numbers



} /*end i loop*/

		matrix drop _all


eststo clear

log close