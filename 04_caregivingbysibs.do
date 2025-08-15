





********************************************************************************
  * Table 4. Percent of helpers by relationship, by the number of siblings *
********************************************************************************

	use "$createddata\regressiondata_fnl.dta", clear

	* Generate study sample 
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
	
	merge 1:m hhidpn wave using "$createddata\cgpattern.dta", keep(master matched)

	
	* Get numbers for Table 4
	g sib_cat=.
	    replace sib_cat = 1 if sib_cnt == 0 
		replace sib_cat = 2 if sib_cnt == 1 
		replace sib_cat = 3 if sib_cnt == 2 
		replace sib_cat = 4 if sib_cnt == 3 
		replace sib_cat = 5 if sib_cnt == 4 
		replace sib_cat = 6 if sib_cnt >=5 & !mi(sib_cnt) 		
		