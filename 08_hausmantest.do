
********************************************************************************
							* IV Test * 
********************************************************************************

/* GENERAL NOTES: 

- This do-file runs the Hausman Test

INPUT: 	$createddata/regressiondata_fnl.dta

OUTPUT: Main Table 1

*/

	clear  
	clear matrix
	clear mata
	capture log close
	set more off
	set maxvar 20000
	set scheme s1color
	cap ssc install estout

	
    log using "$log/Table1_Hausmantest.log", replace 
	
	
	
	
********************************************************************************
     * IVs: sib_cnt numkids_all  * 
********************************************************************************
	
    * Generate Sample: Receiving LTC and over 50, 1+ADL/IADL Limitations
	
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
	g s_ltc_1adl_50 = age_band>=2 & age_band<=9 & ltc_nomedhh==1 & rnhmliv!=1 & fc_combo!=1 & limit_tot >= 1 & !mi(limit_tot) & wave>=6 & sample==1 
	
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

	
	* Start the Hausman test *
	
    loc controls1 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
    loc controls2 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_nowalk iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"  
	
	forv i=3/4 {
    loc controls`i' "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"   
	}
	
    loc controls5 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(1).ever_cancr_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
	
	local outcomes1    "shealth_binary mob_binary depression_binary pos_affect cancr" 
	

	loc count=1


    foreach var of local outcomes1 {  /*start var loop*/				
	    di "outcome `var'"	

		
    * Generate a consistent sample 
    mlogit multi_nomedhh sib_cnt numkids_all `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)
	
		** Changed at 1/17/2025 based on the latest email exchange with Dr. Anirban Basu and Norma.				
	    predict xb2 if e(sample), xb outcome(2)
	    predict xb3 if e(sample), xb outcome(3)
		
		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		
		
   * Test residuals 
	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp] if s1_sample==1, vce(cluster stateid)
	
	test (_b[res2]=0)
	test (_b[res3]=0)
	test (_b[res2]=0)(_b[res3]=0)
	
	
   * Directly compare
	mat A_`var' = J(4,3,.)
	matlist A_`var'
	
	probit `var' i.multi_nomedhh `controls`count'' [pw=rwtresp] if s1_sample==1, vce(cluster stateid) 
	margins[aweight=rwtresp], dydx(multi_nomedhh) post 
	
            matrix T1=r(b)
			matlist T1
            scalar fsMFX_multi2 = T1[1,2]
            scalar fsMFX_multi3 = T1[1,3]
	
            matrix T2=r(V)
            matlist T2
            scalar fsSE_multi2 = sqrt(T2[2,2])
            scalar fsSE_multi3 = sqrt(T2[3,3])
			
			mat A_`var'[1,1]=fsMFX_multi2
			mat A_`var'[2,1]=fsSE_multi2
			
			mat A_`var'[3,1]=fsMFX_multi3
			mat A_`var'[4,1]=fsSE_multi3
	
	
	probit `var' i.multi_nomedhh res2 res3 $controls [pw=rwtresp] if s1_sample==1, vce(cluster stateid) 
	margins[aweight=rwtresp], dydx(multi_nomedhh) post
	
            matrix T3=r(b)
			matlist T3
            scalar fsMFX_multi_iv2 = T3[1,2]
            scalar fsMFX_multi_iv3 = T3[1,3]
	
            matrix T4=r(V)
            matlist T4
            scalar fsSE_multi_iv2 = sqrt(T4[2,2])
            scalar fsSE_multi_iv3 = sqrt(T4[3,3])
			
			mat A_`var'[1,2]=fsMFX_multi_iv2
			mat A_`var'[2,2]=fsSE_multi_iv2
			
			mat A_`var'[3,2]=fsMFX_multi_iv3
			mat A_`var'[4,2]=fsSE_multi_iv3

	mat A_`var'[1,3] = (A_`var'[1,1]-A_`var'[1,2])/sqrt(A_`var'[2,1]^2+A_`var'[2,2]^2)
	matrix list A_`var'
	mat A_`var'[2,3] = 2*(1-normal(abs(A_`var'[1,3])))  //this line should give me the p-value 

	mat A_`var'[3,3] = (A_`var'[3,1]-A_`var'[3,2])/sqrt(A_`var'[4,1]^2+A_`var'[4,2]^2)
	mat A_`var'[4,3] = 2*(1-normal(abs(A_`var'[3,3])))  //this line should give me the p-value 	
	matrix list A_`var'
	
	loc count=`count'+1
	
	drop s1_sample xb2 xb3 p2 p3 res2 res3
	
	}	
	

	
	
********************************************************************************
     * IVs: numson numdaugh sib_cnt  * 
********************************************************************************
	
    * Generate Sample: Receiving LTC and over 50, 1+ADL/IADL Limitations
	
	use "$createddata/regressiondata_fnl.dta", clear

	egen sample = rowmiss(multi_nomedhh numson numdaugh sib_cnt ///
                      stateid wave adl_tot_any iadl_tot_any hatotb ///
                      female age_band educ_cat marital_stat race educ_m_yrs hispanic ///
					  medicare medicaid priv_insurance ltc_insur ///
					  us_born first_marriage ///
					  depression_binary rshlt demented_tm1 ever_stroke_tm1 rarelig bmi_cat drink_cat) 
					  
	recode sample (0=1) (else=0)
	tab sample, m 
	
	* Receiving LTC and over 50: 1+ADL/IADL limitation
	g s_ltc_1adl_50 = age_band>=2 & age_band<=9 & ltc_nomedhh==1 & rnhmliv!=1 & fc_combo!=1 & limit_tot >= 1 & !mi(limit_tot) & wave>=6 & sample==1 
	
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

	
	* Start the Hausman test *
	
    loc controls1 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
    loc controls2 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_nowalk iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"  
	
	forv i=3/4 {
    loc controls`i' "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"   
	}
	
    loc controls5 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(1).ever_cancr_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
	
	local outcomes1    "shealth_binary mob_binary depression_binary pos_affect cancr" 
	

	loc count=1


    foreach var of local outcomes1 {  /*start var loop*/				
	    di "outcome `var'"	

		
    * Generate a consistent sample 
    mlogit multi_nomedhh numson numdaugh sib_cnt `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)
	
		** Changed at 1/17/2025 based on the latest email exchange with Dr. Anirban Basu and Norma.				
	    predict xb2 if e(sample), xb outcome(2)
	    predict xb3 if e(sample), xb outcome(3)
		
		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		
		
   * Test residuals 
	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp] if s1_sample==1, vce(cluster stateid)
	
	test (_b[res2]=0)
	test (_b[res3]=0)
	test (_b[res2]=0)(_b[res3]=0)
	
	
   * Directly compare
	mat A_`var' = J(4,3,.)
	matlist A_`var'
	
	probit `var' i.multi_nomedhh `controls`count'' [pw=rwtresp] if s1_sample==1, vce(cluster stateid) 
	margins[aweight=rwtresp], dydx(multi_nomedhh) post 
	
            matrix T1=r(b)
			matlist T1
            scalar fsMFX_multi2 = T1[1,2]
            scalar fsMFX_multi3 = T1[1,3]
	
            matrix T2=r(V)
            matlist T2
            scalar fsSE_multi2 = sqrt(T2[2,2])
            scalar fsSE_multi3 = sqrt(T2[3,3])
			
			mat A_`var'[1,1]=fsMFX_multi2
			mat A_`var'[2,1]=fsSE_multi2
			
			mat A_`var'[3,1]=fsMFX_multi3
			mat A_`var'[4,1]=fsSE_multi3
	
	
	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp] if s1_sample==1, vce(cluster stateid) 
	margins[aweight=rwtresp], dydx(multi_nomedhh) post
	
            matrix T3=r(b)
			matlist T3
            scalar fsMFX_multi_iv2 = T3[1,2]
            scalar fsMFX_multi_iv3 = T3[1,3]
	
            matrix T4=r(V)
            matlist T4
            scalar fsSE_multi_iv2 = sqrt(T4[2,2])
            scalar fsSE_multi_iv3 = sqrt(T4[3,3])
			
			mat A_`var'[1,2]=fsMFX_multi_iv2
			mat A_`var'[2,2]=fsSE_multi_iv2
			
			mat A_`var'[3,2]=fsMFX_multi_iv3
			mat A_`var'[4,2]=fsSE_multi_iv3

	mat A_`var'[1,3] = (A_`var'[1,1]-A_`var'[1,2])/sqrt(A_`var'[2,1]^2+A_`var'[2,2]^2)
	matrix list A_`var'
	mat A_`var'[2,3] = 2*(1-normal(abs(A_`var'[1,3])))  //this line should give me the p-value 

	mat A_`var'[3,3] = (A_`var'[3,1]-A_`var'[3,2])/sqrt(A_`var'[4,1]^2+A_`var'[4,2]^2)
	mat A_`var'[4,3] = 2*(1-normal(abs(A_`var'[3,3])))  //this line should give me the p-value 	
	matrix list A_`var'
	
	loc count=`count'+1
	
	drop s1_sample xb2 xb3 p2 p3 res2 res3
	
	}	
	
	
	
	
********************************************************************************
     * IVs: numson numdaugh sib_bro_cnt sib_sis_cnt  * 
********************************************************************************
	
    * Generate Sample: Receiving LTC and over 50, 1+ADL/IADL Limitations
	
	use "$createddata/regressiondata_fnl.dta", clear

	egen sample = rowmiss(multi_nomedhh nnumson numdaugh sib_bro_cnt sib_sis_cnt ///
                      stateid wave adl_tot_any iadl_tot_any hatotb ///
                      female age_band educ_cat marital_stat race educ_m_yrs hispanic ///
					  medicare medicaid priv_insurance ltc_insur ///
					  us_born first_marriage ///
					  depression_binary rshlt demented_tm1 ever_stroke_tm1 rarelig bmi_cat drink_cat) 
					  
	recode sample (0=1) (else=0)
	tab sample, m 
	
	* Receiving LTC and over 50: 1+ADL/IADL limitation
	g s_ltc_1adl_50 = age_band>=2 & age_band<=9 & ltc_nomedhh==1 & rnhmliv!=1 & fc_combo!=1 & limit_tot >= 1 & !mi(limit_tot) & wave>=6 & sample==1 
	
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

	
	* Start the Hausman test *
	
    loc controls1 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
    loc controls2 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_nowalk iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"  
	
	forv i=3/4 {
    loc controls`i' "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"   
	}
	
    loc controls5 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(1).ever_cancr_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
	
	local outcomes1    "shealth_binary mob_binary depression_binary pos_affect cancr" 
	

	loc count=1


    foreach var of local outcomes1 {  /*start var loop*/				
	    di "outcome `var'"	

		
    * Generate a consistent sample 
    mlogit multi_nomedhh numson numdaugh sib_bro_cnt sib_sis_cnt `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)
	
		** Changed at 1/17/2025 based on the latest email exchange with Dr. Anirban Basu and Norma.				
	    predict xb2 if e(sample), xb outcome(2)
	    predict xb3 if e(sample), xb outcome(3)
		
		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		
		
   * Test residuals 
	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp] if s1_sample==1, vce(cluster stateid)
	
	test (_b[res2]=0)
	test (_b[res3]=0)
	test (_b[res2]=0)(_b[res3]=0)
	
	
   * Directly compare
	mat A_`var' = J(4,3,.)
	matlist A_`var'
	
	probit `var' i.multi_nomedhh `controls`count'' [pw=rwtresp] if s1_sample==1, vce(cluster stateid) 
	margins[aweight=rwtresp], dydx(multi_nomedhh) post 
	
            matrix T1=r(b)
			matlist T1
            scalar fsMFX_multi2 = T1[1,2]
            scalar fsMFX_multi3 = T1[1,3]
	
            matrix T2=r(V)
            matlist T2
            scalar fsSE_multi2 = sqrt(T2[2,2])
            scalar fsSE_multi3 = sqrt(T2[3,3])
			
			mat A_`var'[1,1]=fsMFX_multi2
			mat A_`var'[2,1]=fsSE_multi2
			
			mat A_`var'[3,1]=fsMFX_multi3
			mat A_`var'[4,1]=fsSE_multi3
	
	
	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp] if s1_sample==1, vce(cluster stateid) 
	margins[aweight=rwtresp], dydx(multi_nomedhh) post
	
            matrix T3=r(b)
			matlist T3
            scalar fsMFX_multi_iv2 = T3[1,2]
            scalar fsMFX_multi_iv3 = T3[1,3]
	
            matrix T4=r(V)
            matlist T4
            scalar fsSE_multi_iv2 = sqrt(T4[2,2])
            scalar fsSE_multi_iv3 = sqrt(T4[3,3])
			
			mat A_`var'[1,2]=fsMFX_multi_iv2
			mat A_`var'[2,2]=fsSE_multi_iv2
			
			mat A_`var'[3,2]=fsMFX_multi_iv3
			mat A_`var'[4,2]=fsSE_multi_iv3

	mat A_`var'[1,3] = (A_`var'[1,1]-A_`var'[1,2])/sqrt(A_`var'[2,1]^2+A_`var'[2,2]^2)
	matrix list A_`var'
	mat A_`var'[2,3] = 2*(1-normal(abs(A_`var'[1,3])))  //this line should give me the p-value 

	mat A_`var'[3,3] = (A_`var'[3,1]-A_`var'[3,2])/sqrt(A_`var'[4,1]^2+A_`var'[4,2]^2)
	mat A_`var'[4,3] = 2*(1-normal(abs(A_`var'[3,3])))  //this line should give me the p-value 	
	matrix list A_`var'
	
	loc count=`count'+1
	
	drop s1_sample xb2 xb3 p2 p3 res2 res3
	
	}	
	
	
	
********************************************************************************
     * IVs: numkids_all sib_bro_cnt sib_sis_cnt  * 
********************************************************************************
	
    * Generate Sample: Receiving LTC and over 50, 1+ADL/IADL Limitations
	
	use "$createddata/regressiondata_fnl.dta", clear

	egen sample = rowmiss(multi_nomedhh numkids_all sib_bro_cnt sib_sis_cnt ///
                      stateid wave adl_tot_any iadl_tot_any hatotb ///
                      female age_band educ_cat marital_stat race educ_m_yrs hispanic ///
					  medicare medicaid priv_insurance ltc_insur ///
					  us_born first_marriage ///
					  depression_binary rshlt demented_tm1 ever_stroke_tm1 rarelig bmi_cat drink_cat) 
					  
	recode sample (0=1) (else=0)
	tab sample, m 
	
	* Receiving LTC and over 50: 1+ADL/IADL limitation
	g s_ltc_1adl_50 = age_band>=2 & age_band<=9 & ltc_nomedhh==1 & rnhmliv!=1 & fc_combo!=1 & limit_tot >= 1 & !mi(limit_tot) & wave>=6 & sample==1 
	
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

	
	* Start the Hausman test *
	
    loc controls1 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
    loc controls2 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_nowalk iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"  
	
	forv i=3/4 {
    loc controls`i' "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"   
	}
	
    loc controls5 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(1).ever_cancr_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
	
	local outcomes1    "shealth_binary mob_binary depression_binary pos_affect cancr" 
	

	loc count=1


    foreach var of local outcomes1 {  /*start var loop*/				
	    di "outcome `var'"	

		
    * Generate a consistent sample 
    mlogit multi_nomedhh numkids_all sib_bro_cnt sib_sis_cnt `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)
	
		** Changed at 1/17/2025 based on the latest email exchange with Dr. Anirban Basu and Norma.				
	    predict xb2 if e(sample), xb outcome(2)
	    predict xb3 if e(sample), xb outcome(3)
		
		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		
		
   * Test residuals 
	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp] if s1_sample==1, vce(cluster stateid)
	
	test (_b[res2]=0)
	test (_b[res3]=0)
	test (_b[res2]=0)(_b[res3]=0)
	
	
   * Directly compare
	mat A_`var' = J(4,3,.)
	matlist A_`var'
	
	probit `var' i.multi_nomedhh `controls`count'' [pw=rwtresp] if s1_sample==1, vce(cluster stateid) 
	margins[aweight=rwtresp], dydx(multi_nomedhh) post 
	
            matrix T1=r(b)
			matlist T1
            scalar fsMFX_multi2 = T1[1,2]
            scalar fsMFX_multi3 = T1[1,3]
	
            matrix T2=r(V)
            matlist T2
            scalar fsSE_multi2 = sqrt(T2[2,2])
            scalar fsSE_multi3 = sqrt(T2[3,3])
			
			mat A_`var'[1,1]=fsMFX_multi2
			mat A_`var'[2,1]=fsSE_multi2
			
			mat A_`var'[3,1]=fsMFX_multi3
			mat A_`var'[4,1]=fsSE_multi3
	
	
	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp] if s1_sample==1, vce(cluster stateid) 
	margins[aweight=rwtresp], dydx(multi_nomedhh) post
	
            matrix T3=r(b)
			matlist T3
            scalar fsMFX_multi_iv2 = T3[1,2]
            scalar fsMFX_multi_iv3 = T3[1,3]
	
            matrix T4=r(V)
            matlist T4
            scalar fsSE_multi_iv2 = sqrt(T4[2,2])
            scalar fsSE_multi_iv3 = sqrt(T4[3,3])
			
			mat A_`var'[1,2]=fsMFX_multi_iv2
			mat A_`var'[2,2]=fsSE_multi_iv2
			
			mat A_`var'[3,2]=fsMFX_multi_iv3
			mat A_`var'[4,2]=fsSE_multi_iv3

	mat A_`var'[1,3] = (A_`var'[1,1]-A_`var'[1,2])/sqrt(A_`var'[2,1]^2+A_`var'[2,2]^2)
	matrix list A_`var'
	mat A_`var'[2,3] = 2*(1-normal(abs(A_`var'[1,3])))  //this line should give me the p-value 

	mat A_`var'[3,3] = (A_`var'[3,1]-A_`var'[3,2])/sqrt(A_`var'[4,1]^2+A_`var'[4,2]^2)
	mat A_`var'[4,3] = 2*(1-normal(abs(A_`var'[3,3])))  //this line should give me the p-value 	
	matrix list A_`var'
	
	loc count=`count'+1
	
	drop s1_sample xb2 xb3 p2 p3 res2 res3
	
	}	