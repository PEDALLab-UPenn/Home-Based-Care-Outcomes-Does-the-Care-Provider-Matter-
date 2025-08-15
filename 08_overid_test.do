
********************************************************************************
							* IV Test * 
********************************************************************************

/* GENERAL NOTES: 

- This do-file runs the Overid Test

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

	
    log using "$log/Table1_Overidtest.log", replace 

	
	
	
** Set controls 

    loc controls1 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 
	
    loc controls2 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_nowalk iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"  
	
	forv i=3/4 {
    loc controls`i' "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat"   
	}
	
    loc controls5 "educ_m_yrs ib(2).race ib(2).race#c.educ_m_yrs ib(1).hispanic ib(1).female ib(6).age_band ib(4).marital_stat ib(0).first_marriage ib(0).us_born ib(1).educ_cat ib(1).networth_pctl adl_tot_any iadl_tot_any ib(5).rshlt ib(0).medicare ib(1).medicaid ib(0).priv_insurance ib(0).ltc_insur ib(1).demented_tm1 ib(1).ever_stroke_tm1 ib(1).ever_cancr_tm1 ib(4).rarelig i.stateid i.wave i.bmi_cat i.drink_cat" 


	
	
********************************************************************************
     * IVs: numson numdaugh sib_cnt  * 
********************************************************************************

    * Generate Sample: Receiving LTC and over 50, 1+ADL/IADL Limitations

	eststo clear
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
	
	sa "$createddata/ivtest1", replace 
	 
	 
	
    ******************************************
	** set 1: numson + sib_cnt VS. numdaugh **
	******************************************
	
	loc count=1

	foreach var in shealth_binary mob_binary depression_binary pos_affect cancr {
	

	use "$createddata/ivtest1", clear 
	
	* Generate a consistent sample 
	mlogit multi_nomedhh numson sib_cnt `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)

		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		

	* SECOND STAGE *
	keep if s1_sample==1

	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp], vce(cluster stateid)
		
		predict p4 if e(sample)
        gen res4=(`var'==1)-p4
		
		
	* Manual overid test 
	reg res4 numdaugh `controls`count'' [pw=rwtresp], vce(cluster stateid)
	regsave numdaugh using "$output\ivset1\set1_`count'", pval ci replace

	use "$output\ivset1\set1_`count'", clear 
	gen type="`var'"
	sa "$output\ivset1\set1_`count'", replace


	loc count=`count'+1
	
}

	use "$output\ivset1\set1_1", clear 
	forv i=2/5 {
	append using "$output\ivset1\set1_`i'"
	}

sa "$output\ivset1\output1-1", replace 




    ******************************************
	** set 2. numdaugh + sib_cnt VS. numson **
	******************************************

loc count=1

foreach var in shealth_binary mob_binary depression_binary pos_affect cancr {
	
	
	use "$createddata/ivtest1", clear 


* Generate a consistent sample 
mlogit multi_nomedhh numdaugh sib_cnt `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)

		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		

* SECOND STAGE *
keep if s1_sample==1

	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp], vce(cluster stateid)
		
		predict p4 if e(sample)
        gen res4=(`var'==1)-p4
		
		
* Manual overid test 
reg res4 numson `controls`count'' [pw=rwtresp], vce(cluster stateid)
regsave numson using "$output\ivset1\set2_`count'", pval ci replace

use "$output\ivset1\set2_`count'", clear 
gen type="`var'"
sa "$output\ivset1\set2_`count'", replace


loc count=`count'+1
	
}

use "$output\ivset1\set2_1", clear 
forv i=2/5 {
	append using "$output\ivset1\set2_`i'"
}

sa "$output\ivset1\output1-2", replace 




	
********************************************************************************
     * IVs: numson numdaugh sib_bro_cnt sib_sis_cnt  * 
********************************************************************************

    * Generate Sample: Receiving LTC and over 50, 1+ADL/IADL Limitations
	
	eststo clear
	use "$createddata/regressiondata_fnl.dta", clear

	egen sample = rowmiss(multi_nomedhh numson numdaugh sib_bro_cnt sib_sis_cnt ///
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
	
	sa "$createddata/ivtest2", clear 
	
	
********************************************************************************
** set1: numson + sib_bro_cnt VS. numdaugh + sib_sis_cnt
********************************************************************************

loc count=1

foreach var in shealth_binary mob_binary depression_binary pos_affect cancr {
	
	
	sa "$createddata/ivtest2", clear 


* Generate a consistent sample 
mlogit multi_nomedhh numson sib_bro_cnt `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)

		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		

* SECOND STAGE *
keep if s1_sample==1

	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp], vce(cluster stateid)
		
		predict p4 if e(sample)
        gen res4=(`var'==1)-p4
		
		
* Manual overid test 
reg res4 numdaugh sib_sis_cnt `controls`count'' [pw=rwtresp], vce(cluster stateid)
regsave numdaugh sib_sis_cnt using "$output\ivset2\set1_`count'", pval ci replace

use "$output\ivset2\set1_`count'", clear 
gen type="`var'"
sa "$output\ivset2\set1_`count'", replace


loc count=`count'+1
	
}

use "$output\ivset2\set1_1", clear 
forv i=2/5 {
	append using "$output\ivset2\set1_`i'"
}

sa "$output\ivset2\output2-1", replace 




	
********************************************************************************
** set2: numson + sib_sis_cnt VS. numdaugh + sib_bro_cnt
********************************************************************************
loc count=1

foreach var in shealth_binary mob_binary depression_binary pos_affect cancr {
	
	
	sa "$createddata/ivtest2", clear 


* Generate a consistent sample 
mlogit multi_nomedhh numson sib_sis_cnt `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)

		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		

* SECOND STAGE *
keep if s1_sample==1

	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp], vce(cluster stateid)
		
		predict p4 if e(sample)
        gen res4=(`var'==1)-p4
		
		
* Manual overid test 
reg res4 numdaugh sib_bro_cnt `controls`count'' [pw=rwtresp], vce(cluster stateid)
regsave numdaugh sib_bro_cnt using "$output\ivset2\set2_`count'", pval ci replace

use "$output\ivset2\set2_`count'", clear 
gen type="`var'"
sa "$output\ivset2\set2_`count'", replace


loc count=`count'+1
	
}

use "$output\ivset2\set2_1", clear 
forv i=2/5 {
	append using "$output\ivset2\set2_`i'"
}

sa "$output\ivset2\output2-2", replace 

	
	
	
	
********************************************************************************
** set3: numdaugh + sib_sis_cnt VS. numson + sib_bro_cnt 
********************************************************************************	
loc count=1

foreach var in shealth_binary mob_binary depression_binary pos_affect cancr {
	
	
	sa "$createddata/ivtest2", clear 


* Generate a consistent sample 
mlogit multi_nomedhh numdaugh sib_sis_cnt `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)

		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		

* SECOND STAGE *
keep if s1_sample==1

	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp], vce(cluster stateid)
		
		predict p4 if e(sample)
        gen res4=(`var'==1)-p4
		
		
* Manual overid test 
reg res4 numson sib_bro_cnt `controls`count'' [pw=rwtresp], vce(cluster stateid)
regsave numson sib_bro_cnt using "$output\ivset2\set3_`count'", pval ci replace

use "$output\ivset2\set3_`count'", clear 
gen type="`var'"
sa "$output\ivset2\set3_`count'", replace


loc count=`count'+1
	
}

use "$output\ivset2\set3_1", clear 
forv i=2/5 {
	append using "$output\ivset2\set3_`i'"
}

sa "$output\ivset2\output2-3", replace 


	

********************************************************************************
** set4: numdaugh + sib_bro_cnt VS. numson + sib_sis_cnt
********************************************************************************	
loc count=1

foreach var in shealth_binary mob_binary depression_binary pos_affect cancr {
	
	
	sa "$createddata/ivtest2", clear 


* Generate a consistent sample 
mlogit multi_nomedhh numdaugh sib_bro_cnt `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)

		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		

* SECOND STAGE *
keep if s1_sample==1

	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp], vce(cluster stateid)
		
		predict p4 if e(sample)
        gen res4=(`var'==1)-p4
		
		
* Manual overid test 
reg res4 numson sib_sis_cnt `controls`count'' [pw=rwtresp], vce(cluster stateid)
regsave numson sib_sis_cnt using "$output\ivset2\set4_`count'", pval ci replace

use "$output\ivset2\set4_`count'", clear 
gen type="`var'"
sa "$output\ivset2\set4_`count'", replace


loc count=`count'+1
	
}

use "$output\ivset2\set4_1", clear 
forv i=2/5 {
	append using "$output\ivset2\set4_`i'"
}

sa "$output\ivset2\output2-4", replace 
	
	
	
	
	
********************************************************************************
     * IVs: numkids_all sib_bro_cnt sib_sis_cnt  * 
********************************************************************************

    * Generate Sample: Receiving LTC and over 50, 1+ADL/IADL Limitations
	
	eststo clear
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
	
	sa "$createddata/ivtest3", replace 

	
	
	
********************************************************************************
** set1: numkids_all sib_bro_cnt VS. sib_sis_cnt
********************************************************************************

loc count=1

foreach var in shealth_binary mob_binary depression_binary pos_affect cancr {
	
	
    use "$createddata/ivtest3", clear 


* Generate a consistent sample 
mlogit multi_nomedhh numkids_all sib_bro_cnt `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)

		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		

* SECOND STAGE *
keep if s1_sample==1

	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp], vce(cluster stateid)
		
		predict p4 if e(sample)
        gen res4=(`var'==1)-p4
		
		
* Manual overid test 
reg res4 sib_sis_cnt `controls`count'' [pw=rwtresp], vce(cluster stateid)
regsave sib_sis_cnt using "$output\ivset3\set1_`count'", pval ci replace

use "$output\ivset3\set1_`count'", clear 
gen type="`var'"
sa "$output\ivset3\set1_`count'", replace


loc count=`count'+1
	
}

use "$output\ivset3\set1_1", clear 
forv i=2/5 {
	append using "$output\ivset3\set1_`i'"
}

sa "$output\ivset3\output3-1", replace 



	
********************************************************************************
** set2: numkids_all sib_sis_cnt VS. sib_bro_cnt
********************************************************************************
loc count=1

foreach var in shealth_binary mob_binary depression_binary pos_affect cancr {
	
	
    use "$createddata/ivtest3", clear 


* Generate a consistent sample 
mlogit multi_nomedhh numkids_all sib_sis_cnt `controls`count'' [pw=rwtresp] if s_ltc_1adl_50==1, vce (cluster stateid) base(1)	

	g s1_sample = e(sample)

		predict p2 if e(sample), outcome(2)
		predict p3 if e(sample), outcome(3)
				
	    gen res2=(multi_nomedhh==2)-p2
		gen res3=(multi_nomedhh==3)-p3
		

* SECOND STAGE *
keep if s1_sample==1

	probit `var' i.multi_nomedhh res2 res3 `controls`count'' [pw=rwtresp], vce(cluster stateid)
		
		predict p4 if e(sample)
        gen res4=(`var'==1)-p4
		
		
* Manual overid test 
reg res4 sib_bro_cnt `controls`count'' [pw=rwtresp], vce(cluster stateid)
regsave sib_bro_cnt using "$output\ivset3\set2_`count'", pval ci replace

use "$output\ivset3\set2_`count'", clear 
gen type="`var'"
sa "$output\ivset3\set2_`count'", replace


loc count=`count'+1
	
}

use "$output\ivset3\set2_1", clear 
forv i=2/5 {
	append using "$output\ivset3\set2_`i'"
}

sa "$output\ivset3\output3-2", replace 
	