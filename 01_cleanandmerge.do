

********************************************************************************
* This file details variables and locations for Informal Care Aim 2    	       *
* Created on: 1/12/2023                                                        *
* Written by CSUN                                                              *
* Notes: The outcome vars require RP to be alive at both time t and t+1 so we  *
*        don't need to add exit waves.                                         *
*        Based on the latest decision, paid trained home care is dropped from  *
*        the preferred definition of formal care.                              *
********************************************************************************


	clear  
	clear matrix
	clear mata
	capture log close
	set more off
	set maxvar 20000
	set scheme s1color
	cap ssc install estout
    eststo clear


	
	
********************************************************************************

*************************************************
* Pull cognitive function data from Langa Weir *
*************************************************

*************
* 2002-2018 *
*************	
clear all 

use hhid pn cogfunction* using "$data/cogfinalimp_9518wide.dta"
g hhidpn=hhid+pn
destring hhidpn, replace

reshape long cogfunction, i(hhidpn) j(year)

g wave=.
    replace wave=3 if year==1995
    replace wave=3 if year==1996
	replace wave=4 if year==1998
	replace wave=5 if year==2000
	replace wave=6 if year==2002
	replace wave=7 if year==2004
	replace wave=8 if year==2006
	replace wave=9 if year==2008
	replace wave=10 if year==2010
	replace wave=11 if year==2012
	replace wave=12 if year==2014
	replace wave=13 if year==2016
	replace wave=14 if year==2018
	
* For people who are not distinct at hhidpn wave level, check what is going on.
    bys hhidpn wave: gen tag=cond(_N==1,0,_n)
    ta tag, m

    bys hhidpn wave: replace cogfunction=cogfunction[_n+1] if cogfunction==.
    bys hhidpn wave: replace cogfunction=cogfunction[_n-1] if cogfunction==.

    bys hhidpn wave: gen tag2=cond(_N==1,0,_n)
    bys hhidpn wave cogfunction: gen tag3=cond(_N==1,0,_n)

    assert tag2==tag3
    keep if tag<=1
	drop tag*
	
* Recode dementia 
g demented= .
    replace demented=1 if cogfunction==3           //DEMENTED
	replace demented=0 if inlist(cogfunction,1,2)  //NORMAL + CIND

* Keep data from 2002 to 2018
keep if wave>=6  

keep hhidpn wave year cogfunction demented 
order hhidpn wave year cogfunction demented 

sa "$createddata/langaweir.dta", replace 

clear 


			
			
********************************************************************************

*****************************************
* Pull helper data from the core files *
*****************************************

*************
* 2002-2018 *
*************	
			
loc u  H02G_HP  H04G_HP	 H06G_HP  H08G_HP  H10G_HP  H12G_HP	 H14G_HP  H16G_HP  h18g_hp
loc wave = 6
loc n: word count `u'
forval i = 1/`n' {

	loc uu: word `i' of `u'	
	
	use "$data/core/`uu'.dta", clear
	ren *, upper
				
	loc g 	HG069   JG069   KG069   LG069   MG069   NG069   OG069   PG069   QG069
	loc h 	HG076   JG076   KG076   LG076   MG076   NG076   OG076   PG076   QG076

	loc g2: word `i' of `g'
	loc h2: word `i' of `h'
    
	rename (`g2' `h2')(help_rel paidstatus)
	
    g rel_cat=.
	    replace rel_cat = 1 if inlist(help_rel, 21, 22, 23, 24, 25)             //Professional
		replace rel_cat = 2 if inlist(help_rel, 2, 26, 27)                      //Spouse/partner 
		replace rel_cat = 3 if inlist(help_rel, 3, 8)                           //Son/daughter in law
		replace rel_cat = 4 if inlist(help_rel, 5, 6)                           //Daughter/son in law
		replace rel_cat = 5 if inlist(help_rel, 4, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 28, 30, 31, 33, 90, 91) //Other family relationship
		replace rel_cat = 6 if inlist(help_rel, 20, 32, 34, 98)                 //Other relationship 
		
	g informal=.
	    replace informal = 1 if paidstatus==5 & rel_cat==6                      //Code all unpaid other as informal 
		replace informal = 1 if inlist(rel_cat,2,3,4,5)                         //Force code all family as informal- regardless if they get paid
		
	g formal=.
	    replace formal = 1 if paidstatus==1 & rel_cat==6                        //Code all paid other as formal 
		replace formal = 1 if rel_cat==1                                        //Force code professionals as formal
					 
	g hhidpn = HHID+PN
	destring hhidpn, replace
			
	g wave = `wave'
	loc wave = `wave' + 1
			
	keep hhidpn wave informal formal		
	order hhidpn wave informal formal
			
	tempfile temp`i'
	sa `temp`i''
	clear
}

	use `temp1'
		forval j = 2/9 {
		append using `temp`j''
		}

		
* Generate informal vs. formal count variable
	bys hhidpn wave: egen informal_cnt = sum(informal), missing
    bys hhidpn wave: egen formal_cnt = sum(formal), missing

* Keep the first occurence for each respondent (alternative to collapsing)
	bys hhidpn wave: g val = _n
		keep if val==1
		drop val informal formal
		
sa "$createddata/helper.dta", replace 

clear




********************************************************************************

*********************************************************************
* Prepare the caregiver pattern data for descriptive statistics *
*********************************************************************

*************
* 2002-2018 *
*************	
			
loc u  H02G_HP  H04G_HP	 H06G_HP  H08G_HP  H10G_HP  H12G_HP	 H14G_HP  H16G_HP  h18g_hp
loc wave = 6
loc n: word count `u'
forval i = 1/`n' {

	loc uu: word `i' of `u'	
	
	use "$data/core/`uu'.dta", clear
	ren *, upper
				
	loc g 	HG069   JG069   KG069   LG069   MG069   NG069   OG069   PG069   QG069
	loc h 	HG076   JG076   KG076   LG076   MG076   NG076   OG076   PG076   QG076

	loc g2: word `i' of `g'
	loc h2: word `i' of `h'
    
	rename (`g2' `h2')(help_rel paidstatus)
	
    g rel_cat=.
	    replace rel_cat = 1 if inlist(help_rel, 21, 22, 23, 24, 25)             //Professional
		replace rel_cat = 2 if inlist(help_rel, 2, 26, 27)                      //Spouse/partner 
		replace rel_cat = 3 if inlist(help_rel, 3, 8)                           //Son/daughter in law
		replace rel_cat = 4 if inlist(help_rel, 5, 6)                           //Daughter/son in law
		replace rel_cat = 5 if inlist(help_rel, 4, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 28, 30, 31, 33, 90, 91) //Other family relationship
		replace rel_cat = 6 if inlist(help_rel, 20, 32, 34, 98)                 //Other relationship 
		
	g informal=.
	    replace informal = 1 if paidstatus==5 & rel_cat==6                      //Code all unpaid other as informal 
		replace informal = 1 if inlist(rel_cat,2,3,4,5)                         //Force code all family as informal- regardless if they get paid
		
	g formal=.
	    replace formal = 1 if paidstatus==1 & rel_cat==6                        //Code all paid other as formal 
		replace formal = 1 if rel_cat==1                                        //Force code professionals as formal
		
		
	** Add siblings for Main Table 4
    g rel_cat2=.
	    replace rel_cat2 = 1 if inlist(help_rel, 21, 22, 23, 24, 25)             //Professional
		replace rel_cat2 = 2 if inlist(help_rel, 2, 26, 27)                      //Spouse/partner 
		replace rel_cat2 = 3 if inlist(help_rel, 3, 8)                           //Son/daughter in law
		replace rel_cat2 = 4 if inlist(help_rel, 5, 6)                           //Daughter/son in law
		replace rel_cat2 = 5 if inlist(help_rel, 15, 17)                         //Siblings
		replace rel_cat2 = 6 if inlist(help_rel, 4, 7, 9, 10, 11, 12, 13, 14, 16, 18, 19, 28, 30, 31, 33, 90, 91) //Other family relationship
		replace rel_cat2 = 7 if inlist(help_rel, 20, 32, 34, 98)                 //Other relationship 
		
					 
	g hhidpn = HHID+PN
	destring hhidpn, replace
			
	g wave = `wave'
	loc wave = `wave' + 1
			
	keep hhidpn wave informal formal rel_cat2		
	order hhidpn wave informal formal rel_cat2
	
	** Label variable
	lab define plabel 1 "Professional" 2 "Spouse/partner " 3 "Son/daughter in law" ///
	4 "Daughter/son in law" 5 "Siblings" 6 "Other family relationship" 7 "Other relationship"
	
	lab values rel_cat2 plabel 
			
	tempfile temp`i'
	sa `temp`i''
	clear
}

	use `temp1'
		forval j = 2/9 {
		append using `temp`j''
		}

sa "$createddata/cgpattern.dta", replace  

clear




********************************************************************************

***********************************************
* Pull formal residential LTC care from core *
***********************************************

*************
* 2002-2018 *
*************

loc u  h02f2c  h04f1c   h06f3a   h08f3a   hd10f5f   h12f3a   h14f2b   h16f2b   h18f2a  
loc wave = 6
loc n: word count `u'
forval i = 1/`n' {

	loc uu: word `i' of `u'	
	
	use "$data/rand_fat/`uu'.dta", clear
	
    loc h  hh115  jh115  kh115  lh115  mh115  nh115  oh115  ph115  qh115  
	loc j  hh124  jh124  kh124  lh124  mh124  nh124  oh124  ph124  qh124 
    loc k  hh130  jh130  kh130  lh130  mh130  nh130  oh130  ph130  qh130 
	loc o  hh004  jh004  kh004  lh004  mh004  nh004  oh004  ph004  qh004

	loc h2: word `i' of `h'
	loc j2: word `i' of `j'
	loc k2: word `i' of `k'
	loc o2: word `i' of `o'

	rename (`h2' `j2' `k2' `o2')(meals adlhelp nursinghelp ltcrent)
  
  
    ** CS: The following coding method is generated by a previous programmer L.Taggart
	
    * INDEPENDENT LIVING = MEALS YES, ADL HELP NO, NURSING NO, RENT YES;  
	g il=0
	    replace il=1 if meals==1 & adlhelp==5 & nursinghelp==5 & ltcrent==2
		
    * ASSISTED LIVING = MEALS YES, ADL HELP YES, NURSING NO, RENT YES;
	g al=0
	    replace al=1 if meals==1 & adlhelp==1 & nursinghelp==5 & ltcrent==2		
	
	* CONTINUING CARE RETIREMENT COMMUNITY = MEALS YES, ADL HELP YES, NURSING YES, RENT OR OWN;
	g ccrc=0
	    replace ccrc=1 if meals==1 & adlhelp==1 & nursinghelp==1	
	
    ** COMBINE THREE ABOVE BECAUSE FREQUENCIES ARE SO LOW FOR THEM SEPARATELY;
	g fc_combo=0
	    replace fc_combo=1 if ccrc==1|al==1|il==1
	
	g wave = `wave'
	loc wave = `wave' + 1
	
	keep hhidpn wave fc_combo 
	order hhidpn wave fc_combo 
			
	tempfile temp`i'
	sa `temp`i''
	clear
}

	use `temp1'
		forval j = 2/9 {
		append using `temp`j''
		}

sa "$createddata/formalrescare.dta", replace 

clear	




********************************************************************************

*************************************************
* Pull kids count from RAND family file *
*************************************************

*************
* 2002-2018 *
*************	
clear all 
use hhidpn hhid opn kagenderbg karel k*agebg k*stat using "$data/rand/randhrsfamk1992_2018v1.dta"

* Keep only children and step-children	
keep if karel==1 | karel==2
	
* Change variable names to use reshape (wide to long)
forval x = 6/14 {
    foreach var in agebg stat {
        rename k`x'`var' `var'`x'
		}
    g kagenderbg`x' = kagenderbg
    g karel`x' = karel
	}
	
drop kagenderbg karel k1* k2* k3* k4* k5* kp* k*mstat

* Reshape data
reshape long kagenderbg karel agebg stat, i(hhidpn opn) j(wave)

* Generate kids relationship indicators
recode karel (1/2=1) (else=.),     g(kids_all)
recode karel (1=1) (2=0) (else=.), g(biokids) 
recode karel (1=0) (2=1) (else=.), g(stepkids) 

* Generate son/daughter regardless of bio/step status
g son=.
    replace son=1 if kagenderbg==1 
g daughter=.
    replace daughter=1 if kagenderbg==2 

* Drop children that died, are not in contact, or are not children... 
keep if stat<=3 | stat==5

* Generate the `sum' kid variables
bys hhidpn wave: egen numkids_all 	  = sum(kids_all)		
bys hhidpn wave: egen numstepkids 	  = sum(stepkids)	
bys hhidpn wave: egen numdaugh 	  	  = sum(daughter)
bys hhidpn wave: egen numson 	  	  = sum(son)
		
* Keep the first occurence for each respondent (alternative to collapsing)
bys hhidpn wave: g val = _n
keep if val==1

* Respond to Edward Norton's comments: make the kids variable ever 
foreach var of varlist numkids_all numstepkids numdaugh numson {
	bys hhidpn (wave): replace `var'=`var'[1]
}


keep hhidpn wave numkids_all numstepkids numdaugh numson 

sa "$createddata/RAND_family_fnl.dta", replace
	
clear




********************************************************************************

******************************************************
* Pull control variables from RAND longitudinal file *
******************************************************
use hhidpn r*iwendy r*iwstat r*wtresp r*agey_e r*shlt r*mstat r*mrct ///
r*walkr r*dress r*bath r*eat r*bed r*toilt r*phone r*money r*meds r*shop r*meals ///
r*govmr r*govmd r*hiltc r*livsib r*livbro r*livsis r*smoken r*smokev r*drink r*drinkd r*bmi ///
r*cesd r*cesdm r*prpcnt r*whappy r*enlife r*mobila r*nhmliv r*homcar r*stroke r*strok ///
r*arthre r*arthr r*cancre r*cancr r*proxy ///
h*atotb h*child h*itot ///
raracem rahispan raeduc ragender rameduc rabplace rabplacf rarelig using "$data/rand/randhrs1992_2018v2.dta"
	
	* Change variable names to use reshape (wide to long)
	forval x = 6 (1) 14 {
		foreach r_var in iwendy iwstat agey_e shlt mstat mrct ///
		                 walkr dress bath eat bed toilt phone money meds shop meals ///
						 govmr govmd hiltc prpcnt livsib livbro livsis ///
                         cesd cesdm whappy enlife mobila nhmliv homcar stroke strok ///
						 arthre arthr cancre cancr proxy smoken smokev drink drinkd bmi {
			rename r`x'`r_var' r`r_var'`x'
			}
		foreach h_var in atotb child itot{
			rename h`x'`h_var' h`h_var'`x'
			}
		foreach var in raracem rahispan ragender raeduc rameduc rabplace rabplacf rarelig {
			g `var'`x' = `var'
			}			
	}
	forval x = 6 (1) 13 {
		    rename r`x'wtresp rwtresp`x'
	}	
	
	drop raracem rahispan ragender raeduc rameduc rabplace rarelig rabplacf ///
	r1* r2* r3* r4* r5* h1* h2* h3* h4* h5*

	reshape long riwendy riwstat rwtresp ragey_e rshlt rmstat rmrct ///
	             rwalkr rdress rbath reat rbed rtoilt rphone rmoney rmeds rshop rmeals ///
				 rgovmr rgovmd rhiltc rprpcnt rlivsib rlivbro rlivsis ///
				 rcesd rcesdm rwhappy renlife rmobila rnhmliv rhomcar rstroke rstrok ///
				 rarthre rarthr rcancre rcancr ///
				 hatotb hchild hitot ///
				 raracem rahispan ragender raeduc rameduc rabplace rabplacf rarelig rproxy ///
				 rsmokev rsmoken rdrink rdrinkd rbmi, i(hhidpn) j(wave)

	keep hhidpn wave riwendy riwstat rwtresp ragey_e rshlt rmstat rmrct ///
	     rwalkr rdress rbath reat rbed rtoilt rphone rmoney rmeds rshop rmeals ///
		 rgovmr rgovmd rhiltc rprpcnt rlivsib rlivbro rlivsis ///
		 rcesd rcesdm rwhappy renlife rmobila rnhmliv rhomcar rstroke rstrok ///
		 rarthre rarthr rcancre rcancr ///		 
		 hatotb hchild hitot ///
		 raracem rahispan ragender raeduc rameduc rabplace rabplacf rarelig rproxy ///
		 rsmokev rsmoken rdrink rdrinkd rbmi
	
	
	*********************************************************
	* Recode demographic variables and adjust for inflation *
	*********************************************************	
	
	* Survey related variables
	rename riwendy year
	
	* Gender
	recode ragender (2=1)(1=0)(else=.), gen(female)
	
	* Race/Ethnicity
	recode raracem (1=1)(2=2)(3=3)(else=.), gen(race) 
	recode rahispan (1=1)(0=0)(else=.), gen(hispanic)
	
	* Age
	g age_band=.
	    replace age_band=1 if ragey_e<=50
		replace age_band=2 if ragey_e>50 & ragey_e<=55 
		replace age_band=3 if ragey_e>55 & ragey_e<=60 
		replace age_band=4 if ragey_e>60 & ragey_e<=65 
		replace age_band=5 if ragey_e>65 & ragey_e<=70 
		replace age_band=6 if ragey_e>70 & ragey_e<=75 
		replace age_band=7 if ragey_e>75 & ragey_e<=80 
		replace age_band=8 if ragey_e>80 & ragey_e<=85 
		replace age_band=9 if ragey_e>85 & !mi(ragey_e)

	* Education
	recode raeduc (1=1) (2/3=2) (4=3) (5=4) (else=.), g(educ_cat)
	recode rameduc (".m" ".d" ".r"=.), gen(educ_m_yrs)
	
	* Marital status
	recode rmstat (1/2=1) (4/6=2) (7=3) (3 8=4) (else=.), g(marital_stat)
	recode rmrct (".m" ".d" ".r"=.)(1=1)(2/10=0), g(first_marriage)
	
	* US-born
	recode rabplace (1/9=1)(10/11=0)(else=.), gen(us_born) 
	
	* Recode home health care variable
	recode rhomcar (1=1)(0=0)(else=.), gen(medhomehealth)
	
	* Generate the sum of adl and iadl variables
	* Following the coding logic: can't do/don't do/yes == 1
	foreach var in rbath rdress reat rbed rwalkr rtoilt { 	
	    recode `var' (1 2 9 =1) (0=0) (else=.), gen(`var'_any)
	}	
	foreach var in rphone rmoney rmeds rshop rmeals { 	
	    recode `var' (1 2 9 =1) (0=0) (else=.), gen(`var'_any)
	}		
	
	egen adl_tot_any = rowtotal(rbath_any rdress_any reat_any rbed_any rwalkr_any rtoilt_any), missing
	    egen rowmiss_adl=rowmiss(rbath_any rdress_any reat_any rbed_any rwalkr_any rtoilt_any) 
	egen adl_tot_nowalk = rowtotal(rbath_any rdress_any reat_any rbed_any rtoilt_any), missing
	    egen rowmiss_adlnowk=rowmiss(rbath_any rdress_any reat_any rbed_any rtoilt_any)  
	egen iadl_tot_any = rowtotal(rphone_any rmoney_any rmeds_any rshop_any rmeals_any), missing
	    egen rowmiss_iadl=rowmiss(rphone_any rmoney_any rmeds_any rshop_any rmeals_any)

	* Family chars
	recode rlivsib (".j" ".m" ".d" ".r"=.), gen(sib_cnt)
	recode rlivbro (".j" ".m" ".d" ".r"=.), gen(sib_bro_cnt)
	recode rlivsis (".j" ".m" ".d" ".r"=.), gen(sib_sis_cnt)
	recode hchild  (".j" ".m" ".d" ".r"=.), gen(hh_child_cnt)
	
	* Respond to Edward Norton's comments: make the sibling variable ever 	
	foreach var of varlist sib_cnt sib_bro_cnt sib_sis_cnt {
		bys hhidpn (wave): gen countnonmissing=sum(!mi(`var')) if !mi(`var')
		bys hhidpn (countnonmissing): gen `var'_new=`var'[1]
		replace `var'_new=. if mi(`var')
		drop countnonmissing
	}
		foreach var of varlist sib_cnt sib_bro_cnt sib_sis_cnt {
		bys hhidpn (wave): replace `var'=`var'_new
	}

	* Ever stroke/arthritis/cancer
	recode rstroke (1=1)(0=0)(else=.), gen(ever_stroke)
	recode rarthre (1=1)(0=0)(else=.), gen(ever_arthr)
	recode rcancre (1=1)(0=0)(else=.), gen(ever_cancr)
	
	* Smoke
		* Currently smokes
	recode rsmoken (1=1) (0=0) (else=.), g(smokes_current)	
	
		* Ever smokes
	recode rsmokev (1=1) (0=0) (else=.), g(smokes_ever) 
	
	* Under/normal/overweight 
	gen bmi_cat=.
	    replace bmi_cat=1 if rbmi<18.5
		replace bmi_cat=2 if rbmi>=18.5 & rbmi<25 
		replace bmi_cat=3 if rbmi>=25 & !mi(rbmi)
	
	* Days drink in a week
	recode rdrinkd(0=0)(1=1)(2=2)(3=3)(4=4)(5=5)(6=6)(7=7)(else=.), g(drink_cat)

	* Stroke/arthritis/cancer this wave 
	recode rstrok (1=1)(0=0)(else=.), gen(stroke)
	recode rarthr (1=1)(0=0)(else=.), gen(arthr)
	recode rcancr (1=1)(0=0)(else=.), gen(cancr)
	
	* Insurance related variables
	recode rgovmr (1=1)(0=0)(else=.), gen(medicare)
	recode rgovmd (1=1)(0=0)(else=.), gen(medicaid)
	recode rhiltc (1=1)(0=0)(else=.), gen(ltc_insur)
	recode rprpcnt (1/23=1)(0=0)(else=.), gen(priv_insurance) //# of Private Insurance Plans
	
	* Adjust for inflation- 2017 as base year	
	* CS: the following code is from Coe, Gopi, and Courtney (2023) LTCI paper. CS inherited from MOney who has been worked on the LTCI project. 
	loc x  1.67	 1.63	1.6		1.57	1.53	1.49	1.44	1.41	1.36	1.31	1.27	1.23	1.19	1.16	1.12	1.1		1.08	1.06	1.04	1.03	1.03	1.01	1		0.98    0.96
	loc y  1995	 1996	1997	1998	1999	2000	2001	2002	2003	2004	2005	2006	2007	2008	2009	2010	2011	2012	2013	2014	2015	2016	2017	2018    2019
		loc n : word count `x'
			forval i=1/`n' {
				loc a : word `i' of `x'
				loc b : word `i' of `y'
				replace hitot=hitot*`a' if year==`b'	//Incm: Total HHold / R+Sp only
				replace hatotb=hatotb*`a' if year==`b'	//Total all Assets inc. 
		}

		
		
	*********************************************************
	* Recode outcome variables  *
	*********************************************************		
	* Generate depression binary variable
	g depression_binary=.
	    replace depression_binary=1 if inlist(rcesd,4,5,6,7,8) & rcesdm==0
		replace depression_binary=0 if inlist(rcesd,0,1,2,3) & rcesdm==0

	* Generate positive affect binary variable
	g pos_affect=.
	    replace pos_affect=1 if rwhappy==1 & renlife==1
		replace pos_affect=0 if rwhappy==0 & renlife==0
		replace pos_affect=0 if rwhappy==1 & renlife==0
		replace pos_affect=0 if rwhappy==0 & renlife==1
		
	* Generate mobility variable
	recode rmobila (0/2=1)(3/5=0)(else=.), gen(mob_binary)
	
	* Generate self-reported binary variable	
	recode rshlt (1/3=1)(4/5=0)(else=.), gen(shealth_binary)
    recode rshlt (".m" ".d" ".r"=.)

	
keep hhidpn wave year riwstat rwtresp ///
     female race hispanic age_band educ_cat marital_stat first_marriage ///
     hh_child_cnt sib_cnt sib_bro_cnt sib_sis_cnt ///
	 adl_tot_any adl_tot_nowalk iadl_tot_any rowmiss* ///
     hitot hatotb medicare medicaid ltc_insur priv_insurance ///
	 pos_affect depression_binary mob_binary shealth_binary rshlt ///
	 rnhmliv educ_m_yrs us_born medhomehealth ever_stroke stroke rarelig ///
	 ever_arthr ever_cancr arthr cancr rcesdm rproxy ///
	 smokes_current smokes_ever rdrink rdrinkd rbmi bmi_cat drink_cat rabplace
	 
so hhidpn wave
sa "$createddata/RAND.dta", replace    

clear




********************************************************************************	

************************
* Pull state_id *
************************	
use "$data/hrsxgeo18v8b_r.dta", clear
    ren *, upper
    rename STATEUSPS state1

	rename ZIPCODE, lower
	recode URBRUR2013 (1/3=1)(4/9=0), gen(urban)
	
    g hhidpn = HHID+PN
    destring hhidpn, replace

    g wave=.
        replace wave=1 if regexm(WAVEA,"A")
        replace wave=2 if regexm(WAVEA,"B")|regexm(WAVEA,"C")
        replace wave=3 if regexm(WAVEA,"D")|regexm(WAVEA,"E")
        replace wave=4 if regexm(WAVEA,"F")
        replace wave=5 if regexm(WAVEA,"G")
        replace wave=6 if regexm(WAVEA,"H")
        replace wave=7 if regexm(WAVEA,"J")
        replace wave=8 if regexm(WAVEA,"K")
        replace wave=9 if regexm(WAVEA,"L")
        replace wave=10 if regexm(WAVEA,"M")
        replace wave=11 if regexm(WAVEA,"N")
        replace wave=12 if regexm(WAVEA,"O")
        replace wave=13 if regexm(WAVEA,"P")
        replace wave=14 if regexm(WAVEA,"Q")
	
    so hhidpn wave 
    drop if wave<6
    keep hhidpn wave state1 zipcode urban
    tempfile state
    sa `state'

			
			
			
********************************************************************************	

************************
* Combine all datasets *
************************

* Start with the RAND dataset
use "$createddata/RAND.dta", clear
	so hhidpn wave year

* Merge in helper data
merge 1:1 hhidpn wave using "$createddata/helper.dta", keep(master matched) nogen
sa "$createddata/regressiondata.dta", replace

* Merge in formal residential care data
merge 1:1 hhidpn wave using "$createddata/formalrescare.dta", keep(master matched) nogen
sa "$createddata/regressiondata.dta", replace

* Merge in cognitive data
merge 1:1 hhidpn wave using "$createddata/langaweir.dta", keep(master matched) nogen
sa "$createddata/regressiondata.dta", replace

* Merge in state file
merge 1:1 hhidpn wave using `state', keep(master matched) nogen

	* Assume no one moved
	so hhidpn wave
	bys hhidpn: replace state1 = state1[_n-1] if state1==""
    encode state1, g(stateid)	

* Merge in family data  
merge 1:1 hhidpn wave using "$createddata/RAND_family_fnl.dta",keep(master matched) nogen 
	so hhidpn wave	

sa "$createddata/regressiondata.dta", replace




********************************************************************************	

************************************************
* Prep for the analytic dataset for time t *
************************************************

use "$createddata/regressiondata.dta", clear 

* Replace kids count=0 if household kids count=0 & missing kids count
foreach var of varlist numkids_all numstepkids numdaugh numson {
	replace `var'=0 if `var'==. & hh_child_cnt==0
}

* Generate total limit variables 	
g limit_tot=adl_tot_any+iadl_tot_any

* Formal care breakout variables- formal helper only
** 2024.7.16: Norma said to take out the paid trained home care from the preferred definition of formal care. Drop it from the paper.
* Who is 0 does not matter in this study, because the multi-homehealth variable is defined based on those 1s. However, it may matter in other studies especially when look at those who did not receive care bc of unmet needs/no need at all (no difficulties doing things). 
g any_formal_helper=0
    replace any_formal_helper=1 if formal_cnt>0 & !mi(formal_cnt)

g formal_care_any_nomedhh=0
    replace formal_care_any_nomedhh=1 if any_formal_helper==1
	
	
* Informal care variables
g informal_care_any=0
    replace informal_care_any=1 if informal_cnt>0 & !mi(informal_cnt)
	
	
* Receiving LTC
g ltc_nomedhh=.
    replace ltc_nomedhh=1 if formal_care_any_nomedhh==1 | informal_care_any==1
	replace ltc_nomedhh=0 if formal_care_any_nomedhh==0 & informal_care_any==0
	
* Generate multi independent variables
g multi_nomedhh=.
    replace multi_nomedhh=1 if informal_care_any==1 & formal_care_any_nomedhh==0        //IC only
	replace multi_nomedhh=2 if informal_care_any==0 & formal_care_any_nomedhh==1        //FC only
	replace multi_nomedhh=3 if informal_care_any==1 & formal_care_any_nomedhh==1        //Both
	
* Generate outcome variables
so hhidpn wave 

	* Controls (baseline) at t-1
	bys hhidpn (wave): gen ever_stroke_tm1=ever_stroke[_n-1] if wave==wave[_n-1]+1 
	    bys hhidpn (wave): replace ever_stroke_tm1=1 if ever_stroke[_n-1]==1 & mi(ever_stroke_tm1)
	    bys hhidpn (wave): replace ever_stroke_tm1=0 if ever_stroke==0 & mi(ever_stroke_tm1)
		
		
	bys hhidpn (wave): gen ever_cancr_tm1=ever_cancr[_n-1] if wave==wave[_n-1]+1 
	    bys hhidpn (wave): replace ever_cancr_tm1=1 if ever_cancr[_n-1]==1 & mi(ever_cancr_tm1)
	    bys hhidpn (wave): replace ever_cancr_tm1=0 if ever_cancr==0 & mi(ever_cancr_tm1)
	
	
	bys hhidpn (wave): gen demented_tm1 = demented[_n-1] if wave==wave[_n-1]+1 
	    bys hhidpn (wave): replace demented_tm1=1 if demented[_n-1]==1 & mi(demented_tm1)
	    bys hhidpn (wave): replace demented_tm1=0 if demented==0 & mi(demented_tm1)
		

sa "$createddata/regressiondata_fnl_nolab.dta", replace


* Label define
lab define multill 			1 "Informal Care Only" 2 "Formal Care Only (No HH)" 3 "Both Care"
lab values multi_nomedhh 	multill
lab define us_bornl 		1 "US-born" 0 "Non-US born" 
lab values us_born 		    us_bornl
lab define urbanl 			1 "Urban" 0 "Rural" 
lab values urban 		    urbanl
lab define elabel 			1 "Less than High School" 2 "High School" 3 "Some College" 4 "College Plus"
lab values educ_cat 		elabel
lab define mlabel 			1 "Married" 2 "Divorced" 3 "Widowed" 4 "Unmarried"
lab values marital_stat 	mlabel
lab define mtlabel 			1 "First Time Marriage" 0 "Never or Multi-Marriage"
lab values first_marriage 	mtlabel
lab define rlabel 			1 "White/Caucasian" 2 "Black/African American" 3 "Other" 
lab values race 			rlabel
lab define hlabel 			1 "Hispanic" 0 "Non-Hispanic"
lab values hispanic 		hlabel
lab define flabel 			1 "Female" 0 "Male"
lab values female 		    flabel
lab define mrlabel 			1 "Covered by Medicare" 0 "Not Covered by Medicare"
lab values medicare 		mrlabel
lab define mdlabel 			1 "Covered by Medicaid" 0 "Not Covered by Medicaid"
lab values medicaid 		mdlabel
lab define pvlabel 			1 "Covered by PrivIns" 0 "Not Covered by PrivIns"
lab values priv_insurance 	pvlabel
lab define ltclabel 		1 "Covered by LTCI" 0 "Not Covered by LTCI"
lab values ltc_insur 		ltclabel
lab define alabel ///	
        1 "Age <50" ///
		2 "Age 50-55" /// 
		3 "Age 55-60" ///
        4 "Age 60-65" ///
		5 "Age 65-70" ///
		6 "Age 70-75" ///
        7 "Age 75-80" ///
		8 "Age 80-85" ///
		9 "Age >85"
lab values age_band        alabel

lab define drlabel ///
       0 "Days drink in a week: 0" ///
       1 "Days drink in a week: 1" ///
	   2 "Days drink in a week: 2" ///
	   3 "Days drink in a week: 3" ///
	   4 "Days drink in a week: 4" ///
	   5 "Days drink in a week: 5" ///
	   6 "Days drink in a week: 6" ///
	   7 "Days drink in a week: 7" 
lab values drink_cat       drlabel

	   
lab define bmilabel ///
       1 "Under weight" ///
       2 "Normal weight" ///
	   3 "Over weight" 
lab values bmi_cat         bmilabel
	   
	   
lab var multi_nomedhh       "Multi Outcomes- No Med HH"
lab var educ_m_yrs          "Mother's Education (# of years)"
lab var rshlt               "Slef-reported Health Status"
lab var adl_tot_any         "# of ADL"
lab var adl_tot_nowalk      "# of ADL (No Walk)"
lab var iadl_tot_any        "# of IADL"         
lab var educ_cat            "Education"   
lab var race                "Race"
lab var hispanic            "Ethinicity"
lab var female              "Female"
lab var marital_stat        "Marital Status"
lab var us_born             "US Born"
lab var rarelig             "Religiousity"
lab var first_marriage      "First Time Marriage"
lab var age_band            "Age"
lab var medicare            "Medicare Coverage"
lab var medicaid            "Medicaid Coverage"
lab var priv_insurance      "PrivIns Coverage"
lab var ltc_insur           "LTCI Coverage"
lab var ever_stroke         "Ever stroke"
lab var ever_arthr          "Ever Arthr"
lab var ever_cancr          "Ever Cancer"
lab var stroke              "Stroke this wave"
lab var arthr               "Arthr this wave"
lab var cancr               "Cancer this wave"
lab var demented            "Demented this wave"
lab var smokes_ever         "Ever smoke"
lab var smokes_current      "Currently smoke"

lab var numkids_all         "# of Kids (All)" 
lab var numson              "# of Son (Bio & Step)"
lab var numdaugh            "# of Daughter (Bio & Step)"
lab var numstepkids         "# of Stepkid"

lab var sib_cnt             "# of Sibling" 
lab var sib_bro_cnt         "# of Brothers" 
lab var sib_sis_cnt         "# of Sisters" 

lab var ever_stroke_tm1     "Stroke at t-1"
lab var ever_cancr_tm1      "Cancer at t-1" 
lab var demented_tm1        "Demented at t-1"

lab var drink_cat           "Days drink in a week"
lab var bmi_cat             "BMI Category"

lab var depression_binary   "Depression time t"
lab var mob_binary          "Mobility time t"
lab var pos_affect          "Affect time t"         
lab var shealth_binary      "Self-reported Health time t"
lab var cancr               "Cancer time t"

sa "$createddata/regressiondata_fnl.dta", replace




********************************************************************************	

************************************************
* Prep for the analytical dataset for time t+1 *
************************************************

use "$createddata/regressiondata.dta", clear 

* Replace kids count=0 if household kids count=0 & missing kids count
foreach var of varlist numkids_all numstepkids numdaugh numson {
	replace `var'=0 if `var'==. & hh_child_cnt==0
}

* Generate total limit variables 	
g limit_tot=adl_tot_any+iadl_tot_any

* Formal care breakout variables- formal helper only
** 2024.7.16: Norma said to take out the paid trained home care from the preferred definition of formal care. Drop it from the paper.
* Reminder: pay attention to the heterogeneity in those who didn't receive any help. Who is 0 for receving help does not matter in this study, because the multi-homehealth variable is defined based on those 1s. However, it may matter in other studies especially when look at those who did not receive care bc of unmet needs (need but didn't receive)/no need at all (no difficulties doing things). 

g formal_care_any_nomedhh=0
    replace formal_care_any_nomedhh=1 if formal_cnt>0 & !mi(formal_cnt)
	
	
* Informal care variables
g informal_care_any=0
    replace informal_care_any=1 if informal_cnt>0 & !mi(informal_cnt)
	
	
* Receiving LTC
g ltc_nomedhh=.
    replace ltc_nomedhh=1 if formal_care_any_nomedhh==1 | informal_care_any==1
	replace ltc_nomedhh=0 if formal_care_any_nomedhh==0 & informal_care_any==0
	
* Generate multi independent variables
g multi_nomedhh=.
    replace multi_nomedhh=1 if informal_care_any==1 & formal_care_any_nomedhh==0        //IC only
	replace multi_nomedhh=2 if informal_care_any==0 & formal_care_any_nomedhh==1        //FC only
	replace multi_nomedhh=3 if informal_care_any==1 & formal_care_any_nomedhh==1        //Both
	
* Generate outcome variables
so hhidpn wave 
by hhidpn (wave): gen depression_lead = depression_binary[_n+1] if wave==wave[_n+1]-1  //depression-cesd   
by hhidpn (wave): gen mobility_lead = mob_binary[_n+1] if wave==wave[_n+1]-1           //mobility   
by hhidpn (wave): gen affect_lead = pos_affect[_n+1] if wave==wave[_n+1]-1             //positive affect  
by hhidpn (wave): gen shealth_lead = shealth_binary[_n+1] if wave==wave[_n+1]-1        //self reported health

sa "$createddata/timep1/regressiondata_fnl_nolab.dta", replace


* Label define
lab define multill 			1 "Informal Care Only" 2 "Formal Care Only (No HH)" 3 "Both Care"
lab values multi_nomedhh 	multill
lab define us_bornl 		1 "US-born" 0 "Non-US born" 
lab values us_born 		    us_bornl
lab define urbanl 			1 "Urban" 0 "Rural" 
lab values urban 		    urbanl
lab define elabel 			1 "Less than High School" 2 "High School" 3 "Some College" 4 "College Plus"
lab values educ_cat 		elabel
lab define mlabel 			1 "Married" 2 "Divorced" 3 "Widowed" 4 "Unmarried"
lab values marital_stat 	mlabel
lab define mtlabel 			1 "First Time Marriage" 0 "Never or Multi-Marriage"
lab values first_marriage 	mtlabel
lab define rlabel 			1 "White/Caucasian" 2 "Black/African American" 3 "Other" 
lab values race 			rlabel
lab define hlabel 			1 "Hispanic" 0 "Non-Hispanic"
lab values hispanic 		hlabel
lab define flabel 			1 "Female" 0 "Male"
lab values female 		    flabel
lab define mrlabel 			1 "Covered by Medicare" 0 "Not Covered by Medicare"
lab values medicare 		mrlabel
lab define mdlabel 			1 "Covered by Medicaid" 0 "Not Covered by Medicaid"
lab values medicaid 		mdlabel
lab define pvlabel 			1 "Covered by PrivIns" 0 "Not Covered by PrivIns"
lab values priv_insurance 	pvlabel
lab define ltclabel 		1 "Covered by LTCI" 0 "Not Covered by LTCI"
lab values ltc_insur 		ltclabel
lab define alabel ///	
        1 "Age <50" ///
		2 "Age 50-55" /// 
		3 "Age 55-60" ///
        4 "Age 60-65" ///
		5 "Age 65-70" ///
		6 "Age 70-75" ///
        7 "Age 75-80" ///
		8 "Age 80-85" ///
		9 "Age >85"
lab values age_band         alabel

lab define drlabel ///
       0 "Days drink in a week: 0" ///
       1 "Days drink in a week: 1" ///
	   2 "Days drink in a week: 2" ///
	   3 "Days drink in a week: 3" ///
	   4 "Days drink in a week: 4" ///
	   5 "Days drink in a week: 5" ///
	   6 "Days drink in a week: 6" ///
	   7 "Days drink in a week: 7" 
lab values drink_cat       drlabel
	   
lab define bmilabel ///
       1 "Under weight" ///
       2 "Normal weight" ///
	   3 "Over weight" 
lab values bmi_cat         bmilabel


lab var multi_nomedhh       "Multi Outcomes- No Med HH"
lab var educ_m_yrs          "Mother's Education (# of years)"
lab var rshlt               "Slef-reported Health Status"
lab var adl_tot_any         "# of ADL"
lab var adl_tot_nowalk      "# of ADL (No Walk)"
lab var iadl_tot_any        "# of IADL"         
lab var educ_cat            "Education"   
lab var race                "Race"
lab var hispanic            "Ethinicity"
lab var female              "Female"
lab var marital_stat        "Marital Status"
lab var us_born             "US Born"
lab var rarelig             "Religiousity"
lab var first_marriage      "First Time Marriage"
lab var age_band            "Age"
lab var medicare            "Medicare Coverage"
lab var medicaid            "Medicaid Coverage"
lab var priv_insurance      "PrivIns Coverage"
lab var ltc_insur           "LTCI Coverage"
lab var ever_stroke         "Ever stroke"
lab var ever_arthr          "Ever Arthr"
lab var ever_cancr          "Ever Cancer"
lab var stroke              "Stroke this wave"
lab var arthr               "Arthr this wave"
lab var cancr               "Cancer this wave"
lab var demented            "Demented this wave"

lab var numkids_all         "# of Kids (All)" 
lab var numkids_bio         "# of Kids (Bio)"
lab var numson              "# of Son (Bio & Step)"
lab var numdaugh            "# of Daughter (Bio & Step)"
lab var numstepkids         "# of Stepkid"

lab var sib_cnt             "# of Sibling" 
lab var sib_bro_cnt         "# of Brothers" 
lab var sib_sis_cnt         "# of Sisters" 

lab var drink_cat           "Days drink in a week"
lab var bmi_cat             "BMI Category"

lab var depression_lead		"Depression wave+1"
lab var mobility_lead 		"Mobility wave+1"
lab var affect_lead 		"Affect wave+1"
lab var shealth_lead 		"Self-reported Health wave+1"

sa "$createddata/timep1/regressiondata_fnl.dta", replace

