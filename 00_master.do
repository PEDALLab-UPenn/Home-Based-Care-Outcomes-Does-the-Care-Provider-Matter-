
********************************************************************************
*	 Home-Based Care Outcomes: Does the Care Provider Matter?
*                        MASTER DO FILE	        
********************************************************************************

/*

GENERAL NOTES:
- This do-file defines folder and data globals and allows users to choose which sections and tables to run.
- Adjust the folder and data globals to replicate the results.

*/


********************************************************************************
	
	clear  
	clear matrix
	clear mata
	capture log close
	set more off
	set maxvar 20000
	set scheme s1color
	cap ssc install estout

		
		
********************************************************************************
	*	PART 1:  PREPARING GLOBALS & DEFINE PREAMBLE	  *
********************************************************************************

* FOLDER AND DATA GLOBALS

if 1 {

*select path
gl csun  1
gl name  0                      /* Enter your name */
 
	if $csun {
	gl folder 					"U:\Aim2_Final"  
	}

	if $name {
	gl folder					""   /* Enter location of main folder */
	}

}


* FOLDER GLOBALS

		gl do			   			"$folder\do"
		gl output		  			"$folder\output"
		gl log			  		 	"$folder\log"
		gl data			   			"$folder\data"
		gl createddata			   	"$folder\createddata"
		


* CHOOSE SECTIONS TO RUN
	
	loc cleanandmerge				1	
	loc firststage					1		
	loc otherstats                  1

	
	
********************************************************************************
*				PART 2:  RUN DO-FILES			*
********************************************************************************

* PART 01: CREATE DATASET	

	if `cleanandmerge' {
		do "$do/01_cleanandmerge.do"          
	}

	
* PART 2: RUN FIRST STAGE	
	
	if `firststage' {
		do "$do/02_fs_timet_main"          
		do "$do/03_fs_timet_fm"  
		do "$do/03_fs_timet_nosp"        
		do "$do/03_fs_timet_nowt"        
		do "$do/03_fs_timet_sondausisiv"   
		do "$do/03_fs_timet_tpcd"        
		do "$do/03_fs_timetp1"   
	}

	
* PART 3: RUN SECOND STAGE
	if `secondstage' {
		do "$do/05_2sri_timet_main"      
		do "$do/06_2sri_timet_female" 
		do "$do/06_2sri_timet_nosp"   
		do "$do/06_2sri_timet_nowt" 
		do "$do/06_2sri_timet_sondausisiv"   
		do "$do/06_2sri_timet_timep1"   
		do "$do/06_2sri_timet_tpcd"    		
	}
	
	
* PART 4: RUN OTHER STATISTICS
	if `otherstats' {
		do "$do/04_caregivingbysibs"      
		do "$do/07_balancingtable_stddiff" 
		do "$do/08_hausmantest"   
		do "$do/08_overidtest" 
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	