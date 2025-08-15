# Home-Based Care Outcomes: Does the Care Provider Matter?

by Norma B. Coe, Chuxuan Sun, Courtney Harold Van Houtven, Anirban Basu, R.Tamara Konetzka

---

## Overview:

Before running the code:

Copy file contents into the project folder with the following subfolders: do, data, createddata, log, and output

Change the file path of the folder global ("gl folder") in 00_master.do to the location of the project folder  

Once these changes have been made, running the master file will produce the data and figures corresponding to the selected sections (noted after each local in 00_master.do). 

For questions about the code, please contact Chuxuan Sun (chuxuan.sun@pennmedicine.upenn.edu).

---

Register for access to the HRS and RAND HRS data on the HRS website (https://hrs.isr.umich.edu/data-products), then download the following files (both .dct and .da, or .dta where noted):

- RAND HRS Fat Files: h02f2c, h04f1c, h06f3a, h08f3a, hd10f5f, h12f3a, h14f2b, h16f2b, h18f2a

- RAND HRS Longitudinal File: randhrs1992_2018v2.dta

- RAND HRS Family Data Respondent-Kid File: randhrsfamk1992_2018v1.dta

- HRS Langa-Weir Classification of Cognitive Function: cogfinalimp_9518wide.dta

- HRS Core Helper File: H02G_HP, H04G_HP, H06G_HP, H08G_HP, H10G_HP, H12G_HP, H14G_HP, H16G_HP, h18g_hp

Place RAND HRS Fat Files in the “rand_fat” folder, place RAND HRS Longitudinal File and RAND HRS Family Data Respondent-Kid File in the “rand” folder, and place HRS Core Helper File in the “core” folder.

The primary analysis uses the HRS's restricted Cross-Wave Geographic Information (Detail) file. Place it in the “data” folder. 

HRS Restricted State File: hrshrsxgeo18v8b_r

---

## Running the code:

This code is for Stata MP, and has been verified to run in version 18.5. The estout package is required to output tables.

---

## Description of files:

The following describes how the files correspond to the inputs and output:

| File | Description | Inputs/Outputs | Notes |
|------|-------------|----------------|-------|
| 00_master.do | Sets macros for all variables, specifications, and replications used in the other files | — | Only edit the global folder and the individual global macros |
| 01_cleanandmerge.do | Cleans and merges all raw data files | Input: RAND HRS Fat Files, RAND HRS Longitudinal File, RAND HRS Family Data Respondent-Kid File, HRS Restricted State File, HRS Core Helper File, and HRS Langa-Weir Cognitive Function File<br>Output: $createddata/regressiondata_fnl.dta and $createddata/timep1/regressiondata_fnl.dta | — |
| 02_fs_timet_main | Runs summary statistics and first stage for the main analysis | Input: $createddata/regressiondata_fnl.dta<br>Output: Main Table 2, Table 3, and Table 5 | — |
| 03_fs_timet_fm;<br>03_fs_timet_nosp;<br>03_fs_timet_nowt;<br>03_fs_timet_sondausisiv;<br>03_fs_timet_tpcd;<br>03_fs_timetp1 | Runs first stage for robustness check and sensitivity analysis | Input: $createddata/regressiondata_fnl.dta; $createddata/timep1/regressiondata_fnl.dta<br>Output: Appendix Table 2 | — |
| 04_caregivingbysibs | Runs descriptive stats for the percent of helpers by relationship, by the number of siblings | Input: $createddata\regressiondata_fnl.dta<br>Output: Table 4 | — |
| 05_2sri_timet_main | Runs second stage for the main analysis | Input: $createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv.dta<br>Output: Main Table 6 | — |
| 06_2sri_timet_female;<br>06_2sri_timet_nosp;<br>06_2sri_timet_nowt;<br>06_2sri_timet_sondausisiv;<br>06_2sri_timet_timep1;<br>06_2sri_timet_tpcd | Runs second stage for robustness check and sensitivity analysis | Input: $createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv_fm.dta; $createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv_nosp.dta; $createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv_nowt.dta; $createddata/regressiondata_fnl_multi_nomedhh_sondausisiv.dta; $createddata/timep1/regressiondata_fnl_multi_nomedhh_sibkidsiv.dta; $createddata/regressiondata_fnl_multi_nomedhh_sibkidsiv_tpcd.dta<br>Output: Main Table 7 + Table 8 | — |
| 07_balancingtable_stddiff | Runs balancing table | Input: $createddata\regressiondata_fnl_multi_nomedhh_sibkidsiv.dta<br>Output: Appendix Table 1 | — |
| 08_hausmantest;<br>08_overidtest | Runs IV tests | Input: $createddata/regressiondata_fnl.dta<br>Output: Main Table 1 | — |
