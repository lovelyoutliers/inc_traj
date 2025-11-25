/////////////////////////////////////////////////////////////////////////////////

log using "X:\0PS_Kirkbride-Merle Schlief\Logs\data prep.smcl", replace

/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
//	Income trajectory project
//	
//	Authors: J Dykxhoorn & M Schlief 
//	Last updated: March 2024 (JD)
//	
//	This do file includes (originally saved as separate files): 
// 		01 data prep (income)
//		01 data prep (covariates)
//		02 trajectory modelling
//		03 descriptive statistics
//		03 regression 
//		04 missingness & sensitivity analysis
//
// Cohort H03 - baseline cohort
// - Born 1983-1996
// - Followed for dx from age 14 (1997-2016)
/////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////


/////////////////////////////////////////////////////////////////////////////////
**# 01 DATA PREP (COVARIATES)
/////////////////////////////////////////////////////////////////////////////////
{
	
cd "P:\0PS_Kirkbride-Merle Schlief\Data\"
use analytic_master_v3

drop lopnr_pseudo
drop nmoves*
drop mother_age* father_age*
drop smi_*

save, replace

merge 1:1 lopnr using  "cohortH03_2016_003_msc_inctraj_merle_v03.dta", keepusing(nmoves_0to6_sum2 nmoves_7to15_sum2 mother_age_at_birth father_age_at_birth)
drop if _m==2
drop _merge

save, replace


//Age
drop if birthyear<1990
drop if birthyear>1996


// See how many parents people have 
gen mor=0
	replace mor=1 if LopnrMor!=.
gen far= 0
	replace far=1 if LopnrFar!=.
gen admor=0
	replace admor=1 if LopnrAdMor!=.
gen adfar=0
	replace adfar=1 if LopnrAdFar!=.
gen nopar = 0
	replace nopar=1 if mor==0 & far==0 & admor==0 & adfar==0
gen n_par=3
	replace n_par=0 if nopar==1
	replace n_par=4 if (mor==1 & far==1 & admor==1 & adfar==1) 
	replace n_par=1 if (mor==1 & far==0 & admor==0 & adfar==0) |  (mor==0 & far==1 & admor==0 & adfar==0) | (mor==0 & far==0 & admor==1 & adfar==0) | (mor==0 & far==0 & admor==0 & adfar==1) 
	replace n_par=2 if (mor==1 & far==1 & admor==0 & adfar==0) | (mor==1 & far==0 & admor==1 & adfar==0) | (mor==1 & far==0 & admor==0 & adfar==1) | (mor==0 & far==1 & admor==1 & adfar==0) | (mor==0 & far==1 & admor==0 & adfar==1) | (mor==0 & far==0 & admor==1 & adfar==1)
	tab n_par, m
	
	
*drop if no parents 
	drop if n_par==0

// SMI 
label define binary 0 "No" 1 "Yes"

gen naff =0 
	replace naff=1 if dx_f2029psyc_h == 1
	lab var naff "Non-affective psychosis"
	lab val naff binary
	tab naff dx_f2029psyc_h, m

gen bp_psyc=0
	replace bp_psyc = 1 if dx_bipm_psyc_h == 1
	lab var bp_psyc "Bipolar/mania with psychosis"
	lab val bp_psyc binary
	tab bp_psyc dx_bipm_psyc_h, m

gen bp_nopsyc=0
	replace bp_nopsyc = 1 if dx_bipm_no_psyc_h==1
	lab var bp_nopsyc "Bipolar/mania without psychosis"
	lab val bp_nopsyc binary
	tab bp_nopsyc dx_bipm_no_psyc_h, m 

gen smi=0
	replace smi=1 if naff==1 | bp_psyc==1 | bp_nopsyc==1
	lab var smi "SMI - non-aff psychosis, bp with/without psychosis"
	lab val smi binary
	tab smi, m
	
	save, replace


// Update migrant status as there seem to be some missing parent info when that should not be the case
use "P:\0PS_Area-Level index_Dykxhoorn\data\rtb.dta"
keep LopNr FLand
save temp, replace

rename LopNr LopnrMor 
rename FLand flandmor 
merge 1:m LopnrMor using "analytic_master_v2.dta"
	drop if _merge==1
	drop _merge
	save analytic_master_v2.dta, replace
	
use temp, clear	
rename LopNr LopnrFar
rename FLand flandfar 
merge 1:m LopnrFar using "analytic_master_v2.dta"
	drop if _merge==1
	drop _merge	
	save analytic_master_v2.dta, replace

use temp, clear	
rename LopNr LopnrAdMor
rename FLand flandadmor 
merge 1:m LopnrAdMor using "analytic_master_v2.dta"
	drop if _merge==1
	drop _merge	
	save analytic_master_v2.dta, replace

use temp, clear	
rename LopNr LopnrAdFar
rename FLand flandadfar 
merge 1:m LopnrAdFar using "analytic_master_v2.dta"
	drop if _merge==1
	drop _merge	
	save analytic_master_v2.dta, replace

erase temp.dta 


tab fland2
gen mig =3
	replace mig = 1 if fland2!=23
	replace mig = 4 if fland2==23 & n_par==0
	
	replace mig = 2 if flandmor!="Sverige" & flandmor!="" & LopnrAdMor==.
	replace mig = 2 if flandfar!="Sverige" & flandfar!="" & LopnrAdFar==. 
	replace mig = 2 if flandadmor!="Sverige" & flandadmor!=""
	replace mig = 2 if flandadfar!="Sverige" & flandadfar!=""
	
	
	tab mig n_par, m
	
	lab def mig 1"Migrant" 2 "CoM" 3 "SB child of SB" 4 "SB no parents", replace
	
	lab val mig mig
	
lab var mig "Migrant status - updated"
	
 
// Sex 
gen sex = .
	replace sex=1 if kon==2
	replace sex=2 if kon==1
	
	lab def sex 1"Female" 2"Male", replace
	lab val sex sex 
	lab var sex "Sex, 1=Female, 2=Male"
	tab sex
	
// Birth cohort
gen cohort = . 
	replace cohort = 1 if birthyear<1994
	replace cohort = 2 if birthyear>1993
	lab def cohort 1"1990-1993" 2"1994-1997", replace
	lab val cohort cohort 
	lab var cohort "Birth cohort"
	tab cohort, m
	
// Number of residential moves 

gen moves = nmoves_0to6_sum2 + nmoves_7to15_sum2
	replace moves = nmoves_0to6_sum2 if moves==.
	replace moves = nmoves_7to15_sum2 if moves==.

gen moves2 = . 
	replace moves2 =0 if moves == 0 
	replace moves2 = 1 if moves ==1 
	replace moves2=2 if moves >1
	replace moves2=. if moves==. 
	
	tab moves moves2, m

	lab def moves 0 "0 moves" 1 "1 move" 2 "2 or more"
	lab val moves2 moves
	lab var moves2 "Number of moves, ages 0-15"
	tab moves2, m


 // parental age at birth 
 recode mother_age_at_birth (min/24.999999 = 1) (25.0/34.99999 = 2) (35.0/44.999999 = 3) (45.0/max = 4), generate(mor_age)
	bysort mor_age: sum mother_age_at_birth
		lab def age 1"<25" 2"25-34" 3"35-44" 4"44 or older"
		lab val mor_age age
		lab var mor_age "Maternal age at birth, 4 categories"
			tab mor_age, m

 
recode father_age_at_birth (min/24.999999 = 1) (25.0/34.99999 = 2) (35.0/44.999999 = 3) (45.0/max = 4), generate(far_age)
	bysort far_age: sum father_age_at_birth
		lab val far_age age
		lab var far_age "Paternal age at birth, 4 categories"
			tab far_age, m
	
	tab mor_age far_age, m
	
// Deprivation quintile at birth 
tab dep5_birth

replace dep5_birth=dep5_1990 if dep5_birth==. & birthyear==1989
replace dep5_birth=dep5_1991 if dep5_birth==. & birthyear==1990
replace dep5_birth=dep5_1992 if dep5_birth==. & birthyear==1991
replace dep5_birth=dep5_1993 if dep5_birth==. & birthyear==1992
replace dep5_birth=dep5_1994 if dep5_birth==. & birthyear==1993
replace dep5_birth=dep5_1995 if dep5_birth==. & birthyear==1994
replace dep5_birth=dep5_1996 if dep5_birth==. & birthyear==1995
replace dep5_birth=dep5_1997 if dep5_birth==. & birthyear==1996


// Parental history of SMI 
*tbh not sure where this data came from. merging in from previous analytic master - which is missing 99,726 people and I CANT FIGURE OUT WHY
merge 1:1 lopnr using analytic_master, keepusing(smi_dad smi_mom)
	
	replace smi_dad=0 if smi_dad==.
	replace smi_mom=0 if smi_mom==.
	
	gen parent_smi =0
		replace parent_smi =1 if smi_mom==1  | smi_dad==1
 
 // Consumption-weighted parental income at birth 
 cd "P:\0PS_Area-Level index_Dykxhoorn\data\lisa\"
   forval x=1990/2004 {
 use lisa`x', clear
	rename LopNr lopnr
 
	gen LopnrMor = lopnr
	gen LopnrFar = lopnr
	gen LopnrAdMor = lopnr 
	gen LopnrAdFar = lopnr 

	gen DispInkPersMor`x' = DispInkPersF
	gen DispInkPersFar`x' = DispInkPersF
	gen DispInkPersAdMor`x' = DispInkPersF
	gen DispInkPersAdFar`x' = DispInkPersF 

save "temp`x'.dta", replace
 }
   forval x=2005/2008 {
 use lisa`x', clear
	rename LopNr lopnr
 
	gen LopnrMor = lopnr
	gen LopnrFar = lopnr
	gen LopnrAdMor = lopnr 
	gen LopnrAdFar = lopnr 

	gen DispInkPersMor`x' = DispInkPersF04
	gen DispInkPersFar`x' = DispInkPersF04
	gen DispInkPersAdMor`x' = DispInkPersF04
	gen DispInkPersAdFar`x' = DispInkPersF04 

save "temp`x'.dta", replace
 }
 
use "P:\0PS_Kirkbride-Merle Schlief\Data\analytic_master_v2"
    forval x=1990/1998 {
 merge m:1 LopnrMor using temp`x', keepusing(DispInkPersMor`x')
	drop if _m==2
	drop _merge
 merge m:1 LopnrFar using temp`x', keepusing(DispInkPersFar`x')
	drop if _m==2
	drop _merge
merge m:1 LopnrAdMor using temp`x', keepusing(DispInkPersAdMor`x')
	drop if _m==2
	drop _merge
 merge m:1 LopnrAdFar using temp`x', keepusing(DispInkPersAdFar`x')
	drop if _m==2
	drop _merge
	
	save "P:\0PS_Kirkbride-Merle Schlief\Data\analytic_master_v2", replace
 }

*Generate highest income 	
forval x=1990/1998 {
	
gen highest_consinc_`x' = DispInkPersAdMor`x'
	replace highest_consinc_`x' = DispInkPersAdFar`x' if adfar==1 & highest_consinc_`x'==. 
		replace highest_consinc_`x' = DispInkPersAdFar`x' if adfar==1 & highest_consinc_`x'!=. & (DispInkPersAdFar`x' > highest_consinc_`x')
		
	replace highest_consinc_`x' = DispInkPersMor`x' if mor==1 & DispInkPersMor`x'!=. & highest_consinc_`x'==. 
		replace highest_consinc_`x' = DispInkPersMor`x' if (mor==1 & admor==0) & DispInkPersMor`x'!=. & (DispInkPersMor`x' > highest_consinc_`x')

	replace highest_consinc_`x' = DispInkPersFar`x' if far==1 & DispInkPersFar`x'!=. & highest_consinc_`x'==. 
		replace highest_consinc_`x' = DispInkPersFar`x' if (far==1 & adfar==0) & DispInkPersFar`x'!=. & (DispInkPersFar`x' > highest_consinc_`x')
			
}
	
gen inc_birth=. 

forval x=1990/1996 {
replace inc_birth=highest_consinc_`x' if birthyear==`x'
}

replace inc_birth=dep5_1990 if inc_birth==. & birthyear==1989
replace inc_birth=dep5_1991 if inc_birth==. & birthyear==1990
replace inc_birth=dep5_1992 if inc_birth==. & birthyear==1991
replace inc_birth=dep5_1993 if inc_birth==. & birthyear==1992
replace inc_birth=dep5_1994 if inc_birth==. & birthyear==1993
replace inc_birth=dep5_1995 if inc_birth==. & birthyear==1994
replace inc_birth=dep5_1996 if inc_birth==. & birthyear==1995
replace inc_birth=dep5_1997 if inc_birth==. & birthyear==1996

save, replace

 
     forval x=1990/2004 {
 erase "temp`x'.dta"
 }
 
 
 ////////////////////////////////////////////////////////////////////////////////
 // Area-level variables
 *sams
 *pop density
 *deprivation 
 
 // Population density at birth
 * 1 = very rural /less than 26.5pp/km2
 * 2 = rural and semi rural (26.3-260.0)
 * 3 Metropolitan, suburban and urban (260.0 or more)
 
 
 *SAMS at birth
 *use sams in year of birth and also bring forward sams in the next year 
 
gen sams_birth=. 
forval x=1990/1998 {
replace sams_birth=sams`x' if birthyear==`x'
}

destring doesams, gen(sams_doe) force
replace sams_birth=sams_doe if sams_birth==.

replace sams_birth=sams1991 if sams_birth==. & birthyear==1990
replace sams_birth=sams1992 if sams_birth==. & birthyear==1991
replace sams_birth=sams1993 if sams_birth==. & birthyear==1992
replace sams_birth=sams1994 if sams_birth==. & birthyear==1993
replace sams_birth=sams1995 if sams_birth==. & birthyear==1994
replace sams_birth=sams1996 if sams_birth==. & birthyear==1995
replace sams_birth=sams1997 if sams_birth==. & birthyear==1996

replace sams_birth=sams1992 if sams_birth==. & birthyear==1990
replace sams_birth=sams1993 if sams_birth==. & birthyear==1991
replace sams_birth=sams1994 if sams_birth==. & birthyear==1992
replace sams_birth=sams1995 if sams_birth==. & birthyear==1993
replace sams_birth=sams1996 if sams_birth==. & birthyear==1994
replace sams_birth=sams1997 if sams_birth==. & birthyear==1995
replace sams_birth=sams1998 if sams_birth==. & birthyear==1996
	br if sams_birth==. 
	*166 people 
	
	
*create a yearly value to merge in info from pop density and dep library 
	forval x=1990/1998 {
		gen sams_birth`x' = sams_birth if birthyear==`x'
	}
save, replace


*re-bring in pop dens and deprivation 
use "P:\K9_FEPI_Projekt_Enterprise\SAMSLibrary\SAMS\Deprivation\combined_dataset_with_variables_created.dta", clear
 
save temp, replace

keep sams popdens* dep5_*
	destring sams, replace force

forval x=1989/1998 {
	gen sams_birth`x' = sams 
	bysort sams_birth`x': keep if _n==1
	}

drop popdens1982 popdens1983 popdens1984 popdens1985 popdens1986 popdens1987 popdens1988 popdens2000 popdens2001 popdens2002 popdens2003 popdens2004 popdens2005 popdens2006 popdens2007 popdens2008 popdens2009 popdens2010 popdens2011 dep5_1982 dep5_1983 dep5_1984 dep5_1985 dep5_1986 dep5_1987 dep5_1988  dep5_1999 dep5_2000 dep5_2001 dep5_2002 dep5_2003 dep5_2004 dep5_2005 dep5_2006 dep5_2007 dep5_2008 dep5_2009 dep5_2010

save temp, replace



*Merge with missingness file 
use missingness.dta, clear
drop popdens* 

forval x=1989/1998 { 
	use temp, clear 
	keep sams_birth`x' popdens`x' dep5_`x'
	merge 1:m sams_birth`x' using missingness.dta
		drop if _m==1
		drop _merge 
		save missingness.dta, replace
}

gen popdens_birth=.
	forval x=1990/1996 {
	replace popdens_birth = popdens`x'
	
}

//population density
gen popdens3=.
	lab var popdens3 "Pop dens at birth, 3 categories"
	
	replace popdens3 = 1 if popdens_birth <26.5 & sams_birth!=.
	replace popdens3 = 2 if popdens_birth >26.4999 & popdens_birth <260.0 & sams_birth!=.
	replace popdens3 = 3 if popdens_birth >259.999 & sams_birth!=.
*164, of which 99 are in the actual analysis (sob)


// Deprivation 
gen dep5_birth = .
	forval x=1990/1996 {
	replace dep5_birth = dep5_`x' if birthyear==`x' & sams_birth!=.
	
}

bysort birthyear: tab sams_birth if dep5_birth==. & sams_birth!=.

	*have a valid SAMS but the SAMS does not have a value of deprivation in that yearly
	
	replace dep5_birth=dep5_1991 if dep5_birth==. & birthyear==1990 & sams_birth!=.
	replace dep5_birth=dep5_1992 if dep5_birth==. & birthyear==1991 & sams_birth!=.
	replace dep5_birth=dep5_1993 if dep5_birth==. & birthyear==1992 & sams_birth!=.
	replace dep5_birth=dep5_1994 if dep5_birth==. & birthyear==1993 & sams_birth!=.
	replace dep5_birth=dep5_1995 if dep5_birth==. & birthyear==1994 & sams_birth!=.
	replace dep5_birth=dep5_1996 if dep5_birth==. & birthyear==1995 & sams_birth!=.
	replace dep5_birth=dep5_1997 if dep5_birth==. & birthyear==1996 & sams_birth!=.


	tab dep5_birth, m
	
// Cleaning dataset
order _all, alphabetic
order lopnr, first

	
}


/////////////////////////////////////////////////////////////////////////////////
**# 01 DATA PREP (INCOME)
/////////////////////////////////////////////////////////////////////////////////
{
	// Family income in each year
{


*1980-1989
*uses older census data, before Lisa introduced - only collected every five years
*generate total population deciles and quintiles
use "Y:\Enterprise\JenHen\MASTERS\Income\InkomsterTillg19681989.dta"

gen inc_80=DISP80
xtile inc5_80=DISP80, nq(5)
	sort DISP80
	br
	tab inc5_80
	
 forval x=81/89 {
	gen inc`x' = DISP`x'
	xtile inc5_`x' = DISP`x', nq(5)
	xtile inc10_`x' = DISP`x', nq(10)
	}

	rename LopNr lopnr 
	
	keep lopnr inc5_80-inc10_89
	save "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnr_inc8089.dta", replace
	
	
*convert all variables to have Mor so I can know what is from mother
*install renvar package
use "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnr_inc8089.dta"	
renvars _all,  prefix(Mor)
		rename MorLopnrMor LopnrMor
	save "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnrmor_inc8089.dta", replace

use "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnr_inc8089.dta"	
	
	renvars _all, prefix(Far)
	rename Farlopnr LopnrFar
	save "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnrfar_inc8089.dta", replace


use "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnr_inc8089.dta"	
renvars _all,  prefix(AdMor)
		rename AdMorlopnr LopnrAdMor
	save "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnradmor_inc8089.dta", replace

use "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnr_inc8089.dta"	
	
	renvars _all, prefix(AdFar)
	rename AdFarlopnr LopnrAdFar
	save "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnradfar_inc8089.dta", replace





*use "Y:\Enterprise\JenHen\MASTERS\Multigen registry\famid9097_num\famid9097_num.dta"



// Merge Parental Lopnrs 
use "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\cohortH03_2016_003_msc_inctraj_merle_v02.dta", clear

merge 1:1 lopnr using "Y:\Enterprise\Private\OtherLinkages\ParentLinkage\parents.dta", keepusing(Lopnr*)
	drop if _m==2
	drop _merge

	save, replace

merge m:1 LopnrMor using "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnrmor_inc8089.dta"
	drop if _m==2
	drop _merge


merge m:1 LopnrFar using "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnrfar_inc8089.dta"
	drop if _m==2
	drop _merge


merge m:1 LopnrAdMor using "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnradmor_inc8089.dta"
	drop if _m==2
	drop _merge


merge m:1 LopnrAdFar using "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\lopnradfar_inc8089.dta"
	drop if _m==2
	drop _merge


///////////////////////////////////////////////////
// create combined parental income for each year and highest fifth and 10th of income 


*Annual Family Income 
gen faminc83=.
	replace faminc83=AdMorinc83+AdFarinc83
	replace faminc83=AdMorinc83 if LopnrAdFar==.
	replace faminc83=AdFarinc83 if LopnrAdMor==.
	
	replace faminc83=Morinc83 + Farinc83 if faminc83==.
	replace faminc83=Morinc83 if faminc83==.
	replace faminc83=Farinc83 if faminc83==.
	
	
 forval x=84/89 {
	gen faminc`x'=.
	replace faminc`x'=AdMorinc`x'+AdFarinc`x'
	replace faminc`x'=AdMorinc`x' if faminc`x'==.
	replace faminc`x'=AdFarinc`x' if faminc`x'==.
	
	replace faminc`x'=Morinc`x' + Farinc`x' if faminc`x'==.
	replace faminc`x'=Morinc`x' if faminc`x'==.
	replace faminc`x'=Farinc`x' if faminc`x'==.
	}
	
*Annual family income quintile
 forval x=83/89 {
	gen faminc5_`x'=.
	replace faminc5_`x'=AdMorinc5_`x' if (AdMorinc5_`x' >= AdFarinc5_`x')
	replace faminc5_`x'=AdFarinc5_`x' if (AdFarinc5_`x' >= AdMorinc5_`x')
	replace faminc5_`x'=AdMorinc5_`x' if faminc5_`x'==.
	replace faminc5_`x'=AdFarinc5_`x' if faminc5_`x'==.
	
	replace faminc5_`x'=Morinc5_`x' if (Morinc5_`x' >= Farinc5_`x') & faminc5_`x'==.
	replace faminc5_`x'=Farinc5_`x' if (Farinc5_`x' >= Morinc5_`x') & faminc5_`x'==.
	replace faminc5_`x'=Morinc5_`x' if faminc5_`x'==.
	replace faminc5_`x'=Farinc5_`x' if faminc5_`x'==.
}


*annual family income decile
 forval x=83/89 {
	gen faminc10_`x'=.
	replace faminc10_`x'=AdMorinc10_`x' if (AdMorinc10_`x' >= AdFarinc10_`x')
	replace faminc10_`x'=AdFarinc10_`x' if (AdFarinc10_`x' >= AdMorinc10_`x')
	replace faminc10_`x'=AdMorinc10_`x' if faminc10_`x'==.
	replace faminc10_`x'=AdFarinc10_`x' if faminc10_`x'==.
	
	replace faminc10_`x'=Morinc10_`x' if (Morinc10_`x' >= Farinc10_`x') & faminc10_`x'==.
	replace faminc10_`x'=Farinc10_`x' if (Farinc10_`x' >= Morinc10_`x') & faminc10_`x'==.
	replace faminc10_`x'=Morinc10_`x' if faminc10_`x'==.
	replace faminc10_`x'=Farinc10_`x' if faminc10_`x'==.
}
	
	replace faminc10_`x'=AdMorinc`x' if faminc`x'==.
	replace faminc10_`x'=AdFarinc`x' if faminc`x'==.
	
	replace faminc10_`x'=Morinc`x' + Farinc`x' if faminc`x'==.
	replace faminc10_`x'=Morinc`x' if faminc`x'==.
	replace faminc10_`x'=Farinc`x' if faminc`x'==.
	}

drop Morinc5_80-AdFarinc10_89

save, replace

	
// LISA 
	cd "Y:\Enterprise\JenHen\MASTERS\LISA\LISA19902010"

 forval x=90/97 {
 use lisa`x'_lopnr, clear
 
	rename lopnr LopnrMor
	gen Morfaminc`x' = dispinkfam
	xtile Morfaminc5_`x' = dispinkfam, n(5)
	xtile Morfaminc10_`x' = dispinkfam, n(10)
	
	save "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\mor`x'.dta", replace
}
 forval x=90/97 {
 use lisa`x'_lopnr, clear
 
	rename lopnr LopnrFar
	gen Farfaminc`x' = dispinkfam
	xtile Farfaminc5_`x' = dispinkfam, n(5)
	xtile Farfaminc10_`x' = dispinkfam, n(10)
	
	save "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\far`x'.dta", replace
}

 forval x=90/97 {
 use lisa`x'_lopnr, clear
 
	rename lopnr LopnrAdMor
	gen AdMorfaminc`x' = dispinkfam
	xtile AdMorfaminc5_`x' = dispinkfam, n(5)
	xtile AdMorfaminc10_`x' = dispinkfam, n(10)
	
	save "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\admor`x'.dta", replace
}
 forval x=90/97 {
 use lisa`x'_lopnr, clear
 
	rename lopnr LopnrAdFar
	gen AdFarfaminc`x' = dispinkfam
	xtile AdFarfaminc5_`x' = dispinkfam, n(5)
	xtile AdFarfaminc10_`x' = dispinkfam, n(10)
	
	save "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\adfar`x'.dta", replace
}
	
	
cd "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief"
use "Y:\Enterprise\Private\Projects\MScProjects\201920\Merle Schlief\cohortH03_2016_003_msc_inctraj_merle_v02.dta", clear



 forvalues x=90/97 {

	merge m:1 LopnrMor using mor`x', keepusing(Morfaminc`x' Morfaminc5_`x' Morfaminc10_`x') 
		drop if _m==2
		drop _merge
	merge m:1 LopnrFar using far`x', keepusing(Farfaminc`x' Farfaminc5_`x' Farfaminc10_`x')  
		drop if _m==2
		drop _merge
	merge m:1 LopnrAdMor using admor`x', keepusing(AdMorfaminc`x' AdMorfaminc5_`x' AdMorfaminc10_`x') 
		drop if _m==2
		drop _merge
	merge m:1 LopnrAdFar using adfar`x', keepusing(AdFarfaminc`x' AdFarfaminc5_`x' AdFarfaminc10_`x') 
		drop if _m==2
		drop _merge
		}
		
*Annual Family Income 
	
 forval x=90/97 {
	gen faminc`x'=.
	replace faminc`x'=AdMorfaminc`x'+ AdFarfaminc`x'
	replace faminc`x'=AdMorfaminc`x' if faminc`x'==.
	replace faminc`x'=AdFarfaminc`x' if faminc`x'==.
	
	replace faminc`x'=Morfaminc`x' + Farfaminc`x' if faminc`x'==.
	replace faminc`x'=Morfaminc`x' if faminc`x'==.
	replace faminc`x'=Farfaminc`x' if faminc`x'==.
	}
	
*Annual family income quintile
 forval x=90/97  {
	gen faminc5_`x'=.
	replace faminc5_`x'=AdMorfaminc5_`x' if (AdMorfaminc5_`x' >= AdFarfaminc5_`x')
	replace faminc5_`x'=AdFarfaminc5_`x' if (AdFarfaminc5_`x' >= AdMorfaminc5_`x')
	replace faminc5_`x'=AdMorfaminc5_`x' if faminc5_`x'==.
	replace faminc5_`x'=AdFarfaminc5_`x' if faminc5_`x'==.
	
	replace faminc5_`x'=Morfaminc5_`x' if (Morfaminc5_`x' >= Farfaminc5_`x') & faminc5_`x'==.
	replace faminc5_`x'=Farfaminc5_`x' if (Farfaminc5_`x' >= Morfaminc5_`x') & faminc5_`x'==.
	replace faminc5_`x'=Morfaminc5_`x' if faminc5_`x'==.
	replace faminc5_`x'=Farfaminc5_`x' if faminc5_`x'==.
}


*annual family income decile
 forval x=90/97  {
	gen faminc10_`x'=.
	replace faminc10_`x'=AdMorfaminc10_`x' if (AdMorfaminc10_`x' >= AdFarfaminc10_`x')
	replace faminc10_`x'=AdFarfaminc10_`x' if (AdFarfaminc10_`x' >= AdMorfaminc10_`x')
	replace faminc10_`x'=AdMorfaminc10_`x' if faminc10_`x'==.
	replace faminc10_`x'=AdFarfaminc10_`x' if faminc10_`x'==.
	
	replace faminc10_`x'=Morfaminc10_`x' if (Morfaminc10_`x' >= Farfaminc10_`x') & faminc10_`x'==.
	replace faminc10_`x'=Farfaminc10_`x' if (Farfaminc10_`x' >= Morfaminc10_`x') & faminc10_`x'==.
	replace faminc10_`x'=Morfaminc10_`x' if faminc10_`x'==.
	replace faminc10_`x'=Farfaminc10_`x' if faminc10_`x'==.
}
	
drop Morfaminc90-AdFarfaminc10_97

save, replace

	
}


/////////////////////////////////////////////////////////////////////////////////
**# 02 TRAJECTORY MODELLING 
/////////////////////////////////////////////////////////////////////////////////
{
cd "P:\0PS_Kirkbride-Merle Schlief\Data"
log using "P:\0PS_Kirkbride-Merle Schlief\Logs\traj.smcl", replace

* Install stata plugin from https://www.andrew.cmu.edu/user/bjones/index.htm
net from http://www.andrew.cmu.edu/user/bjones/traj
net install traj, force


	use analytic, clear
	
*create a time variable 
forval v=0/13 { 
	gen t_`v' = `v'
}

forval v=0/13 { 
	gen consinc_`v' = .
}

forval x=1990/2012 {
	replace consinc_0 = highest_consinc10_`x' if birthyear==`x' & consinc_0==.
	replace consinc_1 = highest_consinc10_`x' if (birthyear==[`x'-1]) & consinc_1==.
	replace consinc_2 = highest_consinc10_`x' if (birthyear==[`x'-2]) & consinc_2==.
	replace consinc_3 = highest_consinc10_`x' if (birthyear==[`x'-3]) & consinc_3==.
	replace consinc_4 = highest_consinc10_`x' if (birthyear==[`x'-4]) & consinc_4==.
	replace consinc_5 = highest_consinc10_`x' if (birthyear==[`x'-5]) & consinc_5==.
	replace consinc_6 = highest_consinc10_`x' if (birthyear==[`x'-6]) & consinc_6==.
	replace consinc_7 = highest_consinc10_`x' if (birthyear==[`x'-7]) & consinc_7==.
	replace consinc_8 = highest_consinc10_`x' if (birthyear==[`x'-8]) & consinc_8==.
	replace consinc_9 = highest_consinc10_`x' if (birthyear==[`x'-9]) & consinc_9==.
	replace consinc_10 = highest_consinc10_`x' if (birthyear==[`x'-10]) & consinc_10==.
	replace consinc_11 = highest_consinc10_`x' if (birthyear==[`x'-11]) & consinc_11==.
	replace consinc_12 = highest_consinc10_`x' if (birthyear==[`x'-12]) & consinc_12==.
	replace consinc_13 = highest_consinc10_`x' if (birthyear==[`x'-13]) & consinc_13==.
}


egen consinc_miss = rowmiss (consinc_0 consinc_1 consinc_2 consinc_3 consinc_4 consinc_5 consinc_6 consinc_7 consinc_8 consinc_9 consinc_10 consinc_11 consinc_12 consinc_13 )
	tab consinc_miss, m
	*we included people who had income measured at birth and at least one more time in childhood (so they need 2 points)
	drop consinc_miss


*Create a minimal dataset (dropping any extra variables) to run trajectory models
 keep lopnr consinc_* t_*
	save traj, replace
 	

// Step 1 - Decide which probability distribution best suits the distribution of dependent variable (income decile)
hist consinc_0
hist consinc_birth
	*determined that censored normal (cnorm) was most suitable for this analysis
	
// Step 2 - Decide how many groups has the BIC closest to 0 with at least 5% of the sample in each group 
*test k=1 through k=6 
traj, var(consinc_*) inde(t_*) model(cnorm) order(1) min(0) max(10)
traj, var(consinc_*) inde(t_*) model(cnorm) order(1 1) min(0) max(10)
traj, var(consinc_*) inde(t_*) model(cnorm) order(1 1 1) min(0) max(10)
traj, var(consinc_*) inde(t_*) model(cnorm) order(1 1 1 1) min(0) max(10)
traj, var(consinc_*) inde(t_*) model(cnorm) order(1 1 1 1 1) min(0) max(10)
traj, var(consinc_*) inde(t_*) model(cnorm) order(1 1 1 1 1 1) min(0) max(10)
	*6 group model selected
	
// Step 3 - Using the 6-group model and exploring optimal shape of the trajectories (linear, quadratic, cubic)
*run additional post-estimation commands on the 6-group linear model to get additional APPA and OCC statistics 
* NOTE - has to be run directly after the traj model runs 

* 6-group linear 
*traj, var(consinc_*) inde(t_*) model(cnorm) order(1 1 1 1 1 1) min(0) max(10)
program summary_table_procTraj6ConsInc1
 preserve
 *now lets look at the average posterior probability
 gen Mp = 0
 foreach i of varlist _traj_ProbG* {
 replace Mp = `i' if `i' > Mp
 }
 sort _traj_Group
 *and the odds of correct classification
 by _traj_Group: gen countG = _N
 by _traj_Group: egen groupAPP = mean(Mp)
 by _traj_Group: gen counter = _n
 gen n = groupAPP/(1 - groupAPP)
 gen p = countG/ _N
 gen d = p/(1-p)
 gen occ = n/d
 *Estimated proportion for each group
 scalar c = 0
 gen TotProb = 0
 foreach i of varlist _traj_ProbG* {
 scalar c = c + 1
 quietly summarize `i'
 replace TotProb = r(sum)/ _N if _traj_Group == c
 }
 gen d_pp = TotProb/(1 - TotProb)
 gen occ_pp = n/d_pp
 *This displays the group number [_traj_~p],
 *the count per group (based on the max post prob), [countG]
 *the average posterior probability for each group, [groupAPP]
 *the odds of correct classification (based on the max post prob group assignment), [occ]
 *the odds of correct classification (based on the weighted post. prob), [occ_pp]
 *and the observed probability of groups versus the probability [p]
 *based on the posterior probabilities [TotProb]
 list _traj_Group countG groupAPP occ occ_pp p TotProb if counter == 1
 restore
end
summary_table_procTraj6ConsInc1

trajplot 
graph save "Graph" "P:\0PS_Kirkbride-Merle Schlief\Output\linear6.gph", replace


* 6-group quadratic 
traj, var(consinc_*) inde(t_*) model(cnorm) order(2 2 2 2 2 2) min(0) max(10)

program summary_table_procTraj6Consinc4
 preserve
 *now lets look at the average posterior probability
 gen Mp = 0
 foreach i of varlist _traj_ProbG* {
 replace Mp = `i' if `i' > Mp
 }
 sort _traj_Group
 *and the odds of correct classification
 by _traj_Group: gen countG = _N
 by _traj_Group: egen groupAPP = mean(Mp)
 by _traj_Group: gen counter = _n
 gen n = groupAPP/(1 - groupAPP)
 gen p = countG/ _N
 gen d = p/(1-p)
 gen occ = n/d
 *Estimated proportion for each group
 scalar c = 0
 gen TotProb = 0
 foreach i of varlist _traj_ProbG* {
 scalar c = c + 1
 quietly summarize `i'
 replace TotProb = r(sum)/ _N if _traj_Group == c
 }
 gen d_pp = TotProb/(1 - TotProb)
 gen occ_pp = n/d_pp
 list _traj_Group countG groupAPP occ occ_pp p TotProb if counter == 1
 restore
end
summary_table_procTraj6Consinc4

trajplot 
graph save "Graph" "P:\0PS_Kirkbride-Merle Schlief\Output\quadradic6.gph", replace


* 6-group cubic 
traj, var(consinc_*) inde(t_*) model(cnorm) order(3 3 3 3 3 3) min(0) max(10)

program summary_table_procTraj6ConsInc3
 preserve
 *now lets look at the average posterior probability
 gen Mp = 0
 foreach i of varlist _traj_ProbG* {
 replace Mp = `i' if `i' > Mp
 }
 sort _traj_Group
 *and the odds of correct classification
 by _traj_Group: gen countG = _N
 by _traj_Group: egen groupAPP = mean(Mp)
 by _traj_Group: gen counter = _n
 gen n = groupAPP/(1 - groupAPP)
 gen p = countG/ _N
 gen d = p/(1-p)
 gen occ = n/d
 scalar c = 0
 gen TotProb = 0
 foreach i of varlist _traj_ProbG* {
 scalar c = c + 1
 quietly summarize `i'
 replace TotProb = r(sum)/ _N if _traj_Group == c
 }
 gen d_pp = TotProb/(1 - TotProb)
 gen occ_pp = n/d_pp
 list _traj_Group countG groupAPP occ occ_pp p TotProb if counter == 1
 restore
end
summary_table_procTraj6ConsInc3

trajplot 
graph save "Graph" "P:\0PS_Kirkbride-Merle Schlief\Output\consinc - cubic6_v2.gph", replace	
	

// Step 4 - compare the fit statistics for each and determine a final combined model using the best fitting polynomials for each of the trajectory groups (APPA >0.70 and OCC >5.0 and visual inspection 

* Once you have picked a model that has the best fit, run it again to assign traj membership using the below programme
	*programme function that will generate additional summary stats I made a function to print out summary stats
	*must be run directly after your traj command as it uses post-estimation variables

*Final 6-group model 
traj, var(consinc_*) inde(t_*) model(cnorm) order(3 2 2 3 1 3) min(0) max(10)

program summary_table_procTraj
    preserve
    *now lets look at the average posterior probability
	gen Mp = 0
	foreach i of varlist _traj_ProbG* {
	    replace Mp = `i' if `i' > Mp 
	}
    sort _traj_Group
    *and the odds of correct classification
    by _traj_Group: gen countG = _N
    by _traj_Group: egen groupAPP = mean(Mp)
    by _traj_Group: gen counter = _n
    gen n = groupAPP/(1 - groupAPP)
    gen p = countG/ _N
    gen d = p/(1-p)
    gen occ = n/d
    *Estimated proportion for each group
    scalar c = 0
    gen TotProb = 0
    foreach i of varlist _traj_ProbG* {
       scalar c = c + 1
       quietly summarize `i'
       replace TotProb = r(sum)/ _N if _traj_Group == c 
    }
    *This displays the group number, the count per group, the average posterior probability for each group,
    *the odds of correct classification, and the observed probability of groups versus the probability 
    *based on the posterior probabilities
    list _traj_Group countG groupAPP occ p TotProb if counter == 1
	restore
end
summary_table_procTraj

rename _traj_Group traj_group

save traj, replace

	
// Step 6 - Merge traj group with analytic file 

use analytic, clear
	merge 1:1 lopnr using traj 
	drop _merge 
	
log close 
	
	
}

/////////////////////////////////////////////////////////////////////////////////
**# 03	DESCRIPTIVE STATISTICS
/////////////////////////////////////////////////////////////////////////////////
{
log using "P:\0PS_Kirkbride-Merle Schlief\Logs\descriptives", replace
cd "P:\0PS_Kirkbride-Merle Schlief\Data"
use analytic, clear 
	
/* Key variables
smi naff bp_psyc bp_nopsyc 
traj_group mig sex cohort moves2 popdens3 dep5_birth  mor_age far_age parent_smi inc_birth  */

// DESCRIPTIVES
*Included these to use in the paragraph on sample characterstics

tab mig, m

tab smi, m 
	tab naff 
	tab bp_psyc
	tab bp_nopsyc

tab mig smi, row 
	tab mig naff, row 
	tab mig bp_psyc, row 
	tab mig bp_nopsyc, row

tab sex, m 
tab cohort, m 
tab moves2, m 
tab mor_age, m 	
tab far_age, m
tab parent_smi, m
tab popdens3, m 
tab dep5_birth, m
	
sum inc_birth //actual income in 1000 sek 
	bysort traj_group: sum inc_birth
tab consinc_0, m //decile at birth

*How many with complete income information (for each year) 
egen inc_miss = rowmiss (consinc_0 consinc_1 consinc_2 consinc_3 consinc_4 consinc_5 consinc_6 consinc_7 consinc_8 consinc_9 consinc_10 consinc_11 consinc_12 consinc_13 )
	tab inc_miss, m


	
// Table 1
tab traj_group, m

tab smi traj_group, col chi m

tab naff traj_group , col chi m
tab bp_psyc traj_group, col chi m
tab bp_nopsyc traj_group , col chi m

tab sex traj_group , col chi m
tab cohort traj_group , col chi m
tab mig traj_group , col chi m

tab parent_smi traj_group , col chi m

tab mor_age traj_group, col chi m
tab far_age traj_group, col chi m

tab moves2 traj_group , col chi m
tab dep5_birth traj_group , col chi m
tab popdens3 traj_group , col chi m


bysort traj_group: sum inc_birth traj_group

bysort traj_group: sum consinc_0 traj_group


//SMI by migrant status //
tab smi mig , m row col chi
tab naff mig, m  row col chi
tab bp_psyc mig, m row col chi
tab bp_nopsyc mig, m row col chi

log close
	
	
}


/////////////////////////////////////////////////////////////////////////////////
**# 04	LOGISTIC REGRESSION
/////////////////////////////////////////////////////////////////////////////////

{
log using "P:\0PS_Kirkbride-Merle Schlief\Logs\regressions", replace
* NOTE - mig is coded that mig=2 is COM and mig=3 is SB to SB parents. Set mig=3 as reference
* NOTE - traj_group=6 is the group with the highest income at all time points, used as reference category 
cd "P:\0PS_Kirkbride-Merle Schlief\Data"

use analytic, clear 


// Step 1 - test for interaction by migrant status, using lincom
	*stratified, if evidence of interaction found 

*Interaction - SMI	
logit smi ib6.traj_group ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 
	est store A
logit smi ib6.traj_group##ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or
	est store B
	lrtest A B
*Interaction - NAFF
logit naff ib6.traj_group ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or
	est store A
logit naff ib6.traj_group##ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or
	est store B
	lrtest A B
*Interaction - BIPOLAR WITH PSYCHOSIS
logit bp_psyc ib6.traj_group ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or
	est store A
logit bp_psyc ib6.traj_group##ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or
	est store B
	lrtest A B
*Interaction - BIPOLAR WITHOUT PSYCHOSIS 
logit bp_nopsyc ib6.traj_group ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or
	est store A
logit bp_nopsyc ib6.traj_group##ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or
	est store B
	lrtest A B

// Step 2 - found evidence of interactions, so report stratified results. 
*this can be done by fitting stratum-specific regressions or the lincom post-estimation command. 
*the unadjusted estimates from these two approaches are identical, but there are very small differences in the adjusted results. Discussed with statistician, and while both approaches are statistically appropriate, the stratified regressions are more flexible and simipler to understand and explain (although looses some statistical efficiency). Have produced both estimates, but will report those generated from stratified regressions. 

//stratified regressions
*Unadjusted
logit smi ib6.traj_group##ib3.mig, or
logit naff ib6.traj_group##ib3.mig, or 
logit bp_psyc ib6.traj_group##ib3.mig, or 
logit bp_nopsyc ib6.traj_group##ib3.mig, or 
 

*Adjusted
logit smi ib6.traj_group##ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 
logit naff ib6.traj_group##ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 
logit bp_psyc ib6.traj_group##ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 
logit bp_nopsyc ib6.traj_group##ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 


//Regressions with lincom post-estimation for stratum-specific results
*smi - Unadjusted  
logit smi ib6.traj_group##ib3.mig, or 
	lincom 1.traj_group + 2.mig#1.traj_group , or 
	lincom 2.traj_group + 2.mig#2.traj_group , or 
	lincom 3.traj_group + 2.mig#3.traj_group , or 
	lincom 4.traj_group + 2.mig#4.traj_group , or 
	lincom 5.traj_group + 2.mig#5.traj_group , or 
	lincom 6.traj_group + 2.mig#6.traj_group , or 
*can just extract the regression coefficients from the main output, as this is the rate by trajectory group in the reference category mig=3, but thought it helpful to have it presented in the same way as the mig=2 group
	lincom 1.traj_group + 3.mig#1.traj_group , or 
	lincom 2.traj_group + 3.mig#2.traj_group , or 
	lincom 3.traj_group + 3.mig#3.traj_group , or 
	lincom 4.traj_group + 3.mig#4.traj_group , or 
	lincom 5.traj_group + 3.mig#5.traj_group , or 
	lincom 6.traj_group + 3.mig#6.traj_group , or

*smi - Adjusted
 logit smi ib6.traj_group##ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 
	lincom 1.traj_group + 2.mig#1.traj_group , or 
	lincom 2.traj_group + 2.mig#2.traj_group , or 
	lincom 3.traj_group + 2.mig#3.traj_group , or 
	lincom 4.traj_group + 2.mig#4.traj_group , or 
	lincom 5.traj_group + 2.mig#5.traj_group , or 
	lincom 6.traj_group + 2.mig#6.traj_group , or 

	lincom 1.traj_group + 3.mig#1.traj_group , or 
	lincom 2.traj_group + 3.mig#2.traj_group , or 
	lincom 3.traj_group + 3.mig#3.traj_group , or 
	lincom 4.traj_group + 3.mig#4.traj_group , or 
	lincom 5.traj_group + 3.mig#5.traj_group , or 
	lincom 6.traj_group + 3.mig#6.traj_group , or

*NAFF - Unadjusted 
logit naff ib6.traj_group##ib3.mig, or 
	lincom 1.traj_group + 2.mig#1.traj_group , or 
	lincom 2.traj_group + 2.mig#2.traj_group , or 
	lincom 3.traj_group + 2.mig#3.traj_group , or 
	lincom 4.traj_group + 2.mig#4.traj_group , or 
	lincom 5.traj_group + 2.mig#5.traj_group , or 
	lincom 6.traj_group + 2.mig#6.traj_group , or 

	lincom 1.traj_group + 3.mig#1.traj_group , or 
	lincom 2.traj_group + 3.mig#2.traj_group , or 
	lincom 3.traj_group + 3.mig#3.traj_group , or 
	lincom 4.traj_group + 3.mig#4.traj_group , or 
	lincom 5.traj_group + 3.mig#5.traj_group , or 
	lincom 6.traj_group + 3.mig#6.traj_group , or
		
*Naff - Adjusted
logit naff ib6.traj_group##ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 
	lincom 1.traj_group + 2.mig#1.traj_group , or 
	lincom 2.traj_group + 2.mig#2.traj_group , or 
	lincom 3.traj_group + 2.mig#3.traj_group , or 
	lincom 4.traj_group + 2.mig#4.traj_group , or 
	lincom 5.traj_group + 2.mig#5.traj_group , or 
	lincom 6.traj_group + 2.mig#6.traj_group , or 

	lincom 1.traj_group + 3.mig#1.traj_group , or 
	lincom 2.traj_group + 3.mig#2.traj_group , or 
	lincom 3.traj_group + 3.mig#3.traj_group , or 
	lincom 4.traj_group + 3.mig#4.traj_group , or 
	lincom 5.traj_group + 3.mig#5.traj_group , or 
	lincom 6.traj_group + 3.mig#6.traj_group , or

*BP_psyc - Unadjusted 
logit bp_psyc ib6.traj_group##ib3.mig, or 
	lincom 1.traj_group + 2.mig#1.traj_group , or 
	lincom 2.traj_group + 2.mig#2.traj_group , or 
	lincom 3.traj_group + 2.mig#3.traj_group , or 
	lincom 4.traj_group + 2.mig#4.traj_group , or 
	lincom 5.traj_group + 2.mig#5.traj_group , or 
	lincom 6.traj_group + 2.mig#6.traj_group , or 

	lincom 1.traj_group + 3.mig#1.traj_group , or 
	lincom 2.traj_group + 3.mig#2.traj_group , or 
	lincom 3.traj_group + 3.mig#3.traj_group , or 
	lincom 4.traj_group + 3.mig#4.traj_group , or 
	lincom 5.traj_group + 3.mig#5.traj_group , or 
	lincom 6.traj_group + 3.mig#6.traj_group , or

*bp_psyc - Adjusted
 logit bp_psyc ib6.traj_group##ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 
	lincom 1.traj_group + 2.mig#1.traj_group , or 
	lincom 2.traj_group + 2.mig#2.traj_group , or 
	lincom 3.traj_group + 2.mig#3.traj_group , or 
	lincom 4.traj_group + 2.mig#4.traj_group , or 
	lincom 5.traj_group + 2.mig#5.traj_group , or 
	lincom 6.traj_group + 2.mig#6.traj_group , or 

	lincom 1.traj_group + 3.mig#1.traj_group , or 
	lincom 2.traj_group + 3.mig#2.traj_group , or 
	lincom 3.traj_group + 3.mig#3.traj_group , or 
	lincom 4.traj_group + 3.mig#4.traj_group , or 
	lincom 5.traj_group + 3.mig#5.traj_group , or 
	lincom 6.traj_group + 3.mig#6.traj_group , or

*bp_nopsyc - Unadjusted 
logit bp_nopsyc ib6.traj_group##ib3.mig, or 
	lincom 1.traj_group + 2.mig#1.traj_group , or 
	lincom 2.traj_group + 2.mig#2.traj_group , or 
	lincom 3.traj_group + 2.mig#3.traj_group , or 
	lincom 4.traj_group + 2.mig#4.traj_group , or 
	lincom 5.traj_group + 2.mig#5.traj_group , or 
	lincom 6.traj_group + 2.mig#6.traj_group , or 

	lincom 1.traj_group + 3.mig#1.traj_group , or 
	lincom 2.traj_group + 3.mig#2.traj_group , or 
	lincom 3.traj_group + 3.mig#3.traj_group , or 
	lincom 4.traj_group + 3.mig#4.traj_group , or 
	lincom 5.traj_group + 3.mig#5.traj_group , or 
	lincom 6.traj_group + 3.mig#6.traj_group , or

*bp_nopsyc - Adjusted 
logit bp_nopsyc ib6.traj_group##ib3.mig sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 
	lincom 1.traj_group + 2.mig#1.traj_group , or 
	lincom 2.traj_group + 2.mig#2.traj_group , or 
	lincom 3.traj_group + 2.mig#3.traj_group , or 
	lincom 4.traj_group + 2.mig#4.traj_group , or 
	lincom 5.traj_group + 2.mig#5.traj_group , or 
	lincom 6.traj_group + 2.mig#6.traj_group , or 

	lincom 1.traj_group + 3.mig#1.traj_group , or 
	lincom 2.traj_group + 3.mig#2.traj_group , or 
	lincom 3.traj_group + 3.mig#3.traj_group , or 
	lincom 4.traj_group + 3.mig#4.traj_group , or 
	lincom 5.traj_group + 3.mig#5.traj_group , or 
	lincom 6.traj_group + 3.mig#6.traj_group , or


log close

}


/////////////////////////////////////////////////////////////////////////////////
**# 05	MISSINGNESS & SENSITIVITY 
/////////////////////////////////////////////////////////////////////////////////
{
log using "P:\0PS_Kirkbride-Merle Schlief\Logs\missingness", replace
cd "P:\0PS_Kirkbride-Merle Schlief\Data"
use analytic, clear

gen missing=0
	replace missing=1 if traj_group==.
	replace missing=1 if sams_birth==.
	replace missing = 1 if mig==4
	replace missing =1 if popdens3==.
	replace missing =1 if dep5_birth==.
tab missing, m

tab missing kon, row m 
tab missing cohort, row m 
tab missing mig, row m 
tab missing mor_age, row m 
tab missing far_age, row m 
tab missing parent_smi, row m 
bysort missing: count if sams_birth==. 
tab missing dep5_birth, row m 
tab missing popdens3, row m 
tab missing traj_group, row m 

// Regression predicting missingness 
logit missing ib2.kon, or
logit missing i.cohort, or
logit missing ib3.mig, or
logit missing i.mor_age, or 
logit missing i.far_age, or 
logit missing ib5.dep5_birth, or 
logit missing i.popdens3, or 
logit missing ib6.traj_group, or 

//Complete case - dropping anyone missing mother or father age 
drop if mor_age==0 | far_age==0
*Unadjusted
bysort mig: logit smi ib6.traj_group, or
bysort mig: logit naff ib6.traj_group, or 
bysort mig: logit bp_psyc ib6.traj_group, or 
bysort mig: logit bp_nopsyc ib6.traj_group, or 
 
*Adjusted
bysort mig: logit smi ib6.traj_group sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 
bysort mig: logit naff ib6.traj_group sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 
bysort mig: logit bp_psyc ib6.traj_group sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 
bysort mig: logit bp_nopsyc ib6.traj_group sex cohort parent_smi  mor_age far_age moves2 dep5_birth popdens_birth, or 


log close
	
}































	
	
	