/*************************************************************************
 *************************************************************************			       	
					Event study plotting
			 
1) Created by: Daniel Márquez
			   Harvard Business School
			   dmarquezm20@gmail.com	
				
2) Date: February 2024

3) Objective: Estimate the event studies for PILA and RIPS.
			  This do file can be ran on any computer with access to the CS results.

4) Output:	- One graph per variable in globals
*************************************************************************
*************************************************************************/	


****************************************************************************
*Required packages (uncomment if running for the first time)
****************************************************************************
*ssc install schemeplot, replace

****************************************************************************
*Globals
****************************************************************************
clear all
set more off

global FOLDER_PROYECTO "//wmedesrv/gamma/Christian Posso/_banrep_research/proyectos/PhysiciansPosgraduates"
global tables	"${FOLDER_PROYECTO}\Output\Tables"
global figures	"${FOLDER_PROYECTO}\Output\Figures"
global logs "${FOLDER_PROYECTO}/Logs"

* Plot parameters
set scheme white_tableau
global x_min_lim -8
global x_max_lim 11

cap log close
log using "${logs}\step4_ES_figures.smcl", replace
****************************************************************************
**#						1. Processing
****************************************************************************

use "${tables}\CS_Results.dta", clear
drop if var == "Pre_avg" | var == "Post_avg"

gen 	dist 		= substr(var, 3, 1) if (strlen(var) == 3)
replace dist 		= substr(var, 3, 2) if (strlen(var) == 4)
destring dist, replace
replace dist 		= dist * (-1) if (substr(var, 1, 2) == "tm")
sort outcome dist
drop var

* Balance for variables with non-estimated periods
/*
preserve

	keep outcome
	duplicates drop
	expand 14
	
	bys outcome: g dist = _n - 5
	drop if dist == -1
	
	tempfile temp
	save `temp'
	
restore

merge m:1 dist outcome using `temp', nogen
replace coef = 0 if coef == . & dist < 0
replace ci_lower = 0 if ci_lower == . & dist < 0
replace ci_upper = 0 if ci_upper == . & dist < 0
*/

* Relevant periods


drop if dist < $x_min_lim
drop if dist > $x_max_lim

* Controls label
tostring controls, replace
replace controls = "Controls" if controls == "1"
replace controls = "NoControls" if controls == "0"

* Temporal restrictions
keep if wboot == 0
*drop if estimation == "NT"

bys outcome cohort: ereplace mean = min(mean)
drop wboot
		
****************************************************************************
**#						2. Figures
****************************************************************************

tempfile tempfile
save `tempfile'

levelsof estimation, 	  local(estimations)
levelsof treatment_group, local(treatment_groups)
levelsof cohort, 		  local(cohors)
levelsof controls, 		  local(controls)
levelsof outcome, 		  local(outcomes)

*Tester
*local outcomes = "service_mental"
foreach treatment_group in `treatment_groups' {
foreach estimation in `estimations' {
foreach cohort in `cohors' {
foreach control in `controls' {
	
	use `tempfile', clear	
	keep if estimation == "`estimation'" & cohort == `cohort' & ///
	        controls == "`control'" & treatment_group == "`treatment_group'"

	foreach outcome in `outcomes' {
	
		qui sum mean if (outcome =="`outcome'")

		if r(mean)>1 & r(mean) >= 1000  {			
			local format = "%16.0fc"			
		}
		if r(mean)>1 & r(mean) < 1000 {		
			local format = "%5.0f"			
		}		
		if r(mean)<=1 				 {		
			local format = "%5.4f"			
		}		
		
		local mean: dis `format' r(mean)
		local mean: dis strtrim("`mean'")
		
		qui sum n if (outcome == "`outcome'")
		local samp: dis r(mean)
			
		twoway 	(rspike ci_lower ci_upper dist 	if (outcome == "`outcome'"), lcolor(gs11) color(gs11) lp(solid))					///
				(scatter coef dist 				if (outcome == "`outcome'"), mlc(gs11)    mfc(gs11)   m(O) msize(medsmall)),		///
				xlabel($x_min_lim(1)$x_max_lim, nogrid labsize(small))	 																			///
				ylabel(#10, angle(h) format(`format') labsize(small))																///
				xline(-1, lcolor(gs10))																								///
				yline(0, lcolor(gs10))																								///
				xtitle("Years from Enrollment")																						///
				ytitle("Point Estimate")																							///
				legend(order(2 "Coefficient" 1 "IC at 95%") position(6) col(4))														///
				graphregion(fcolor(white)) note("Control mean: `mean'" "Sample: `samp'")
		graph export "${figures}\\`control'\ES_`treatment_group'_`outcome'_`cohort'_`estimation'.png", replace
		
	}
	
}
}
}
}
log close