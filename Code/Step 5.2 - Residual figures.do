
/*************************************************************************
 *************************************************************************
	        Difference in difference estimations
			 
1) Created by: Daniel Márquez
			   HBS
			   dmarquezm20@gmail.com		
				
2) Date: July 2024

3) Objective: 	Generate figures for the average outcome for each group (treated - control)

4) Output:	- 2 figures for each outcome in the case of general estimates
*************************************************************************
*************************************************************************/


****************************************************************************
*Globals
****************************************************************************
timer clear
global FOLDER_PROYECTO "//wmedesrv/gamma/Christian Posso/_banrep_research/proyectos/PhysiciansPosgraduates"
global logs "${FOLDER_PROYECTO}/Logs"
global data "${FOLDER_PROYECTO}/Data"
global output "${FOLDER_PROYECTO}/Output"

cap log close
log using "${logs}\step3_event_study.smcl", replace


* Controls
global controls year_grado
global cohorts 1995 //2000 2005 2010 
global treatment_groups treated_1a treated_1b treated_1c treated_1d 						///
                        treated_2a treated_2b treated_2c treated_2d
* Outcomes
global outcomes service_mental service_mental2 service_mental3 cardio_risk laboral			///
				pila_independientes p_cotizaciones pila_salario_r sal_dias_cot
					
					
****************************************************************************
**#						1. Merge data and set-up
****************************************************************************

* RIPS
use "${data}\Individual_balanced_all_RIPS", clear
rename year_RIPS fecha_pila

* PILA
merge 1:1 personabasicaid fecha_pila using "${data}\Individual_balanced_all_PILA"
drop if _merge == 2 // Estos son los del 2008
keep if inrange(posgrado_start, 2011, 2017) | mi(posgrado_start)

* ReTHUS
merge m:1 personabasicaid using "${data}\master_rethus.dta", keep(3) nogen 
drop if mi(fechapregrado)
gen year_grado = yofd(fechapregrado)

rename rethus_sexo rethus_sexo_temp
gen rethus_sexo = 0 
replace rethus_sexo = 1 if rethus_sexo_temp == "MASCULINO"
replace rethus_sexo = 2 if rethus_sexo_temp == "FEMENINO"
drop rethus_sexo_temp
drop if rethus_sexo == 0

* Final outcomes
gen service_mental3 = (service_mental == 1 & service_mental2 == 0)
gen cardio_risk     = (stroke == 1 | infarct == 1 | cardiovascular == 1)
gen laboral         = (diag_laboral == 1 | estres_laboral == 1 | ///
                       accidente_laboral == 1 | enfermedad_laboral == 1	| ///
					   acc_enf_laboral == 1)


****************************************************************************
**#						2. Estimation features
****************************************************************************

* Distance to enrollment
gen dist = fecha_pila - posgrado_start

* Label globals
global labels ""Mental health diagnosis" "Stress, anxiety or depression" "Mental health diagnosis (excluding)" "Cardio risk" "Laboral disease" "Independent" "Multiple jobs" "Real monthly earnings (total - 2018 COP)" "Monthly worked days""

local o = 1
foreach label in $labels {		
	global o`o' = "`label'"	
	local o = `o' + 1	
}


****************************************************************************
**# 				3.  Figures
****************************************************************************

preserve

* Average calculations
global residuals ""
foreach outcome in $outcomes {	
	
	reghdfe `outcome', abs(personabasicaid fecha_pila) resid(res_`outcome')
	g r_`outcome' = _b[_cons] + res_`outcome'
	drop res_`outcome'
	
	global residuals "${residuals} r_`outcome'"
	
}

collapse (mean) $outcomes $residuals , by(dist treated_1a)
keep if dist >= -2 & dist <= 5

* Graphs
local i = 1
foreach outcome in $outcomes {
	
	* Format local
	qui sum `outcome'
	local mean = r(mean)
	
	if r(mean)>1 & r(mean)>1000  {			
		local format = "%12.0fc"			
	}
	if r(mean)>1 & r(mean)<=1000 {		
		local format = "%5.0f"			
	}		
	if r(mean)<=1 				 {		
		local format = "%5.2f"			
	}

	twoway 	(line `outcome' date_to_draw if (treated == 1), color(gs5))					///
			(line `outcome' date_to_draw if (treated == 0), color(gs10)),				///
			xlabel(-2(1)5, nogrid labsize(vsmall))	 									///
			ylabel(#6, labsize(small) angle(360) format(`format'))						///
			xline(-1,lwidth(thin) lcolor(navy*0.5) lpattern(-))							///
			xtitle("Time since enrollment (months)", size(small)) 						///
			ytitle("${o`i'}", size(small))												///
			legend(order(1 "Treated Average" 2 "Control Average") position(6) col(2))	///
			scheme(s1color) graphregion(fcolor(white)) plotregion(fcolor(white))
			
	graph export "${output}\Figures\Averages\Ave_`outcome'.png", replace
			
	twoway 	(line r_`outcome' date_to_draw if (treated == 1), color(gs5))				///
			(line r_`outcome' date_to_draw if (treated == 0), color(gs10)),				///
			xlabel(-2(1)5, nogrid labsize(vsmall))	 									///
			ylabel(#6, labsize(small) angle(360) format(`format'))						///
			xline(-1,lwidth(thin) lcolor(navy*0.5) lpattern(-))							///
			xtitle("Time since enrollment (months)", size(small)) 						///
			ytitle("${o`i'}", size(small))												///
			legend(order(1 "Treated Average" 2 "Control Average") position(6) col(2))	///
			scheme(s1color) graphregion(fcolor(white)) plotregion(fcolor(white))
			
	graph export "${output}\Figures\Averages\Res_`outcome'.png", replace
			
	local i = `i' + 1
			
} 

restore

