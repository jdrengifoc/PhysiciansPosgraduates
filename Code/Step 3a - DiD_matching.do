/*************************************************************************
 *************************************************************************			       	
					Event study estimation
			 
1) Created by: Juan David Rengifo Castro
               Banco de la República
               jdrengifoc@eafit.edu.co
               
               Daniel Márquez
			         Harvard Business School
			         dmarquezm20@gmail.com
				
2) Date: February 2024

3) Objective: Estimate the event studies for PILA and RIPS.
			  This do file can only be ran at BanRep's servers.

4) Output:	- ES_results_S.dta
*************************************************************************
*************************************************************************/	


****************************************************************************
*Required packages (uncomment if running for the first time)
****************************************************************************
*ssc install regsave, replace
*ssc install reghdfe, replace

****************************************************************************
*Globals
****************************************************************************
timer clear
global FOLDER_PROYECTO "//wmedesrv/gamma/Christian Posso/_banrep_research/proyectos/PhysiciansPosgraduates"
global logs "${FOLDER_PROYECTO}/Logs"
global data "${FOLDER_PROYECTO}/Data"
global output "${FOLDER_PROYECTO}/Output"
global output_matching "${output}\Tables\CS_results_matching"

cap log close
log using "${logs}\step3a_DiD_matching.smcl", replace


* Controls
global controls year_grado
global cohorts 1995 //2000 2005 2010 
global treatment_groups treated_1a treated_1b treated_1c treated_1d ///
                        treated_2a treated_2b treated_2c treated_2d
* Outcomes
global outcomes service_mental service_mental2 service_mental3 cardio_risk laboral						///
				pila_independientes p_cotizaciones pila_salario_r sal_dias_cot
				
/*
global outcomes service_mental service_mental2 service_mental3 diag_laboral estres_laboral				///
				accidente_laboral enfermedad_laboral acc_enf_laboral laboral							///
				pregnancy stroke infarct cardiovascular cardio_risk digestive 							///
				pila_independientes p_cotizaciones pila_salario_r pila_salario_r_max sal_dias_cot
*/
	
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
merge m:1 personabasicaid using "${data}\master_matching.dta", keep(3) nogen 
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

gen dist = fecha_pila - posgrado_start
*keep if (dist >= -2 & dist <= 5)

* Eventually treated dummy
gen eventually_treated = (posgrado_start != .)

* Never treated condition
replace posgrado_start = 0 	if	posgrado_start == .

****************************************************************************
**#						3. CS never treated only
****************************************************************************			
local replace replace

preserve

timer on 1
foreach treated in $treatment_groups {    
	
	keep if `treated' != .
	
	foreach cohort in $cohorts {
		
		keep if year_grado >= `cohort'
		
		foreach outcome in $outcomes {
						
			dis as err "Running event study for `outcome'"
			
			* Without controls
			qui sum `outcome' if mi(dist)
			local mean = r(mean)
			
			qui sum `outcome' if fecha_pila == 2015
			local obs  = r(N) 
			
			csdid2 		`outcome',					///
						i(personabasicaid) 			/// panel id variable
						t(fecha_pila) 				/// Time variable
						gvar(posgrado_start) 		/// Treatment time
						long2 						/// Calculate results relative to -1
						asinr 						/// Calculate pre-treatment results as in R
						method(drimp)				/// Use doubly robust improved method
						cluster(personabasicaid)				
			estat event, post						// Aggregate estimation like an event-study
			
			regsave using "${output_matching}", `replace' ci level(95) ///
					addlabel(treatment_group, `treated', outcome, `outcome', mean, `mean', n, `obs', wboot, 0, estimation, NT, cohort, `cohort', controls, 0)
			
			local replace append
			
			estat event, wboot post					// SE using WildBootstrap
			regsave using "${output_matching}", append ci rtable ///
					addlabel(treatment_group, `treated', outcome, `outcome', mean, `mean', n, `obs', wboot, 1, estimation, NT, cohort, `cohort', controls, 0)
					
			* With controls
			qui sum `outcome' if mi(dist)
			local mean = r(mean)
			
			qui sum `outcome' if fecha_pila == 2015
			local obs  = r(N) 
			
			csdid2 		`outcome' $controls , 		///
						i(personabasicaid) 			/// panel id variable
						t(fecha_pila) 				/// Time variable
						gvar(posgrado_start) 		/// Treatment time
						long2 						/// Calculate results relative to -1
						asinr 						/// Calculate pre-treatment results as in R
						method(drimp)				/// Use doubly robust improved method
						cluster(personabasicaid)				
			estat event, post						// Aggregate estimation like an event-study
			
			regsave using "${output_matching}", append ci level(95) ///
					addlabel(treatment_group, `treated', outcome, `outcome', mean, `mean', n, `obs', wboot, 0, estimation, NT, cohort, `cohort', controls, 1)
			
			estat event, wboot post					// SE using WildBootstrap
			regsave using "${output_matching}", append ci rtable ///
					addlabel(treatment_group, `treated', outcome, `outcome', mean, `mean', n, `obs', wboot, 1, estimation, NT, cohort, `cohort', controls, 1)
		
		}
		
	}

	cap restore
}
timer off 1

****************************************************************************
**#						4. CS with eventually treated
****************************************************************************
timer on 2
foreach treated in $treatment_groups {    
	
	keep if `treated' != .
	
	foreach cohort in $cohorts  {
			
		preserve	
		keep if year_grado >= `cohort'
		
		foreach outcome in $outcomes {
						
			dis as err "Running event study for `outcome'"
			
			* Without controls
			qui sum `outcome' if mi(dist)
			local mean = r(mean)
			
			qui sum `outcome' if fecha_pila == 2015
			local obs  = r(N) 
			
			csdid2 		`outcome',					///
						i(personabasicaid) 			/// panel id variable
						t(fecha_pila) 				/// Time variable
						gvar(posgrado_start) 		/// Treatment time
						long2 						/// Calculate results relative to -1
						asinr 						/// Calculate pre-treatment results as in R
						method(drimp)				/// Use doubly robust improved method
						notyet						/// Request using eventually treated
						cluster(personabasicaid)					
			estat event, post						// Aggregate estimation like an event-study
			
			regsave using "${output_matching}", append ci level(95) ///
					addlabel(treatment_group, `treated', outcome, `outcome', mean, `mean', n, `obs', wboot, 0, estimation, NET, cohort, `cohort', controls, 0)
			
			estat event, wboot post					// SE using WildBootstrap
			regsave using "${output_matching}", append ci rtable ///
					addlabel(treatment_group, `treated', outcome, `outcome', mean, `mean', n, `obs', wboot, 1, estimation, NET, cohort, `cohort', controls, 0)	

			* With controls
			qui sum `outcome' if mi(dist)
			local mean = r(mean)
			
			qui sum `outcome' if fecha_pila == 2015
			local obs  = r(N) 
			
			csdid2 		`outcome' $controls ,		///
						i(personabasicaid) 			/// panel id variable
						t(fecha_pila) 				/// Time variable
						gvar(posgrado_start) 		/// Treatment time
						long2 						/// Calculate results relative to -1
						asinr 						/// Calculate pre-treatment results as in R
						method(drimp)				/// Use doubly robust improved method
						notyet						/// Request using eventually treated
						cluster(personabasicaid)					
			estat event, post						// Aggregate estimation like an event-study
			
			regsave using "${output_matching}", append ci level(95) ///
					addlabel(treatment_group, `treated', outcome, `outcome', mean, `mean', n, `obs', wboot, 0, estimation, NET, cohort, `cohort', controls, 1)
			
			estat event, wboot post					// SE using WildBootstrap
			regsave using "${output_matching}", append ci rtable ///
					addlabel(treatment_group, `treated', outcome, `outcome', mean, `mean', n, `obs', wboot, 1, estimation, NET, cohort, `cohort', controls, 1)
					
		}
		
	}

	cap restore		
		
}
timer off 2
****************************************************************************
**#						5. CS eventually treated only
****************************************************************************			
timer on 3
foreach treated in $treatment_groups {    
	
	keep if eventually_treated == 1
	keep if `treated' != .
	
	foreach cohort in $cohorts  {
			
		preserve	
		keep if year_grado >= `cohort'
		
		foreach outcome in $outcomes {
						
			dis as err "Running event study for `outcome'"
			
			* Without controls
			qui sum `outcome' if mi(dist)
			local mean = r(mean)
			
			qui sum `outcome' if fecha_pila == 2015
			local obs  = r(N) 
			
			csdid2 		`outcome',					///
						i(personabasicaid) 			/// panel id variable
						t(fecha_pila) 				/// Time variable
						gvar(posgrado_start) 		/// Treatment time
						long2 						/// Calculate results relative to -1
						asinr 						/// Calculate pre-treatment results as in R
						method(drimp)				/// Use doubly robust improved method
						notyet						/// Request using eventually treated
						cluster(personabasicaid)					
			estat event, post						// Aggregate estimation like an event-study
			
			regsave using "${output_matching}", append ci level(95) ///
					addlabel(treatment_group, `treated', outcome, `outcome', mean, `mean', n, `obs', wboot, 0, estimation, ET, cohort, `cohort', controls, 0)
			
			estat event, wboot post					// SE using WildBootstrap
			regsave using "${output_matching}", append ci rtable ///
					addlabel(treatment_group, `treated', outcome, `outcome', mean, `mean', n, `obs', wboot, 1, estimation, ET, cohort, `cohort', controls, 0)	

			* With controls
			qui sum `outcome' if mi(dist)
			local mean = r(mean)
			
			qui sum `outcome' if fecha_pila == 2015
			local obs  = r(N) 
			
			csdid2 		`outcome' $controls ,		///
						i(personabasicaid) 			/// panel id variable
						t(fecha_pila) 				/// Time variable
						gvar(posgrado_start) 		/// Treatment time
						long2 						/// Calculate results relative to -1
						asinr 						/// Calculate pre-treatment results as in R
						method(drimp)				/// Use doubly robust improved method
						notyet						/// Request using eventually treated
						cluster(personabasicaid)					
			estat event, post						// Aggregate estimation like an event-study
			
			regsave using "${output_matching}", append ci level(95) ///
					addlabel(treatment_group, `treated', outcome, `outcome', mean, `mean', n, `obs', wboot, 0, estimation, ET, cohort, `cohort', controls, 1)
			
			estat event, wboot post					// SE using WildBootstrap
			regsave using "${output_matching}", append ci rtable ///
					addlabel(treatment_group, `treated', outcome, `outcome', mean, `mean', n, `obs', wboot, 1, estimation, ET, cohort, `cohort', controls, 1)
					
		}
			
	}
		
	cap restore
}

timer off 3

dis "NT duration:"
timer list 1
dis "NET duration:"
timer list 2
dis "ET duration:"
timer list 3
log close