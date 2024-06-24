/*************************************************************************
 *************************************************************************			       	
					Event study estimation
			 
1) Created by: Daniel Márquez
			   Harvard Business School
			   dmarquezm20@gmail.com
				
2) Date: Februar 2023

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

* Working directory
if "`c(hostname)'" == "SM201439"{
	global pc "C:"
}

else {
	global pc "\\sm093119"
}

global data 	"${pc}\Proyectos\Banrep research\PhysiciansPosgraduates\Data"
global logs 	"${pc}\Proyectos\Banrep research\PhysiciansPosgraduates\Logs"
global output 	"${pc}\Proyectos\Banrep research\PhysiciansPosgraduates\Output"

cap log close
log using "${logs}\event_study.smcl", replace

* Outcomes and categories
global specialities	all quirurgical clinical other none
global genders 		all female male

/*
global outcomes 	sal_dias_cot posgrado_salud pila_salario_r l_pila_salario_r 				///
					posgrado_rethus p_cotizaciones nro_cotizaciones							 	///
					pila_independientes pila_dependientes formal								///
					diag_mental diag_mental2 service_mental service_mental2 					///
					urg hosp urg_np hosp_np pregnancy service cons_mental 						///
					cons_mental2 cons_mental3 cons_mental4 consul proce
*/
					
****************************************************************************
**#						1. Merge data and set-up
****************************************************************************

* Panel
use "${data}\Individual_balanced_all_RIPS", clear
rename year_RIPS fecha_pila
drop if mi(fechapregrado)
merge 1:1 personabasicaid fecha_pila using "${data}\Individual_balanced_all_PILA"

drop if _merge == 2 // Estos son los del 2008

drop if (rethus_sexo != 1 & rethus_sexo != 2)
keep if inrange(posgrado_start, 2011, 2017) | mi(posgrado_start)

* Final outcomes
g service_mental3 	= (service_mental == 1 		& service_mental2 == 0								)
g cardio_risk		= (stroke == 1 				| infarct == 1 			| cardiovascular == 1	 	)
g laboral			= (diag_laboral == 1 		| estres_laboral == 1 	| accidente_laboral == 1 	| 	///
					   enfermedad_laboral == 1	| acc_enf_laboral == 1								)

* Esto lo tenemos que borrar cuando corramos rips desde el comienzo
keep if rethus_codigoperfilpre1 == "P07"


****************************************************************************
**#						2. Estimation features
****************************************************************************

* Balance
gen dist = fecha_pila - posgrado_start
*keep if (dist >= -2 & dist <= 5)

* Eventually treated dummy
g eventually_treated = (posgrado_start != .)

* Never treated condition
replace posgrado_start = 0 	if	posgrado_start == .

* Outcomes all
/*
global outcomes service_mental service_mental2 service_mental3 diag_laboral estres_laboral				///
				accidente_laboral enfermedad_laboral acc_enf_laboral laboral							///
				pregnancy stroke infarct cardiovascular cardio_risk digestive 							///
				pila_independientes p_cotizaciones pila_salario_r pila_salario_r_max sal_dias_cot
*/

* Outcomes simplified
global outcomes service_mental service_mental2 service_mental3 cardio_risk laboral						///
				pila_independientes p_cotizaciones pila_salario_r sal_dias_cot

* Controls
global controls year_grado


****************************************************************************
**#						3. CS never treated only
****************************************************************************			
					
foreach cohort in 1995 2000 2005 2010 {
	
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
					cluster(personabasicaid)				
		estat event, post						// Aggregate estimation like an event-study
		
		regsave using "${output}\Tables\CS_results", replace ci level(95) ///
				addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 0, estimation, NT, cohort, `cohort', controls, 0)
		
		estat event, wboot post					// SE using WildBootstrap
		regsave using "${output}\Tables\CS_results", append ci rtable ///
				addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 1, estimation, NT, cohort, `cohort', controls, 0)
				
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
		
		regsave using "${output}\Tables\CS_results", append ci level(95) ///
				addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 0, estimation, NT, cohort, `cohort', controls, 1)
		
		estat event, wboot post					// SE using WildBootstrap
		regsave using "${output}\Tables\CS_results", append ci rtable ///
				addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 1, estimation, NT, cohort, `cohort', controls, 1)
	
	}
	
	restore
	
}


****************************************************************************
**#						4. CS with eventually treated
****************************************************************************			

foreach cohort in 1995 2000 2005 2010 {
		
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
		
		regsave using "${output}\Tables\CS_results", append ci level(95) ///
				addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 0, estimation, NET, cohort, `cohort', controls, 0)
		
		estat event, wboot post					// SE using WildBootstrap
		regsave using "${output}\Tables\CS_results", append ci rtable ///
				addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 1, estimation, NET, cohort, `cohort', controls, 0)	

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
		
		regsave using "${output}\Tables\CS_results", append ci level(95) ///
				addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 0, estimation, NET, cohort, `cohort', controls, 1)
		
		estat event, wboot post					// SE using WildBootstrap
		regsave using "${output}\Tables\CS_results", append ci rtable ///
				addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 1, estimation, NET, cohort, `cohort', controls, 1)
				
	}
	
	restore
	
}


****************************************************************************
**#						5. CS eventually treated only
****************************************************************************			

keep if eventually_treated == 1

foreach cohort in 1995 2000 2005 2010 {
		
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
		
		regsave using "${output}\Tables\CS_results", append ci level(95) ///
				addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 0, estimation, ET, cohort, `cohort', controls, 0)
		
		estat event, wboot post					// SE using WildBootstrap
		regsave using "${output}\Tables\CS_results", append ci rtable ///
				addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 1, estimation, ET, cohort, `cohort', controls, 0)	

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
		
		regsave using "${output}\Tables\CS_results", append ci level(95) ///
				addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 0, estimation, ET, cohort, `cohort', controls, 1)
		
		estat event, wboot post					// SE using WildBootstrap
		regsave using "${output}\Tables\CS_results", append ci rtable ///
				addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 1, estimation, ET, cohort, `cohort', controls, 1)
				
	}
	
	restore
	
}


log close
