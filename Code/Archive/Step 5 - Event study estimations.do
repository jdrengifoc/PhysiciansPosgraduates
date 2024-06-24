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

use "${data}\Individual_balanced_all_RIPS", clear
rename year_RIPS fecha_pila
drop if mi(fechapregrado)
merge 1:1 personabasicaid fecha_pila using "${data}\Individual_balanced_all_PILA"

drop if _merge == 2 // Estos son los del 2008

keep if (year_grado >= 1995)
drop if (rethus_sexo != 1 & rethus_sexo != 2)
keep if inrange(posgrado_start, 2011, 2017) | mi(posgrado_start) 

global previous pre2 pre3
global post pos0 pos1 pos2 pos3 pos4 pos5 pos6

* Create the distance-to-event dummies
gen dist = fecha_pila - posgrado_start
gen pre3 = dist < -2
gen pos6 = dist >  5

forval i = 2(-1)1 {	
	gen pre`i' = (dist == -`i')	
}

forval i = 0/5 {	
	gen pos`i' = (dist == `i')
}

* Esto lo tenemos que borrar cuando corramos rips desde el comienzo
keep if rethus_codigoperfilpre1 == "P07"

****************************************************************************
**#						2. All results
****************************************************************************			

* Last outcomes
g service_mental3 	= (service_mental == 1 & service_mental2 == 0)
g laboral			= (diag_laboral == 1 		| estres_laboral == 1 	| accidente_laboral == 1 | ///
					   enfermedad_laboral == 1	| acc_enf_laboral == 1	)
g cardio_risk		= (stroke == 1 | infarct == 1 | cardiovascular == 1)

global outcomes service_mental service_mental2 service_mental3 diag_laboral estres_laboral			///
				accidente_laboral enfermedad_laboral acc_enf_laboral laboral						///
				pregnancy stroke infarct cardiovascular cardio_risk digestive 						///
				pila_independientes p_cotizaciones pila_salario_r pila_salario_r_max sal_dias_cot

/*
* TWFE regressions
local replace replace
foreach outcome in $outcomes {
		
	dis as err "Running event study for `outcome'"
	qui sum `outcome' if mi(dist)
	local mean = r(mean)
	
	reghdfe `outcome' $previous $post, 												///
			a(personabasicaid fecha_pila) vce(cluster personabasicaid)
	
	regsave $previous $post using "${output}\Tables\ES_results", `replace' ci 		///
			level(95) addlabel(outcome, `outcome', mean, `mean')
	
	local replace append
	
}
*/
		
replace posgrado_start = 0 	if	posgrado_start == .

* CS regressions
local replace replace
foreach outcome in $outcomes {
		
	
	dis as err "Running event study for `outcome'"
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
			
	estat event, post			// Aggregate estimation like an event-study			
	regsave using "${output}\Tables\CS_results", `replace' ci 				///
			level(95) addlabel(outcome, `outcome', mean, `mean', n, `obs', wboot, 0)
	
	local replace append
	
	estat event, wboot post
	regsave using "${output}\Tables\CS_results", `replace' ci rtable		///
			addlabel(outcome, `outcome', mean, `mean', n,  `obs', wboot, 1)	
	
	

}


****************************************************************************
**#						3. Main results TWFE
****************************************************************************
/*
sum service_mental_forever if mi(dist) 
g service_mental_forever_rel = (service_mental_forever/r(mean))*100

reghdfe service_mental_forever $previous $post ,				///
			a(personabasicaid fecha_pila) vce(cluster personabasicaid)
	
reghdfe service_mental_forever_rel $previous $post ,				///
			a(personabasicaid fecha_pila) vce(cluster personabasicaid)

sum service_mental if mi(dist) 
g service_mental_rel = (service_mental/r(mean))*100

reghdfe service_mental $previous $post ,				///
			a(personabasicaid fecha_pila) vce(cluster personabasicaid)
	
reghdfe service_mental_rel $previous $post ,				///
			a(personabasicaid fecha_pila) vce(cluster personabasicaid)
	
****************************************************************************
**#						4. Main results CS
****************************************************************************
		
replace posgrado_start = 0 	if	posgrado_start == .

csdid2 		service_mental_forever,		///
			i(personabasicaid) 			/// panel id variable
			t(fecha_pila) 				/// Time variable
			gvar(posgrado_start) 		/// Treatment time
			long2 						/// Calculate results relative to -1
			asinr 						/// Calculate pre-treatment results as in R
			method(drimp)				/// Use doubly robust improved method
			cluster(personabasicaid)			
			estat event, post			// Aggregate estimation like an event-study			
			estat event, wboot

csdid2 		service_mental_forever_rel,	///
			i(personabasicaid) 			/// panel id variable
			t(fecha_pila) 				/// Time variable
			gvar(posgrado_start) 		/// Treatment time
			long2 						/// Calculate results relative to -1
			asinr 						/// Calculate pre-treatment results as in R
			method(drimp)				/// Use doubly robust improved method
			cluster(personabasicaid)
			estat event, post			// Aggregate estimation like an event-study			
			estat event, wboot
			
csdid2 		service_mental,				///
			i(personabasicaid) 			/// panel id variable
			t(fecha_pila) 				/// Time variable
			gvar(posgrado_start) 		/// Treatment time
			long2 						/// Calculate results relative to -1
			asinr 						/// Calculate pre-treatment results as in R
			method(drimp)				/// Use doubly robust improved method
			cluster(personabasicaid)
			estat event, post			// Aggregate estimation like an event-study			
			estat event, wboot

csdid2 		service_mental_rel,			///
			i(personabasicaid) 			/// panel id variable
			t(fecha_pila) 				/// Time variable
			gvar(posgrado_start) 		/// Treatment time
			long2 						/// Calculate results relative to -1
			asinr 						/// Calculate pre-treatment results as in R
			method(drimp)				/// Use doubly robust improved method
			cluster(personabasicaid)
			estat event, post			// Aggregate estimation like an event-study			
			estat event, wboot		

			
****************************************************************************
**#						5. In edition
****************************************************************************
	
local replace replace

foreach speciality in $specialities {

	foreach outcome in $outcomes {
		
		foreach gender in $genders {
	
			preserve
			
			if "`gender'" == "all" {
				dis as err "All"
			}
			if "`gender'" == "female" {
				dis as err "Women only"
				keep if rethus_sexo == 2
			}
			if "`gender'" == "male" {
				dis as err "Men only"
				keep if rethus_sexo == 1
			}

			dis as err "Running event study for PILA `ocupacion' in `outcome'"
			qui sum `outcome' if (pre1 == 1 & rethus_codigoperfilpre1 == "`speciality'")
			local mean = r(mean)
			
			reghdfe `outcome' $previous $post if (inrange(year_grado, 2011, 2017) 	///
			& rethus_codigoperfilpre1 == "`speciality'"), 							///
			a(personabasicaid fecha_pila) vce(cluster personabasicaid)
			
			regsave $previous $post using "${tables}\ES_results", `replace' ci 	///
			level(95) addlabel(outcome, `outcome', speciality, `speciality', mean, `mean', gender, `gender')
			
			local replace append
			
			restore

		}
		
	}	
	
}

*/
log close
