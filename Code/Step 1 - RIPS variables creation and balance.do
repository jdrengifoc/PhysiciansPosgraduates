/*************************************************************************
 *************************************************************************			       	
	        RIPS variables creation
			 
1) Created by: Juan David Rengifo Castro
               Banco de la República
               jdrengifoc@eafit.edu.co
               
               Pablo Uribe						      
			         World Bank						        
			         puribebotero@worldbank.org		
			         
			         Daniel Márquez
			         Harvard Business School
				       dmarquezm20@gmail.com
				       
2) Date: December 2023

3) Objective: Retrieves the RIPS variables across modules and years for our sample
			  This do file can only be ran at BanRep's servers.

4) Input: - Merge_individual_RIPS.dta
          - master_rethus.dta

5) Output:	- Individual_balanced_all_RIPS.dta
*************************************************************************
*************************************************************************/	


****************************************************************************
*Globals
****************************************************************************

global FOLDER_PROYECTO "//wmedesrv/gamma/Christian Posso/_banrep_research/proyectos/PhysiciansPosgraduates"
global data "${FOLDER_PROYECTO}/Data"
global logs "${FOLDER_PROYECTO}/Logs"

cap log close
log using "${logs}/step1_create_RIPS_variables_balance.smcl", replace
timer clear
timer on 1
********************************************************************************
**#                   1. Read R output files         
********************************************************************************

use "${data}\Merge_individual_RIPS.dta", clear
*merge m:1 personabasicaid using "${data}\master_rethus.dta", keepusing(personabasicaid rethus_sexo) keep(3) nogen 

gen year_RIPS = yofd(date)
drop date
replace service = "procedimientos" if service == "p"
replace service = "consultas" if service == "c"
replace service = "urgencias" if service == "u"
replace service = "Hospitalizacion" if service == "h"

********************************************************************************
**#                   2. Variable creation          
********************************************************************************

* Create variables by looping over diagnosis codes
local i = 1
foreach d in diag_prin diag_r1 diag_r2 diag_r3 {

	* Pregnancy
	gen pregnancy_`i' 	= (inlist(substr(`d',1,4),"Z340","Z348","Z349") | ///
	inlist(substr(`d',1,2),"O1","O2","O3","O4","O5","O6","O7","O8","O9"))
	
	* Depression
	gen depresion_`i' 	= (inlist(substr(`d',1,3),"F32","F33"))
	
	* Anxiety
	gen ansiedad_`i' 	= (inlist(substr(`d',1,3),"F40","F41"))

	* Stress
	gen estres_`i' 		= ((substr(`d',1,2)=="F3")   | (substr(`d',1,4)=="Z563") | ///
							   (substr(`d',1,4)=="Z637") | (substr(`d',1,4)=="Z733") )

	* Mental diagnosis: complete chapter of mental diagnosis
	gen diag_mental_`i'	 = (substr(substr(`d', 1, 3),1,1) == "F")
	
	* Mental diagnosis: only for depression, anxiety and stress
	gen diag_mental2_`i' = (depresion_`i' == 1 | ansiedad_`i' == 1 | estres_`i' == 1)

	* Stroke
	gen stroke_`i' = (substr(`d', 1, 3) == "I61" 					///
	| substr(`d', 1, 3) == "I60" | substr(`d', 1, 3) == "I64" 		///
	| substr(`d', 1, 3) == "I63" |`d' == "G934" |`d' == "G935" 		///
	| `d' == "G936" | `d' == "S062" | `d' == "S065" | `d' == "E104" ///
	| `d' == "G412" | `d' == "I638" | `d' == "S066" | `d' == "P294" ///
	| `d' == "G464" |`d' == "G407" | `d' == "R568")

	* Infarcts
	gen infarct_`i' = (substr(`d', 1, 3) == "I20" 					///
	| substr(`d', 1, 3) == "I21" | substr(`d', 1, 3) == "I22" 		///
	| substr(`d', 1, 3) == "I23" | substr(`d', 1, 3) == "I24" 		///
	| substr(`d', 1, 3) == "I25")

	* Cardiovascular
	gen cardiovascular_`i' = (substr(`d', 1, 3)== "I46" | substr(`d', 1, 3) == "I50")	
	
	* Digestive 
	gen digestive_`i' = (substr(`d', 1, 3) == "K35" | `d' == "K922" ///
	| `d' == "K353" | `d' == "A542" | `d' == "K441" | `d' == "N835" ///
	| `d' == "I713")
	
	* Diag laboral
	gen diag_laboral_`i' = ((substr(`d',1,3)=="R53") 				///
	| (substr(`d',1,3)=="Y96") | (substr(`d',1,3)=="Z56") 			///
	| (substr(`d',1,3)=="Z57") | (substr(`d',1,4)=="Z732"))

	* Stress laboral
	gen estres_laboral_`i' = ((substr(`d',1,4)=="F480") 			///
	| (substr(`d',1,4)=="F488") | (substr(`d',1,4)=="Z563"))

	local i = `i' + 1
}

foreach var in pregnancy diag_mental diag_mental2 depresion ansiedad estres diag_laboral stroke infarct cardiovascular digestive estres_laboral {
	
	egen `var' = rowmax(`var'_1 `var'_2 `var'_3 `var'_4)	
	drop `var'_1 `var'_2 `var'_3 `var'_4
	
}

drop depresion ansiedad estres

* Mental health variables

gen accidente_laboral  	= (causa_externa==1)
gen enfermedad_laboral 	= (causa_externa==14)
gen acc_enf_laboral		= (accidente_laboral == 1 | enfermedad_laboral == 1)


global t_sensitive pregnancy stroke infarct cardiovascular digestive 
global work diag_mental diag_mental2 diag_laboral estres_laboral			///
			accidente_laboral enfermedad_laboral acc_enf_laboral


* Intensive margin
gen contador 				= 1
gen contador_nopreg			= 1 if (substr(substr(diag_prin, 1, 3),1,1) != "O")
gen contador_mental 		= 1 if (diag_mental   == 1)
gen contador_mental2 		= 1 if (diag_mental2  == 1)
compress


* Count number of health services by month and keep one observation per person and date
bys personabasicaid year_RIPS service: gegen nro_servicios 		= total(contador)
bys personabasicaid year_RIPS service: gegen nro_servicios_np	= total(contador_nopreg)

bys personabasicaid year_RIPS service: gegen nro_serv_mental 	= total(contador_mental)
bys personabasicaid year_RIPS service: gegen nro_serv_mental2 	= total(contador_mental2)


* Replace all values for each person in a single year with the maximum value (1 if happens that year)
foreach variable in $t_sensitive $work {
	
    dis as err "Creating variable for `variable'"
	bys personabasicaid year_RIPS: ereplace `variable' = max(`variable')
	
}


keep 	personabasicaid year_RIPS service nro_servicios nro_servicios_np 		  		///
		nro_serv_mental* $t_sensitive $work 
compress

********************************************************************************
**#           3. Duplicates removal and data reshape        
********************************************************************************

* Keep a single observation per person-year-module
gduplicates drop personabasicaid year_RIPS service, force

* Reshape the dataset to get an individual-year panel
greshape wide  nro_servicios nro_servicios_np nro_serv_mental nro_serv_mental2,  					///
		       i(personabasicaid year_RIPS) j(service) string

foreach var of varlist nro* {
	
	replace `var' = 0 if mi(`var')
	
}

tempfile temp
save `temp', replace


********************************************************************************
**#           			4. Balance the panel     
********************************************************************************

use personabasicaid using "${data}\master_rethus.dta", clear

expand 14 // 14 years between 2009 and 2022
bys personabasicaid: gen year_RIPS = (_n - 1) + 2009
merge 1:1 personabasicaid year_RIPS using `temp', nogen keep(1 3)

* Replace unmatched observations with zeroes
foreach var of varlist nro* $t_sensitive $work {
	
	replace `var' = 0 if mi(`var')
	
}

********************************************************************************
**#           	5. Additional variable creation
********************************************************************************

gen proce 		    = (nro_serviciosprocedimientos > 0)
gen consul 	        = (nro_serviciosconsultas > 0)
gen urg 		    = (nro_serviciosurgencias > 0) 
gen hosp 		    = (nro_serviciosHospitalizacion > 0)

gen urg_np 		    = (nro_servicios_npurgencias > 0) 
gen hosp_np		    = (nro_servicios_npHospitalizacion > 0)

gen consul_mental   = (nro_serv_mentalconsultas > 0)
gen consul_mental2  = (nro_serv_mental2consultas > 0)
gen proce_mental    = (nro_serv_mentalprocedimientos > 0) 
gen urg_mental 	    = (nro_serv_mentalurgencias > 0) 
gen hosp_mental     = (nro_serv_mentalHospitalizacion > 0)

gen service 	    = (nro_serviciosprocedimientos > 0 | ///
                      nro_serviciosconsultas > 0 |  ///
					  nro_serviciosurgencias > 0 | ///
                      nro_serviciosHospitalizacion > 0)					   
gen service_mental  = (proce_mental == 1 | consul_mental == 1 | /// 
                       urg_mental == 1 | hosp_mental == 1)
gen service_mental2	= (proce_mental  == 1 | consul_mental2  == 1 | ///
                       urg_mental  == 1 | hosp_mental  == 1)

/*
gen 	year_mental = year_RIPS if consul_mental == 1
bys 	personabasicaid: ereplace year_mental = min(year_mental)
gen 	consul_mental_forever = 0
replace consul_mental_forever = 1 if year_RIPS >= year_mental
drop 	year_mental

gen 	year_mental = year_RIPS if consul_mental2 == 1
bys 	personabasicaid: ereplace year_mental = min(year_mental)
gen 	consul_mental2_forever = 0
replace consul_mental2_forever = 1 if year_RIPS >= year_mental
drop 	year_mental

gen 	year_mental = year_RIPS if service_mental == 1
bys 	personabasicaid: ereplace year_mental = min(year_mental)
gen 	service_mental_forever = 0
replace service_mental_forever = 1 if year_RIPS >= year_mental
drop 	year_mental

gen 	year_mental = year_RIPS if service_mental2 == 1
bys 	personabasicaid: ereplace year_mental = min(year_mental)
gen 	service_mental2_forever = 0
replace service_mental2_forever = 1 if year_RIPS >= year_mental
drop 	year_mental
*/

compress

* Save the final balanced RIPS panel
save "${data}\Individual_balanced_all_RIPS.dta", replace


timer off 1
timer list 1
log close
