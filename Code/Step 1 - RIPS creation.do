/*************************************************************************
 *************************************************************************			       	
	        RIPS dataset creation by year
			 
1) Created by: Pablo Uribe						Daniel Márquez
			   World Bank						Harvard Business School
			   puribebotero@worldbank.org		dmarquezm20@gmail.com		
				
2) Date: December 2023

3) Objective: Retrieves the RIPS variables across modules and years for our sample
			  This do file can only be ran at BanRep's servers.

4) Output:	- 14 files titled in the following format with X going from 2009 to 2022: 
			  sample_X_RIPS.dta
*************************************************************************
*************************************************************************/	


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

global data 		"${pc}\Proyectos\Banrep research\Returns to Health Sector\Data"
global urgencias 	"${pc}\Proyectos\Data"
global data_rethus 	"${pc}\Proyectos\Banrep research\f_ReturnsToEducation Health sector\Data"

global RIPS 		"\\sm134796\E\RIPS\Stata"
global RIPS2 		"\\wmedesrv\gamma\rips"


clear all

****************************************************************************
**# 				1. Create main dataset
****************************************************************************

use "${data_rethus}\RETHUS_procesada.dta", clear
*drop if !mi(rethus_codigoperfilpre2) // Drop individuals with more than one major? Maybe sample is not big enough

keep if rethus_codigoperfilpre1 == "P07"

gen year_licencia = year(dofm(fechalicenciapregrado))
gen year_grado = year(dofm(fechapregrado))
gen year_posgrado = year(rethus_fechagradopos1)
gen rethus_posgrado = (rethus_codigoperfilpos1 != "")

keep personabasicaid rethus_sexo rethus_posgrado year_posgrado 			///
fechalicenciapregrado year_licencia fechapregrado year_grado 			///
rethus_fechagradopos1 year_posgrado rethus_codigoperfilpos1 			///
rethus_perfilpos1 rethus_codigoperfilpre1

order personabasicaid rethus_sexo fechalicenciapregrado year_licencia 	///
fechapregrado year_grado rethus_fechagradopos1 year_posgrado 			///
rethus_codigoperfilpos1 rethus_perfilpos1 rethus_posgrado year_posgrado 

compress
save "${data}\master_rethus.dta", replace


****************************************************************************
**# 2. Retrieve variables from RIPS and merge with our main sample
****************************************************************************

* Identify each module in global b
global b "consultas procedimientos urgencias Hospitalizacion"

forvalues year = 2009/2021 {
    
	clear
	gen aux = ""
	save "${data}\RIPS\sample_`year'_RIPS", replace // Save empty dataset where observations will be appended
	
	foreach base of global b {
		
		dis in red "`base'`year'"
		
		if "`base'" == "consultas" | "`base'" == "procedimientos" {
		
			if "`base'"=="consultas" {
				local date 		= "fecha_consul"
				local diag 		= "cod_diag_prin"
				local extra_diag= "cod_diag_r1 cod_diag_r2 cod_diag_r3"
				local causa 	= "causa_externa"
				local consul 	= "cod_consul"
				local lrename rename (cod_diag_r1 cod_diag_r2 cod_diag_r3) ///
				(diag_r1 diag_r2 diag_r3)
			}
					
			if "`base'" == "procedimientos" {
				local date 		= "fecha"
				local diag 		= "diag_prin"
				local extra_diag= "diag_r1"
				local causa 	= ""
				local consul 	= ""
				local lrename 	= ""
			}
			
			
			if `year' < 2019 {
			
				use personabasicaid `diag' `date' `extra_diag' `causa' `consul' using "${RIPS}\\`base'`year'", clear
				
				gen date = date(substr(`date',1,10),"YMD")
				format date %td
				
				`lrename'
								
				compress
			}
			
			else {
				if "`base'"=="consultas" {
					local vari_id 	= "Id"
					local max_i 	= 3
				}
				
				else {
					
					local vari_id 	= "ID"
					
					if `year' == 2020 {
						local max_i = 5
					}
					
					else {
						local max_i = 4
					}
				}
				
				clear
				gen x = ""
				save "${data}/`base'_`year'", replace
				
				forvalues i = 1/`max_i' {
					
					use personabasicaid `diag' `date' `extra_diag' `causa' `consul' using "${RIPS2}\\`base'`vari_id'`year'_`i'", clear
										
					compress
					
					append using "${data}/`base'_`year'"
					save "${data}/`base'_`year'", replace
					
				}
				
				use "${data}/`base'_`year'", clear
				
				gen date = date(substr(`date',1,10) ,"YMD")
				format date %td
				
				`lrename'
				
				erase "${data}/`base'_`year'.dta"
			}
		}
		
		else {
			
			if "`base'" == "urgencias" {
				
				use personabasicaid diag_prin diag_r1 diag_r2 diag_r3 fecha_ingreso dead causa_externa using  "${urgencias}\Master_urgencias_2009_2022", clear
				
				gen date = date(substr(fecha_ingreso,1,10),"YMD")
				format date %td
				
			}
			
			else {
				
				use personabasicaid diag_prin_ingre diag_egre1 diag_egre2 diag_egre3 date dead diag_muerte causa_externa using "${urgencias}\Master_hospitalizaciones_2009_2022", clear
				
				rename (diag_egre1 diag_egre2 diag_egre3) (diag_r1 diag_r2 diag_r3)
				
			}
			
			compress
			
			keep if year(date) == `year'
		}
		
		gen service = "`base'"
		
		drop if mi(personabasicaid)
		
		cap drop aux
	
		destring personabasicaid, force replace
		
		* Merge the module that is currently in memory to our main sample
		merge m:1 personabasicaid using "${data}\master_rethus.dta", keep(3) nogen
		
		cap noi destring causa_externa, replace
		
		* Append observations to the file created at the top
		append using "${data}\RIPS\sample_`year'_RIPS"
	
		compress
		
		* Save that file so in the next iteration, the new observations are part of the file
		save "${data}\RIPS\sample_`year'_RIPS", replace
	}
}


* 2022 data is not in the same format as the rest (it's in TXT).
* The following loop does the same as above but using TXT files instead of dta to call the raw data.

clear
gen aux = ""
save "${data}\RIPS\sample_2022_RIPS", replace

forvalues i = 1/8 {
	* Emergencies
	if `i' == 1 {
	    
		local service = "urgencias"
		
		use personabasicaid diag_prin diag_r1 diag_r2 diag_r3 fecha_ingreso dead causa_externa using "${urgencias}\Master_urgencias_2009_2022", clear
	
		gen date = date(substr(fecha_ingreso,1,10),"YMD")
		format date %td
				
		keep if year(date) == 2022
	}
	
	* Hospitalizations
	if `i' == 2 {
	    
		local service = "Hospitalizacion"
		
		use personabasicaid diag_prin_ingre diag_egre1 diag_egre2 diag_egre3 date dead diag_muerte causa_externa using  "${urgencias}\Master_hospitalizaciones_2009_2022", clear
		
		rename (diag_egre1 diag_egre2 diag_egre3) (diag_r1 diag_r2 diag_r3)
				
		keep if year(date) == 2022
	}
	
	* Consultations
	if `i' == 3 | `i' == 4 {
	    
		local service = "consultas"
		
		if `i' == 3 {
			import delimited "${RIPS2}\BANREP_consultas2022_01_04.txt", clear
		}
		
		else {
			import delimited "${RIPS2}\BANREP_consultas2022_05_09.txt", clear
		}
				
		keep personabasicaid cod_diag_prin cod_diag_r1 cod_diag_r2 cod_diag_r3 fecha_consul causa_externa cod_consul
					
		rename (cod_diag_r1 cod_diag_r2 cod_diag_r3) (diag_r1 diag_r2 diag_r3)

		gen date = date(substr(fecha,1,10) ,"YMD")
		format date %td
	}
	
	* Procedures
	if `i' > 4 {
	    
		local service = "procedimientos"
		
		if `i' == 5 {
			import delimited "${RIPS2}\BANREP_proc2022_01_02.txt", clear
		} 
		
		if `i' == 6 {
			import delimited "${RIPS2}\BANREP_proc2022_03_04.txt", clear
		}
		
		if `i' == 7 {
			import delimited "${RIPS2}\BANREP_proc2022_05_06.txt", clear
		}
		
		if `i' == 8 {
			import delimited "${RIPS2}\BANREP_proc2022_07_09.txt", clear
		}
				
		keep personabasicaid diag_prin diag_r1 fecha
		
		gen date = date(substr(fecha,1,10) ,"YMD")
		format date %td
	}
	
	destring personabasicaid, force replace
	
	cap drop aux
	
	merge m:1 personabasicaid using "${data}\master_rethus.dta", keep(3) nogen
		
	gen service = "`service'"
	
	drop if mi(personabasicaid)
			
	append using "${data}\RIPS\sample_2022_RIPS"
	
	compress
	save "${data}\RIPS\sample_2022_RIPS", replace
}