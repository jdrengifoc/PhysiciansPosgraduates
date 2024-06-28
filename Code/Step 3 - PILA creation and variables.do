/*************************************************************************
 *************************************************************************			       	
	        PILA dataset creation by month
			 
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

3) Objective: Create the PILA panel with our main sample for each of the
			  occupations.
			  This do file can only be ran at BanRep's servers.

4) Output:	- P01_PILA_monthly.dta
			- P03_PILA_monthly.dta
			- P07_PILA_monthly.dta
			- P09_PILA_monthly.dta
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

global data_rethus 	"${pc}\Proyectos\Banrep research\f_ReturnsToEducation Health sector\Data"
global data 		"${pc}\Proyectos\Banrep research\Returns to Health Sector\Data"

cap log close
log using "${pc}\Proyectos\Banrep research\Returns to Health Sector\Logs\Rethus-Pila.smcl", replace


****************************************************************************
**#  1. Create each occupations dataset by merging with PILA
****************************************************************************

use "${data_rethus}\RETHUS_procesada.dta", clear
*drop if !mi(rethus_codigoperfilpre2) // Drop individuals with more than one major? Maybe sample is not big enough

keep if rethus_codigoperfilpre1 == "P07"

gen year_licencia 	= year(dofm(fechalicenciapregrado))
gen year_grado 		= year(dofm(fechapregrado))
gen year_posgrado 	= year(rethus_fechagradopos1)
gen rethus_posgrado = !mi(rethus_codigoperfilpos1)

keep personabasicaid rethus_sexo rethus_posgrado year_posgrado 			///
fechalicenciapregrado year_licencia fechapregrado year_grado 			///
rethus_fechagradopos1 year_posgrado rethus_codigoperfilpos1 			///
rethus_perfilpos1 rethus_codigoperfilpre1

order personabasicaid rethus_sexo fechalicenciapregrado year_licencia 	///
fechapregrado year_grado rethus_fechagradopos1 year_posgrado 			///
rethus_codigoperfilpos1 rethus_perfilpos1 rethus_codigoperfilpre1 		///
rethus_posgrado year_posgrado 

compress
tempfile temp_P07
save `temp_P07'


* Create empty dataset where each month will be appended recursively
clear all
gen delete = .
save "${data}\\P07_PILA_monthly.dta", replace

* Loop through PILA
forval y=2008/2022 {
	
	if `y'==2022 {
		global loop "01 02 03 04 05 06 07 08"
	}
	else {
		global loop "01 02 03 04 05 06 07 08 09 10 11 12"
	}
	

	foreach i of global loop {

		di as err "********* `y'm`i' *********" 
		cd "\\sm134796\D\Originales\PILA\1.Pila mensualizada\PILA mes cotizado"
		
		use personabasicaid id ibc* salario_bas sal_dias_cot tipo_cotiz sexomode fechantomode ///
			ciudad_cod depto_cod licen_mat incap_gral incap_trab using "`y'm`i'cBR", clear
			compress
		
		merge m:1 personabasicaid using `temp_P07', keep(3) nogen
		
		cap drop year month
		gen year 		= `y'
		gen month 		= `i'
		gen fecha_pila	= ym(`y',`i')
		format fecha_pila %tm
		
		*New variables
		rename	tipo_cotiz tipo_cotizante
		
		gen 	pila_independientes = inlist(tipo_cotizante, 2, 3, 16, 41, 42, 59, 57, 66)			
		gen 	pila_posgrad_salud 	= (tipo_cotizante == 21)	
		gen 	pila_dependientes   = (pila_independientes != 1 & pila_posgrad_salud! = 1)
		
		foreach var of varlist salario_bas ibc_pens ibc_sal ibc_rprof ibc_ccf  {
			
			rename `var' `var'_orig
			bys personabasicaid id: egen 	`var' = max(`var'_orig) if (pila_dependientes == 1)
			bys personabasicaid id: replace `var' = `var'_orig if (pila_independientes == 1)
			
		}

		*Maternity license
		g licen_mat1 = (licen_mat == "X")
		drop licen_mat
		rename licen_mat1 licen_mat
		
		*General incapacity
		g incap_gral1 = (incap_gral == "X")
		drop incap_gral
		rename incap_gral1 incap_gral
		
		*Days of incapacity
		replace incap_trab = 0 if incap_trab < 0
		rename incap_trab incap_dias
		
		drop salario_bas_orig ibc_pens_orig ibc_sal_orig ibc_rprof_orig ibc_ccf_orig
		
		*Minimum wage per year
		gen     mw = 461500  if (year == 2008)
		replace mw = 496900  if (year == 2009)
		replace mw = 515000  if (year == 2010)
		replace mw = 535600  if (year == 2011)
		replace mw = 566700  if (year == 2012)
		replace mw = 589500  if (year == 2013)
		replace mw = 616000  if (year == 2014)
		replace mw = 644350  if (year == 2015)
		replace mw = 689455  if (year == 2016)
		replace mw = 737717  if (year == 2017)
		replace mw = 781242  if (year == 2018)
		replace mw = 828116  if (year == 2019)
		replace mw = 877803  if (year == 2020)
		replace mw = 908526  if (year == 2021)
		replace mw = 1000000 if (year == 2022)
		
		*Minimum wage t-1
		gen     mw_1 = 461500  if (year == 2009)
		replace mw_1 = 496900  if (year == 2010)
		replace mw_1 = 515000  if (year == 2011)
		replace mw_1 = 535600  if (year == 2012)
		replace mw_1 = 566700  if (year == 2013)
		replace mw_1 = 589500  if (year == 2014)
		replace mw_1 = 616000  if (year == 2015)
		replace mw_1 = 644350  if (year == 2016)
		replace mw_1 = 689455  if (year == 2017)
		replace mw_1 = 737717  if (year == 2018)
		replace mw_1 = 781242  if (year == 2019)
		replace mw_1 = 828116  if (year == 2020)
		replace mw_1 = 877803  if (year == 2021)
		replace mw_1 = 908526  if (year == 2022)
		replace mw_1 = 1000000 if (year == 2023)

		egen	rowmax 		= rowmax(ibc*)
		replace rowmax 		= mw if (rowmax >= mw_1 * 0.8 & rowmax < mw)		
		replace rowmax 		= rowmax / 0.4 if (pila_independientes == 1 & rowmax > mw)
		
		replace salario_bas = mw if (rowmax >= mw_1 * 0.8 & rowmax < mw)
		
		egen	pila_salario = rowmax(rowmax salario_bas)
		replace pila_salario = .  if (pila_posgrad_salud == 1)
		lab var pila_salario 	"Salario nominal"
		drop 	rowmax
		
		gen		pila_salario_max = pila_salario
		
		*Get the CPI
		merge m:1 year month using "\\sm037577\D\Proyectos\Banrep research\c_2018_SSO Servicio Social Obligatorio\Project SSO Training\Data\IPC mensual", keepusing(IPC) keep(1 3) nogen
		
		*Generate real wages (base 2018m12)
		global vars pila_salario pila_salario_max
		
		foreach var in $vars {
			
			gen     `var'_r = (`var' / IPC) * 100
			replace `var'_r = . if mi(`var')
			
		}
		
		
		* Remove duplicates of contributions with same company
		gduplicates drop personabasicaid id if pila_dependientes == 1, force
		
		bys personabasicaid: gen nro_cotizaciones = _N
			
		* Since people may have more than one contribution each month, sum the wages of each contribution and keep the max of worked days. 
		foreach var of varlist pila_salario_r {
			
		  bys personabasicaid: ereplace `var' 				 = total(`var')
		  bys personabasicaid: egen 	`var'_dependientes 	 = total(`var') if (pila_dependientes == 1)
		  bys personabasicaid: egen 	`var'_independientes = total(`var') if (pila_independientes == 1)
		  
		}

		foreach var of varlist sal_dias_cot pila_dependientes pila_independientes pila_posgrad_salud pila_salario_max_r incap_dias incap_gral licen_mat {
		  bys personabasicaid: ereplace `var' = max(`var')
		}
		
		replace sal_dias_cot = 30 if (sal_dias_cot > 30 & !mi(sal_dias_cot))
		
		gsort personabasicaid tipo_cotizante
		
		gduplicates drop personabasicaid, force
		
		tostring ciudad_cod depto_cod, replace
		
		replace ciudad_cod 	= "00" + ciudad_cod if (length(ciudad_cod) == 1 & ciudad_cod != ".")
		replace ciudad_cod 	= "0" + ciudad_cod 	if (length(ciudad_cod) == 2 & ciudad_cod != ".")
		
		replace depto_cod 	= "0" + depto_cod 	if (length(depto_cod) == 1 & depto_cod != ".")
		
		gen pila_cod_mun 	= depto_cod + ciudad_cod
		
		*Keep relevant variables
		keep personabasicaid fecha_pila year month sexomode fechantomode *_r 	///
		pila_independientes pila_dependientes pila_cod_mun id 					///
		sal_dias_cot *dependientes *independientes nro_cotizaciones 			///
		pila_posgrad_salud rethus_sexo rethus_posgrado year_posgrado 			///
		fechalicenciapregrado year_licencia fechapregrado year_grado 			///
		rethus_fechagradopos1 year_posgrado rethus_codigoperfilpos1 			///
		rethus_perfilpos1 tipo_cotiz rethus_codigoperfilpre1 					///
		pila_salario_max_r incap_dias incap_gral licen_mat
		
		*Remove any duplicates
		gduplicates drop personabasicaid, force
		
		compress
		
		* Append observations to the file created at the top
		append using "${data}\\P07_PILA_monthly.dta"
		cap drop delete
		
		* Save that file so in the next iteration, the new observations are part of the file
		save "${data}\\P07_PILA_monthly.dta", replace
		
	}
	
}

log close