/*************************************************************************
 *************************************************************************			       	
	        PILA balancing and appending
			 
1) Created by: Pablo Uribe						Daniel Márquez
			   World Bank						Harvard Business School
			   puribebotero@worldbank.org		dmarquezm20@gmail.com
				
2) Date: December 2023

3) Objective: Balance the PILA panel at a semiannual level and append the 
			  four occupations together.
			  This do file can only be ran at BanRep's servers.

4) Output:	- Individual_balanced_all_PILA.dta
*************************************************************************
*************************************************************************/	


****************************************************************************
*Required packages (uncomment if running for the first time)
****************************************************************************
*ssc install ereplace, replace

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

global data 		"${pc}\Proyectos\Banrep research\PhysiciansPosgraduates\Data"

cap log close
log using "${pc}\Proyectos\Banrep research\PhysiciansPosgraduates\Logs\Step_4.smcl", replace

****************************************************************************
**#				1. Process each occupation's dataset
****************************************************************************

* Balancing with rethus sample
use personabasicaid fechapregrado rethus_codigoperfilpre1 rethus_sexo if rethus_codigoperfilpre1 == "P07" using "${data}\master_rethus", clear
drop rethus_codigoperfilpre1

* Balancear panel
bys personabasicaid: egen temp = min(fechapregrado)
drop if temp == .

expand 176 // 176 months between 2008 and 2022m8
bys personabasicaid: gen fecha_pila = _n + 575
format fecha_pila %tm

merge 1:1 personabasicaid fecha_pila using "${data}\\P07_PILA_monthly", keep(1 3)

gen formal = (_merge == 3)
drop _merge
replace pila_dependientes 	= 0 if mi(pila_dependientes)
replace pila_independientes = 0 if mi(pila_independientes)

bys personabasicaid: ereplace fechapregrado = max(fechapregrado)
drop if mi(fechapregrado)
replace year_grado = year(dofm(fechapregrado))

* Postgraduate PILA
gen 	posgrado_salud 			= (tipo_cotiz == 21)
replace posgrado_salud 			= 0 if (fecha_pila < fechapregrado)

* Postgraduate starting date
gen 	posgrado_start			= fecha_pila if posgrado_salud == 1
bys 	personabasicaid:		ereplace posgrado_start = min(posgrado_start)

* Postgraduate RETHUS
gen 	month_posgrado 			= mofd(rethus_fechagradopos1)
bys 	personabasicaid:		ereplace month_posgrado = min(month_posgrado)
gen 	posgrado_rethus 		= 0
replace posgrado_rethus 		= 1  if (month_posgrado == fecha_pila)

replace rethus_perfilpos1		= "" if (month_posgrado > fecha_pila)
replace rethus_codigoperfilpos1 = "" if (month_posgrado > fecha_pila)

gen auxiliar 	  = substr(rethus_perfilpos1, 1, 1)
gen posgrado_clin = 1 if auxiliar == "M" & rethus_codigoperfilpre1 == "P07" & rethus_codigoperfilpos1 != "MA99"
gen posgrado_quir = 1 if auxiliar == "Q" & rethus_codigoperfilpre1 == "P07"
gen posgrado_otro = 1 if posgrado_clin != 1 & posgrado_quir != 1 & !mi(rethus_perfilpos1)
drop auxiliar

* PILA variables
rename 	pila_salario_max_r pila_salario_r_max

replace pila_salario_r	 		= 0 if mi(pila_salario_r)
replace pila_salario_r_max		= 0 if mi(pila_salario_r_max)	
replace sal_dias_cot			= 0 if mi(sal_dias_cot)
replace nro_cotizaciones		= 0 if mi(nro_cotizaciones)	

* Annualize
replace fecha_pila 				= yofd(dofm(fecha_pila))
replace fechapregrado 			= yofd(dofm(fechapregrado))
replace month_posgrado			= yofd(dofm(month_posgrado))
replace posgrado_start			= yofd(dofm(posgrado_start))

format fecha_pila %ty
format fechapregrado %ty
format month_posgrado %ty
format posgrado_start %ty

collapse 	(median) sal_dias_cot pila_salario_r pila_salario_r_max							///				
			(max) nro_cotizaciones formal incap_dias incap_gral licen_mat					///
			posgrado_salud posgrado_start posgrado_rethus posgrado_clin						///
			posgrado_quir posgrado_otro pila_independientes pila_dependientes, 				///
			by(personabasicaid fecha_pila fechapregrado year_grado rethus_sexo)

* Last variables
gen 	l_pila_salario_r 		= log(pila_salario_r)
gen		p_cotizaciones			= (nro_cotizaciones > 1)

* Identify occupation
gen 	rethus_codigoperfilpre1 = "P07"

save "${data}\Individual_balanced_all_PILA.dta", replace

log close
