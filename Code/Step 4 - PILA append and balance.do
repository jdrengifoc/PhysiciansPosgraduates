/*************************************************************************
 *************************************************************************			       	
	        PILA balancing and appending
			 
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

3) Objective: Balance the PILA panel at a semiannual level and append the 
			  four occupations together.
			  This do file can only be ran at BanRep's servers.

4) Output:	- Individual_balanced_all_PILA.dta

5) Notes: 
   - Comment replace posgrado_salud  	= 0 if (fecha_pila < fechapregrado)
*************************************************************************
*************************************************************************/	


****************************************************************************
*Required packages (uncomment if running for the first time)
****************************************************************************
*ssc install ereplace, replace

****************************************************************************
*Globals
****************************************************************************

global FOLDER_PROYECTO "//wmedesrv/gamma/Christian Posso/_banrep_research/proyectos/PhysiciansPosgraduates"
global logs "${FOLDER_PROYECTO}\Logs"
global data "${FOLDER_PROYECTO}\Data"

cap log close
log using "${logs}\Step_4.smcl", replace

****************************************************************************
**#				1. Process each occupation's dataset
****************************************************************************

* Balancing with rethus sample
use personabasicaid using "${data}\master_rethus", clear

* Balancear panel
expand 176 // 176 months between 2008 and 2022m8
bys personabasicaid: gen fecha_pila = _n + 575
format fecha_pila %tm


merge 1:1 personabasicaid fecha_pila using "${data}\\P07_PILA_monthly", keep(1 3)

gen formal = (_merge == 3)
drop _merge
replace pila_dependientes 	= 0 if mi(pila_dependientes)
replace pila_independientes = 0 if mi(pila_independientes)

* Postgraduate PILA
gen 	posgrado_salud 			= (tipo_cotizante == 21)
* replace posgrado_salud  	= 0 if (fecha_pila < fechapregrado)

* Postgraduate starting date
gen 	posgrado_start			= fecha_pila if posgrado_salud == 1
bys 	personabasicaid:	  ereplace posgrado_start = min(posgrado_start)

* Postgraduate RETHUS
/*
gen 	month_posgrado 			= mofd(rethus_fechagradopos1)
bys 	personabasicaid:		ereplace month_posgrado = min(month_posgrado)
gen 	posgrado_rethus 		= 0
replace posgrado_rethus 	    = 1  if (month_posgrado == fecha_pila)

replace rethus_perfilpos1		    = "" if (month_posgrado > fecha_pila)
replace rethus_codigoperfilpos1 = "" if (month_posgrado > fecha_pila)

gen auxiliar 	    = substr(rethus_perfilpos1, 1, 1)
gen posgrado_clin = 1 if auxiliar == "M" & rethus_codigoperfilpos1 != "MA99"
gen posgrado_quir = 1 if auxiliar == "Q"
gen posgrado_otro = 1 if posgrado_clin != 1 & posgrado_quir != 1 & !mi(rethus_perfilpos1)
drop auxiliar
*/
* PILA variables
rename 	pila_salario_max_r pila_salario_r_max

replace pila_salario_r	   = 0 if mi(pila_salario_r)
replace pila_salario_r_max = 0 if mi(pila_salario_r_max)	
replace sal_dias_cot	   = 0 if mi(sal_dias_cot)
replace nro_cotizaciones   = 0 if mi(nro_cotizaciones)	

* Annualize
rename fecha_pila fecha_pila_temp
gen fecha_pila = yofd(dofm(fecha_pila_temp))
drop fecha_pila_temp

/*rename fechapregrado fechapregrado_temp
gen fechapregrado = yofd(fechapregrado_temp)
drop fechapregrado_temp

rename month_posgrado month_posgrado_temp
gen month_posgrado = yofd(dofm(month_posgrado_temp))
drop month_posgrado_temp*/

rename posgrado_start posgrado_start_temp
gen posgrado_start = yofd(dofm(posgrado_start_temp))
drop posgrado_start_temp

format fecha_pila     %ty
/*format fechapregrado  %ty
format month_posgrado %ty*/
format posgrado_start %ty

/*collapse 	(median) sal_dias_cot pila_salario_r pila_salario_r_max							///				
			(max) nro_cotizaciones formal incap_dias incap_gral licen_mat					///
			posgrado_salud posgrado_start posgrado_rethus posgrado_clin						///
			posgrado_quir posgrado_otro pila_independientes pila_dependientes, 				///
			by(personabasicaid fecha_pila fechapregrado year_grado rethus_sexo)*/
			
collapse 	(median) sal_dias_cot pila_salario_r pila_salario_r_max							///				
			(max) nro_cotizaciones formal incap_dias incap_gral licen_mat					///
			posgrado_salud posgrado_start  						///
			pila_independientes pila_dependientes, 				///
			by(personabasicaid fecha_pila)

* Last variables
gen 	l_pila_salario_r 		= log(pila_salario_r)
gen		p_cotizaciones			= (nro_cotizaciones > 1)

save "${data}\Individual_balanced_all_PILA.dta", replace

log close
