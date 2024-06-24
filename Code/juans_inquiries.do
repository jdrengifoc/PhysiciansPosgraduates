global data_rethus 	"${pc}\Proyectos\Banrep research\f_ReturnsToEducation Health sector\Data"

use "C:\Proyectos\Banrep research\PhysiciansPosgraduates\Data\Individual_balanced_all_PILA.dta", clear

keep personabasicaid fechapregrado fecha_pila posgrado_rethus posgrado_start

gen year_posgrado = fecha_pila if posgrado_rethus == 1
bys personabasicaid: ereplace year_posgrado = min(year_posgrado)
gen diff_posgrado = year_posgrado - posgrado_start
replace	diff_posgrado = -0.5 if posgrado_start == . & year_posgrado != .
replace	diff_posgrado = 0.5 if year_posgrado == . & posgrado_start != .

merge m:1 personabasicaid using "C:\Proyectos\Banrep research\PhysiciansPosgraduates\Data\master_rethus.dta"

keep if _merge == 3
drop _merge
keep if fecha_pila == 2014

tab diff_posgrado
tab rethus_perfilpos1  if diff_posgrado == 0.5, m
tab	year_posgrado if diff_posgrado == -0.5
tab	rethus_perfilpos1 if diff_posgrado == -0.5 & year_posgrado >= 2015

merge 1:1 personabasicaid using "${data_rethus}\RETHUS_procesada.dta", keepusing(rethus_origentitulopos1)

tab	rethus_origentitulopos1 if diff_posgrado == -0.5 & year_posgrado < 2012

tab	year_posgrado rethus_origentitulopos1 if diff_posgrado == -0.5