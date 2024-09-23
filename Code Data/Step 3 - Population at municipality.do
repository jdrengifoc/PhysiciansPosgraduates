
* Get sample municipalities

clear 		all 
set 		more off

cap log close
log using "Logs/Data - Step 3.smcl", replace


use		"Data\base_hospital_cohorte_bebe.dta", replace
	
keep 		if p_emb_obs_ave==1 
keep        if d_area_metro ==0


keep codmundep
rename codmundep MPIO_CCNCT
duplicates drop

tempfile tempfile
save `tempfile'


* Merge with population data in 2005
import excel "Data\Anonimizada\DCD-area-proypoblacion-Mun-2005-2019.xlsx", sheet("NuevaMpal") cellrange(A12:H50502) firstrow allstring clear

keep if ÁREAGEOGRÁFICA == "Total"
keep if AÑO == "2005"

keep MPIO Población
destring Población, replace
sort MPIO


rename MPIO MPIO_CCNCT
rename Población poblacion

replace poblacion = poblacion/100000

merge 1:1 MPIO_CCNCT using `tempfile'
replace poblacion = . if _merge != 3
drop _merge

compress
save "Data\poblacion_munic", replace

log close