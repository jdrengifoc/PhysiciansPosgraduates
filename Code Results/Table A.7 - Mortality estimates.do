	/*
	
	Replication package
	Table A.7. Mortality estimates
	Last modified: 02/10/2023 
	
	*/
	
clear 		all 
set 		more off


cap log close
log using "Logs/Table A7.smcl", replace

********************************************************************************
	
	* Deaths

	use "Data/Anonimizada/Defunciones_SSO_collapsed", clear
	
	collapse (sum) def* , by(codigo12 fecha)
	drop if codigo12 == ""	
	g byte match = 1
	
	tempfile deaths
	save `deaths'

	
	* Scores
	use 		id_cohorte codigo12 p_draw ave_ecaes_med_administracion 			///
				ave_ecaes_med_spublica ave_prom_health d_area_metro 				///
				p_emb_obs_ave p_fechin_obs_ave p_fechfi_obs_ave 					///
				using "Data/base_hospital_cohorte_bebe"								///
				if p_emb_obs_ave==1 & d_area_metro ==0, replace
	
	duplicates drop
	duplicates report codigo12 p_draw
	
	pca 	ave_ecaes_med_administracion ave_ecaes_med_spublica, com(1)
	predict comp1_health	
	
	keep codigo12 p_draw ave_ecaes_med_administracion ave_ecaes_med_spublica ave_prom_health comp1_health p_fechin_obs_ave p_fechfi_obs_ave id_cohorte
	save		"Data/puntajes_cohorte", replace

	
	* Match with deaths
	use 		"Data/puntajes_cohorte", replace
	
	joinby codigo12 using `deaths', unm(m)
	keep if inrange(fecha, p_fechin_obs_ave, p_fechfi_obs_ave)
	
	collapse (sum) def* , by(codigo12 p_draw ave_prom_health comp1_health id_cohorte)	
	merge 1:1 id_cohorte using "Data/puntajes_cohorte", nogen
	replace defuncion1 = 0 if defuncion1 == .
	replace defuncion2 = 0 if defuncion2 == .
	replace defuncion3 = 0 if defuncion3 == .
	
	tempfile matched
	save `matched'
	* Def 1 refers to fetal deaths, def 2 refers to neonatal deaths, def 3 refers to both.

	
	* Births
	use 		"Data/base_hospital_cohorte_bebe"	 if d_area_metro ==0, replace
	
	g nacidos = inrange(fechanto, p_fechin_obs_ave, p_fechfi_obs_ave)
	collapse (sum) nacidos gest = p_emb_obs_ave, by(codigo12 p_draw)
	merge 1:1 codigo12 p_draw using `matched', nogen
	
	drop if gest == 0	
	egen id_h = group(codigo12)	
	g p_defuncion = defuncion3/(nacidos+defuncion3)
	
	
	* Defunctions only for cohorts with more than 5, 10 and 50 births
	foreach var in nacidos defuncion1 defuncion2 defuncion3 p_defuncion {
		
		g `var'_5b = `var'
		replace `var'_5b = . if nacidos < 5
		
		g `var'_10b = `var'
		replace `var'_10b = . if nacidos < 10
		
		g `var'_50b = `var'
		replace `var'_50b = . if nacidos < 50
		
	}
	

	*Main regressions
	foreach var in nacidos defuncion1 defuncion3 p_defuncion {
		
		rename `var' `var'_0b
		
	}	
	
	gen 	s1=0
	mat 	table1 = J(5,6,.)
	
	local c=1
	foreach var in defuncion1 defuncion3 p_defuncion   {
		
		foreach sub in 10b   {
		
			foreach x in ave_prom_health comp1_health   {

			reghdfe `var'_`sub' `x', abs(p_draw) cluster(id_h)
			replace s1=(e(sample)==1)
				
			reghdfe `var'_`sub' `x', abs(p_draw) cluster(id_h)
			mat 	table1[1,`c']  = _b[`x']
			mat 	table1[2,`c'] = _se[`x']
				
			mat table1[5,`c'] = e(N) // obs
							
			sum `var'_`sub' if e(sample)==1
			mat table1[4,`c']  = r(mean)
			mat table1[3,`c'] = table1[1,`c']/table1[4,`c']
			
			local c= `c'+1
			
			}
		
		}
	
	}
	
	matlist table1 
	putexcel set "Results\Tables paper matrices replication.xls", sheet("Raw Table A7") modify
	putexcel B2=mat(table1)		
	

log close

	