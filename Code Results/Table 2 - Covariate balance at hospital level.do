	/*
	
	Replication package
	Table 2 - Balancing hospitals
	
	*/

    
clear 		all 
set 		more off

cap log close
log using "Logs/Table 2.smcl", replace
	
	
********************************************************************************

	use 	        "Data/Anonimizada/base_medico_rural_con_fecha_inicio",replace
    
	cap drop        p_date
	gen             p_date =ym(p_year,p_month)
	format          p_date %tm
	duplicates drop p_num_doc p_date, force
	drop 	        codigo10

	preserve
    
        use         "Data/base_hospital_cohorte_bebe", replace
        keep if     p_emb_obs_ave==1 & d_area_metro==0 
        tempfile    base1
        save        `base1', replace 
        
	restore

	joinby  codigo12 p_date using `base1', unm(b) _merge(merge_pru) 
	keep if p_emb_obs_ave==1 
	keep if d_area_metro ==0
    
	ren 	                codigo12 codigo	
	gsort                   p_num_doc p_date codigo -id_nacido	
	duplicates drop         p_num_doc p_date codigo, force 

	drop 	                p_mes_inicio
	gen 	                p_mes_inicio=ym(2010,1)
	merge m:1               p_mes_inicio codigo using "Data/Anonimizada/SSO_Hospital_Vitales.dta", keep(3) nogen
	
	egen tag                = tag(codigo)
	gen  apgar1men7         = 1-h_prop_v_apgar1_masy7
	egen p75_apgar7         = pctile(apgar1men7)          if tag==1, p(75)
	egen p75_apgar71        = max(p75_apgar7)
	egen p75_bw             = pctile(h_prop_low_weight)   if tag==1, p(75)
	egen p75_bw1		    = max(p75_bw)
	egen p75_prem           = pctile(h_prop_v_nacer_38_41) if tag==1,p(75)
	egen p75_prem1		    = max(p75_prem)
	
	gen dummy_prematurotop  = (h_prop_v_nacer_38_41<p75_prem1   & h_prop_v_nacer_38_41!=.)
	gen dummy_bwtop         = (h_prop_low_weight   >p75_bw1     & h_prop_low_weight   !=.)
	gen dummy_apgartop      = (apgar1men7          >p75_apgar71 & apgar1men7          !=.)
	compress
    
	gen  dummy_1hosp        = (tot_hosp==1)
	gen  apgar1menor7       = apgar1min_7==0

	*Hospitals with many babies (more than percentile 75)
	sum     h_born, det
	gen     dummy_Nbabies   = (h_born> r(p75))
	gen 	unhealthy       =1-(bpn==0&nacer_menos_37==0&apgar1menor7==0)
	
	global 	covars  unhealthy bpn nacer_menos_37        ///
                    apgar1menor7 h_prop_numconsul4      /// 
                    h_prop_babyfemale h_prop_basicedu   ///
                    h_prop_married h_prop_adolescent    /// 
                    tot_hosp c_population
					
	global 	S: word count $covars
		
	mat vlrp_b1 = J($S ,2,.)

	local c = 1
	foreach var of global covars{
		
		reghdfe `var' ave_prom_health, abs(p_state p_cohorte) cluster(codigo) 

		mat vlrp_b1[`c',1] = _b[ave_prom_health] 
		mat vlrp_b1[`c',2] = _se[ave_prom_health]
        
		local c = `c'+1	
        
	}
		
	mat rown vlrp_b1 = $covars
	mat list vlrp_b1
	
	putexcel set "Results/Tables paper matrices replication.xls", sheet("Raw Table 2") modify
	putexcel B2=mat(vlrp_b1)
	
    
log close