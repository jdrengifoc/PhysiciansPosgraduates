	/*
	
	Replication package
	Table 6. Physicians' observables and their relative value added
	Last modified: 02/10/2023 
	
	*/
	
clear all 
set more off


cap log close
log using "Logs/Table 6.smcl", replace


********************************************************************************
	
global      outcomes bpn nacer_menos_37 apgar1menor7 unhealthy
global 		controles area babyfemale adolescent basicedu married pop100
global 		controles1 h_prop_low_weight h_prop_apgar1_mas7 h_prop_v_nacer_38_41  

********************************************************************************

    *	Creación base resumida para calcular Formas reducidas y VA
    use 	"Data/base_hospital_cohorte_bebe", replace

    egen 	codigo_tot= group(codigo12 codigo10)	


    keep 	if p_emb_obs_ave==1 
    keep    if d_area_metro ==0
    global 	cohorte  percent_*
        
    keep 	id_cohorte id_nacido p_state p_draw  codigo_tot $cohorte
    sort    id_cohorte
    egen 	id_cohorte2=group(id_cohorte)

    collapse (firstnm) percent_*, by(id_cohorte2)

    save    "Data/Percent_cohorte_10232023.dta", replace

    * Merge VA
    use 	"Data/base_hospital_cohorte_bebe", replace
    merge   1:1 id_cohorte id_nacido using "Data\Anonimizada\VA_vars", nogen

    keep 		if p_emb_obs_ave==1 
    keep        if d_area_metro ==0
    pca 		ave_ecaes_med_administracion ave_ecaes_med_spublica, com(1)
    predict 	comp1_health
     

    gen  		apgar1menor7    = apgar1min_7==0
    egen 		codigo_tot      = group(codigo12 codigo10)	

	*Creating hospital covars (bw and prematurity 3 years before)
	egen tag               =  tag(codigo10 codigo12)		
	egen p75_bw            =  pctile(h_prop_low_weight)   if tag==1, p(75)
	egen p75_bw1		   =  max(p75_bw)
	egen p75_prem          =  pctile(h_prop_v_nacer_38_41) if tag==1,p(75)
	egen p75_prem1		   =  max(p75_prem)
	egen p75_apgar         =  pctile(h_prop_apgar1_mas7) if tag==1,p(75)
	egen p75_apgar1		   =  max(p75_apgar)
	
	gen dummy_prematurotop  = (h_prop_v_nacer_38_41<p75_prem1  & h_prop_v_nacer_38_41!=.)
	gen dummy_bwtop         = (h_prop_low_weight>p75_bw1 & h_prop_low_weight!=.)
	gen dummy_apgartop      = (h_prop_apgar1_mas7>p75_apgar1 & h_prop_apgar1_mas7!=.)

	drop p75_*

	gen  dummy_1hosp = (tot_hosp==1)

	
	global 		c_b      babyfemale adolescent basicedu married 
	global 		c_m      pop100 dummy_1hosp area 
	global		c_cd     dummy_female 
	global      c_h      dummy_bwtop dummy_prematurotop dummy_apgartop
	global      c_h_cn   h_prop_v_nacer_38_41 h_prop_low_weight h_prop_apgar1_mas7 h_prop_unhealthy
	global 		cohorte  percent_*
	
    keep id_nacido p_state p_draw  codigo_tot apgar1menor7 bpn nacer_menos_37 fechanto p_fechfi_obs_ave p_fechin_obs_ave  v_fconcep ave_prom_4 ave_comp1 ave_prom_health ave_comp1_health ave_prom_acad ave_comp1_acad n_docs id_cohorte $c_b $c_m $c_cd $c_h $c_h_cn $cohorte p_emb_obs_ave d_area_metro

    gen     unhealthy=1-(bpn==0&nacer_menos_37==0&apgar1menor7==0)

    save    "Data/Base_intermedia_value_added.dta", replace

    * Provisional
    use 	"Data/Base_intermedia_value_added.dta", replace
    egen 	id_cohorte2=group(id_cohorte)

    *Value added discounting other cohorts       
    keep	id_nacido id_cohorte2 $outcomes
    tab 	id_cohorte2,gen(fe_cohort)
    collapse (max) fe_cohort*,by(id_nacido $outcomes)
    save 	"Data/provisional_coh_20220526",replace

    * Merge
    use 	"Data/Base_intermedia_value_added.dta", replace
    keep	id_nacido $controles $controles1
    duplicates drop
    merge 	1:1 id_nacido using "Data/provisional_coh_20220526"

    foreach x in $outcomes{	
    reg 	`x' fe_cohort*
    mat 	`x' =e(b)'
    mat 	`x'=`x'[1..1261,1] 
    }
    mat 	va=[bpn,nacer_menos_37,apgar1menor7,unhealthy]
        
    foreach x in $outcomes{	
    reg 	`x' fe_cohort* $controles $controles1
    mat 	`x' =e(b)'
    mat 	`x'=`x'[1..1261,1] 
    }
    mat 	va=[va,bpn,nacer_menos_37,apgar1menor7,unhealthy]


            
    clear
    svmat 	va
    ren 	(va*) (bpn nacer_menos_37 apgar1menor7 unhealthy bpn_con nacer_menos_37_con apgar1menor7_con unhealthy_con) 
    ren 	* va_*
    gen 	id_cohorte2=_n
    save 	"Data/va_coh_20220526",replace


	
	********************
	* TABLA
		
	use 	"Data/Base_intermedia_value_added.dta", replace
	keep 		if p_emb_obs_ave==1 
	keep        if d_area_metro ==0	
	egen 	id_cohorte2=group(id_cohorte)
	* A nivel de cohorte
	egen 	n_bebes=tag(id_nacido)
	replace ave_prom_4=(ave_prom_4-10.50637)/0.7386106          // 10.50637    .7386106 
	areg 	ave_prom_4, abs(p_draw)
	predict desv_prom4, r
	
	replace ave_prom_health=ave_prom_health-10.41811/0.7998619  // 10.41811    .7998619 
	areg 	ave_prom_health, abs(p_draw)
	predict desv_promH, r
	
	collapse (sum)n_bebes,by(id_cohorte2 p_draw ave_prom_4 desv_prom4 ave_prom_health desv_promH codigo_tot percent_* )
	
	merge 1:1 id_cohorte2 using "Data/Anonimizada/Percent_cohorte_10232023.dta"
	
	
	areg	ave_prom_4,a(p_draw)
	predict desv2_prom4,r
	areg	ave_prom_health,a(p_draw)
	predict desv2_promH,r
	egen 	id_h=group(codigo_tot)
	
	
	foreach var of varlist percent_* {
	areg	`var',a(p_draw)
	predict desv_`var', r
	}
	
	global physicians	desv_percent_female desv_percent_father desv_percent_mother desv_percent_work
	global program 		desv_percent_top desv_percent_top_income desv_percent_public desv_percent_acred
	
	merge 	1:1 id_cohorte2 using "Data/va_coh_20220526",nogen 

	mat 	table1 = J(18,4,.)


	local c=1
	foreach var in va_unhealthy va_unhealthy_con {
	areg  `var' desv_promH if n_bebes>=4, abs(p_draw) cluster(id_h)
	
	mat 	table1[1,`c']  = _b[desv_promH]
	mat 	table1[2,`c']  = _se[desv_promH]
		
	
	areg  `var' desv_promH $physicians $program if n_bebes>=4,abs(p_draw) cluster(id_h)
	
	local r = 1
	foreach x in desv_promH $physicians $program {
		mat 	table1[`r',`c'+1]  		=  _b[`x']
		mat 	table1[`r'+1,`c'+1]  	= _se[`x']
		
		local r = `r'+2
	}
		
	local c=`c'+2
	
	}
	
	mat list table1 
	putexcel set "Results\Tables paper matrices replication.xls", sheet("Raw Table 6") modify
	putexcel B2=mat(table1)	
	

	log close
