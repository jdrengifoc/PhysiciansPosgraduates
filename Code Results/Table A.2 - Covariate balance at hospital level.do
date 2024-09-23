	/*
	
	Project SSO-Quality
	Table A2 - Balancing hospitals
	Date created:  30/06/2020 
	Last modified: 04/09/2020 
	
	*/
	
clear 		all 
set 		more off


cap log close
log using "Logs/Table A2.smcl", replace

********************************************************************************

	use 	   "Data\Anonimizada\base_medico_rural_con_fecha_inicio",replace
	cap drop   p_date
	gen        p_date =ym(p_year,p_month)
	format     p_date %tm
	
	drop 	   codigo10
	des 	   ecaes_med_reading ecaes_med_quantitative  ecaes_med_administracion ecaes_med_spublica 
	count 	if ecaes_med_reading != . &  ecaes_med_quantitative != . & ecaes_med_administracion != . & ecaes_med_spublica != .

	preserve
	use "Data\base_hospital_cohorte_bebe", replace
	keep 	if p_emb_obs_ave==1 & d_area_metro==0 & embarazos_madre==1
	tempfile base1
	save `base1', replace 
	restore
	
	joinby  codigo12 p_date using `base1', unm(b) _merge(merge_pru) 

	keep 	if p_emb_obs_ave==1 & d_area_metro==0 & embarazos_madre==1
	ren 	codigo12 codigo

	
	collapse  (mean)  apgar1min_7  bpn nacer_menos_37 prom_4 h_prop_v_apgar1_masy7 h_prop_numconsul4 h_prop_babyfemale h_prop_basicedu   h_prop_married h_prop_adolescent tot_hosp c_population h_prop_low_weight h_prop_v_nacer_38_41 h_born, by(codigo p_state p_cohorte)

	gen 	p_mes_inicio=ym(2010,1)
	merge 	m:1 p_mes_inicio codigo using "Data\Anonimizada\SSO_Hospital_Vitales_GS", keep(3) nogen

	egen tag               = tag(codigo)

	gen  apgar1men7        = 1-h_prop_v_apgar1_masy7
	egen p75_apgar7        =  pctile(apgar1men7)          if tag==1, p(75)
	egen p75_apgar71       =  max(p75_apgar7)
	egen p75_bw            =  pctile(h_prop_low_weight)   if tag==1, p(75)
	egen p75_bw1		   =  max(p75_bw)
	egen p75_prem          =  pctile(h_prop_v_nacer_38_41) if tag==1,p(75)
	egen p75_prem1		   =  max(p75_prem)
	
	gen dummy_prematurotop  = (h_prop_v_nacer_38_41<p75_prem1   & h_prop_v_nacer_38_41!=.)
	gen dummy_bwtop         = (h_prop_low_weight   >p75_bw1     & h_prop_low_weight   !=.)
	gen dummy_apgartop      = (apgar1men7          >p75_apgar71 & apgar1men7          !=.)
	compress 
	gen  dummy_1hosp = (tot_hosp==1)
	gen  apgar1menor7 = apgar1min_7==0

	*Hospitals with many babies (more than percentile 75)
	sum     h_born, det
	gen     dummy_Nbabies = (h_born> r(p75))
	tab     dummy_Nbabies
	
	gen 	unhealthy=1-(bpn==0&nacer_menos_37==0&apgar1menor7==0)
		
	global 	covars unhealthy h_prop_low_weight h_prop_preterm ///
			apgar1menor7   h_prop_numconsul4 /// 
			h_prop_babyfemale h_prop_basicedu   ///
			h_prop_married h_prop_adolescent ///
			tot_hosp c_population
			
	
	global 	S: word count $covars
		
	mat vlrp_b1 = J($S ,2,.)

	local c = 1
	foreach var of global covars{

		areg `var' prom_4 i.p_cohorte , abs(p_state) cluster(codigo) 
		
		mat vlrp_b1[`c',1] = _b[prom_4] 
		mat vlrp_b1[`c',2] = _se[prom_4]
		
		local c = `c'+1	
	}
		

	mat list vlrp_b1

	putexcel set "Results\Tables paper matrices replication.xls", sheet("Raw Table A2") modify
	putexcel B2=mat(vlrp_b1)
	

log close
	