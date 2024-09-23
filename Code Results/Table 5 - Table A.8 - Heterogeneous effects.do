	/*
	
	Replication package
	Table 5. Heterogenenity of the effects across mother's characteristics
	Table A.7. Other heterogeneous effects
	Last modified: 02/10/2023 
	
	*/
	
clear 		all 
set 		more off


cap log close
log using "Logs/Table 5 - Table A8.smcl", replace	
	
********************************************************************************
	
	use 		"Data\base_hospital_cohorte_bebe", replace
	
	keep 		if p_emb_obs_ave==1 
	keep        if d_area_metro ==0
	
	gen edumadre=(nivel_madre<=3 | nivel_madre==13)

	pca 	ave_ecaes_med_administracion ave_ecaes_med_spublica, com(1)
	predict comp1_health
	 
	
	*Creating hospital covars (bw and prematurity 3 years before)
	egen tag               =  tag(codigo10 codigo12)		
	egen p75_bw            =  pctile(h_prop_low_weight)   if tag==1, p(75)
	egen p75_bw1		   =  max(p75_bw)
	egen p75_prem          =  pctile(h_prop_v_nacer_38_41) if tag==1,p(75)
	egen p75_prem1		   =  max(p75_prem)
	egen p75_unhealthy     =  pctile(h_prop_unhealthy)   if tag==1, p(50)
	egen p75_unhealthy1	   =  max(p75_unhealthy)
	egen p75_apgar         =  pctile(h_prop_apgar1_mas7) if tag==1,p(75)
	egen p75_apgar1		   =  max(p75_apgar)

	
	gen dummy_prematurotop  = (h_prop_v_nacer_38_41<p75_prem1  & h_prop_v_nacer_38_41!=.)
	gen dummy_bwtop         = (h_prop_low_weight   >p75_bw1    & h_prop_low_weight!=.)
	gen dummy_unhealthy     = (h_prop_unhealthy   >p75_unhealthy1    & h_prop_unhealthy!=.)
	gen dummy_apgartop      = (h_prop_apgar1_mas7>p75_apgar1 & h_prop_apgar1_mas7!=.)

	drop p75_*
	
	gen  dummy_1hosp = (tot_hosp==1)
	gen  apgar1menor7 = apgar1min_7==0
	egen codigo_tot= group(codigo12 codigo10)	
	
		
	global 		c_b      babyfemale adolescent basicedu married 
	global 		c_m      pop100 dummy_1hosp i.area 
	global		c_cd     dummy_female 
	global      c_h      dummy_bwtop dummy_prematurotop dummy_apgartop
	global      outcomes bpn nacer_menos_37 apgar1menor7
	
	

	gen 	days_coinc=min(p_fechfi_obs_ave,fechanto)-max(v_fconcep,p_fechin_obs_ave)+1
	gen 	w_doc=round(days_coinc)*n_docs
	egen 	id_h=group(codigo12)
	egen 	p_drawm=min(p_draw),by(id_nacido)
	
	gen 	full=p_fechfi_obs_ave>=fechanto & v_fconcep>=p_fechin_obs_ave
	egen 	Full=max(full),by(id_nacido)
	replace w_doc=0 if full==0 & Full==1
	
	collapse (mean) ave_prom_4 comp1_health ave_comp1 ave_prom_health ave_ecaes_med_administracion ave_ecaes_med_spublica ave_ecaes_med_reading ave_ecaes_med_quantitative (max) full dummy_female dummy_top dummy_public [fw=w_doc],by($c_b pop100 p_drawm edumadre dummy_1hosp area $c_h $outcomes dummy_unhealthy embarazos_madre d_area_metro id_nacido id_h year codigo_tot)
		
	/* Table */
	
	global p_res "d_area_metro==0"

	*Standardized scores
	egen 	tagc=tag(p_drawm codigo_tot)
	sum     ave_ecaes_med_administracion if tagc==1 & $p_res
	replace ave_ecaes_med_administracion=(ave_ecaes_med_administracion-r(mean))/r(sd)
	sum     ave_ecaes_med_spublica if tagc==1 & $p_res
	replace ave_ecaes_med_spublica=(ave_ecaes_med_spublica-r(mean))/r(sd)
	sum     ave_prom_health if tagc==1 & $p_res
	replace ave_prom_health=(ave_prom_health-r(mean))/r(sd)
	sum     comp1_health if tagc==1 & $p_res
	replace comp1_health=(comp1_health-r(mean))/r(sd)
	sum     ave_prom_4 if tagc==1 & $p_res
	replace ave_prom_4=(ave_prom_4-r(mean))/r(sd)
	
	gen first_time = (embarazos_madre==1)
	
	gen unhealthy=1-(bpn==0&nacer_menos_37==0&apgar1menor7==0)
	

	foreach var in unhealthy  {
	
		mat 	table1 = J(5,6,.)
	
		local c=1
		foreach condi in dummy_unhealthy first_time adolescent 	{
		di in red "*** `condi' ***"
		
		foreach x in ave_prom_health   {
		
		di in red "*** `condi'==1 ***"			
		
		reghdfe `var' `x'      if   `condi'==1 &       $p_res , abs(p_drawm) cluster(id_h)
		mat 	table1[1,`c']  = _b[`x']
		mat 	table1[2 ,`c'] = _se[`x']
		
		mat table1[5,`c'] = e(N) // obs
						
		sum `var' if e(sample)==1
		mat table1[4,`c']  = r(mean)
		mat table1[3,`c'] = table1[1,`c']/table1[4,`c']
		
		
		di in red "*** `condi'==0 ***"
			
		reghdfe `var' `x'       if   `condi'==0 &       $p_res , abs(p_drawm) cluster(id_h)
		mat 	table1[1,`c'+1]  = _b[`x']
		mat 	table1[2 ,`c'+1] = _se[`x']
			
		mat table1[5,`c'+1] = e(N) // obs
						
		sum `var' if e(sample)==1
		mat table1[4,`c'+1]  = r(mean)
		mat table1[3,`c'+1] = table1[1,`c'+1]/table1[4,`c'+1]
		
		local c= `c'+2
		
		}
	
	}
	
}
	
	mat lis table1
	putexcel set "Results\Tables paper matrices replication.xls", sheet("Raw Table 5") modify
	putexcel B2=mat(table1)
	
	
	foreach var in unhealthy  {
	
		mat 	table1 = J(8,6,.)
	
		local c=1
		foreach condi in edumadre married  babyfemale	{
		di in red "*** `condi' ***"
		
		foreach x in ave_prom_health   {
		
		di in red "*** `condi'==1 ***"			
		
		reghdfe `var' `x'      if   `condi'==1 &       $p_res , abs(p_drawm) cluster(id_h)
		mat 	table1[1,`c']  = _b[`x']
		mat 	table1[2 ,`c'] = _se[`x']
		
		reghdfe `var' `x' $c_b $c_m $c_cd  $c_h     if   `condi'==1 &       $p_res , abs(year p_drawm) cluster(id_h)
		mat 	table1[4,`c']  = _b[`x']
		mat 	table1[5 ,`c'] = _se[`x']
				
		mat table1[8,`c'] = e(N) // obs
						
		sum `var' if e(sample)==1
		mat table1[7,`c']  = r(mean)
		mat table1[3,`c'] = table1[1,`c']/table1[7,`c']
		mat table1[6,`c'] = table1[4,`c']/table1[7,`c']
		
		
		di in red "*** `condi'==0 ***"
			
		reghdfe `var' `x'       if   `condi'==0 &       $p_res , abs(p_drawm) cluster(id_h)
		mat 	table1[1,`c'+1]  = _b[`x']
		mat 	table1[2 ,`c'+1] = _se[`x']
		
		reghdfe `var' `x' $c_b $c_m $c_cd $c_h      if   `condi'==0 &       $p_res , abs(year p_drawm) cluster(id_h)
		mat 	table1[4,`c'+1]  = _b[`x']
		mat 	table1[5 ,`c'+1] = _se[`x']
				
		mat table1[8,`c'+1] = e(N) // obs
						
		sum `var' if e(sample)==1
		mat table1[7,`c'+1]  = r(mean)
		mat table1[3,`c'+1] = table1[1,`c'+1]/table1[7,`c'+1]
		mat table1[6,`c'+1] = table1[4,`c'+1]/table1[7,`c'+1]
		
		local c= `c'+2
		
		}
	
	}
	
}	
	
	mat lis table1
	putexcel set "Results\Tables paper matrices replication.xls", sheet("Raw Table A8") modify
	putexcel B2=mat(table1)
		
		
log close		
		