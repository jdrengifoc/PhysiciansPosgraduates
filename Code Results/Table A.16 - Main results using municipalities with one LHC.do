	/*
	
	Replication package
	Table A.16.  Main results using municipalities with one LHC
	Last modified: 02/10/2023 
	
	*/

clear 		all 
set 		more off


cap log close
log using "Logs/Table A15.smcl", replace

********************************************************************************

	use 		"Data\base_hospital_cohorte_bebe", replace	

	merge 		m:1 codmundep using "Data\Anonimizada\Municipios_1_hospital", keep(3) nogen

	keep if nvals==1
	keep 		if p_emb_obs_ave==1 
	keep        if d_area_metro ==0

	pca 	ave_ecaes_med_administracion ave_ecaes_med_spublica, com(1)
	predict comp1_health
	 
	
	
	*Creating hospital covars (bw and prematurity 3 years before)
	egen tag               =  tag(codigo10 codigo12)		
	egen p75_bw            =  pctile(h_prop_low_weight)   if tag==1, p(75)
	egen p75_bw1		   =  max(p75_bw)
	egen p75_prem          =  pctile(h_prop_v_nacer_38_41) if tag==1,p(75)
	egen p75_prem1		   =  max(p75_prem)
	egen p75_apgar          =  pctile(h_prop_apgar1_mas7) if tag==1,p(75)
	egen p75_apgar1		   =  max(p75_apgar)
	
	gen dummy_prematurotop  = (h_prop_v_nacer_38_41<p75_prem1  & h_prop_v_nacer_38_41!=.)
	gen dummy_bwtop         = (h_prop_low_weight>p75_bw1 & h_prop_low_weight!=.)
	gen dummy_apgartop         = (h_prop_apgar1_mas7>p75_apgar1 & h_prop_apgar1_mas7!=.)

	drop p75_*
	
	gen  dummy_1hosp = (tot_hosp==1)
	gen  apgar1menor7 = apgar1min_7==0
	egen codigo_tot= group(codigo12 codigo10)	
	
	sum pop100, det
	
	gen pop=(pop100>=r(p50))
	
		
	global 		c_b      babyfemale adolescent basicedu married 
	global 		c_m      pop100 dummy_1hosp i.area 
	global		c_cd     dummy_female 
	global      c_h      dummy_bwtop dummy_prematurotop dummy_apgartop
	global      outcomes bpn nacer_menos_37 apgar1menor7
	
	

	gen 	days_coinc=min(p_fechfi_obs_ave,fechanto)-max(v_fconcep,p_fechin_obs_ave)+1
	gen 	w_doc=round(days_coinc)*n_docs
	egen 	id_h=group(codigo12)

	egen 	p_drawm=min(p_draw),by(id_nacido)
	
	collapse (mean) ave_prom_4 comp1_health ave_comp1 ave_prom_health ave_ecaes_med_administracion ave_ecaes_med_spublica ave_ecaes_med_reading ave_ecaes_med_quantitative (max) dummy_female dummy_top dummy_public [fw=w_doc],by($c_b pop100 p_drawm dummy_1hosp area $c_h $outcomes embarazos_madre d_area_metro id_nacido id_h year codigo_tot)
		
	/* Table */
	
	global p_res "d_area_metro==0"
	global res1  $c_b $c_m $c_cd $c_h     if         $p_res , abs(p_drawm) cluster(id_h)
	global res2  					      if s1==1 & $p_res , abs(p_drawm) cluster(id_h)

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
	
	gen 	unhealthy=1-(bpn==0&nacer_menos_37==0&apgar1menor7==0)

	
	gen 	s1=0

	
	mat 	table1 = J(8,8,.)
	
	local c=1
	foreach var in unhealthy $outcomes  {
		
		foreach x in ave_prom_health comp1_health {

		reghdfe `var' `x' if $p_res , abs(p_drawm) cluster(id_h)
		replace s1=(e(sample)==1)
				
		reghdfe `var' `x' $res1
		mat 	table1[4,`c']  = _b[`x']
		mat 	table1[5 ,`c'] = _se[`x']
		
		reghdfe `var' `x' $res2
		mat 	table1[1,`c']  = _b[`x']
		mat 	table1[2,`c'] = _se[`x']
				
		qui tab codigo_tot if e(sample)==1	
		mat table1[8,`c'] = e(N) // obs
						
		sum `var' if e(sample)==1
		mat table1[7,`c']  = r(mean)
		
		mat table1[3,`c'] = table1[1,`c']/table1[7,`c']
		mat table1[6,`c'] = table1[4,`c']/table1[7,`c']		
		
		local c= `c'+1
		
		}
	
	}
	
	mat list table1 	
	putexcel set "Results\Tables paper matrices replication.xls", sheet("Raw Table A16") modify
	putexcel B2=mat(table1)

	
log close 
	