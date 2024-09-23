
/*
Project SSO-Quality

This do-file creates:
Table A18 	


*/
	
clear 		all 
set 		more off


cap log close
log using "Logs/Table A18.smcl", replace

********************************************************************************

*tiempo de exposicion del niño a un medico de sso (a nivel cohorte)

	use 		"Data\base_hospital_cohorte_bebe", replace
		
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
	global      c_h_cn   h_prop_v_nacer_38_41 h_prop_low_weight h_prop_apgar1_mas7
	global      outcomes bpn nacer_menos_37 apgar1menor7
	
	

	* Crear base con una sola observacion por bebe 
	gen 	days_coinc=min(p_fechfi_obs_ave,fechanto)-max(v_fconcep,p_fechin_obs_ave)+1
	*hist 	days_coinc,w(7)
	gen 	w_doc=round(days_coinc)*n_docs
	egen 	id_h=group(codigo12)

	egen 	p_drawm=min(p_draw),by(id_nacido)
	gen days_ges=fechanto-v_fconcep
	gen por_exp=days_coinc*100/days_ges
	gen dummy_exp60= (por_exp>=60)
	gen dummy_exp75= (por_exp>=75)
	gen dummy_exp90= (por_exp>=90)
	gen dummy_exp100= (por_exp>=100)
	
	gen coincidencia_final=1 if v_fconcep< p_fechin_obs_ave & dummy_exp100==0
	replace coincidencia_final=0 if v_fconcep>p_fechin_obs_ave & dummy_exp100==0
	
		
	/* Table */
	
	global p_res "d_area_metro==0"
	global res1     $c_b $c_m $c_cd $c_h       if  s1==1 & $p_res , abs(p_drawm) cluster(id_h)
    global res1_cn  $c_b $c_m $c_cd $c_h_cn    if  s1==1 & $p_res , abs(p_drawm) cluster(id_h)
	global res2  					           if s1==1 & $p_res , abs(p_drawm) cluster(id_h)

	*Estandarizar los puntajes
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
	
		
	foreach condi in dummy_exp100 {
				
	foreach var in unhealthy  $outcomes {
			
		di in red "*** `condi' ***"
		
		foreach x in ave_prom_health {
 
		di in red "*** `condi'==1 ***"	
		
		reghdfe `var' `x' if  `condi'==1 & $p_res , abs(p_drawm) cluster(id_h)
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
		
		
		di in red "*** `condi'==0 ***"
		
		reghdfe `var' `x' if  `condi'==0 & $p_res , abs(p_drawm) cluster(id_h)
		replace s1=(e(sample)==1)
			
		reghdfe `var' `x' $res1
		mat 	table1[4,`c'+1]  = _b[`x']
		mat 	table1[5 ,`c'+1] = _se[`x']
		
		reghdfe `var' `x' $res2
		mat 	table1[1,`c'+1]  = _b[`x']
		mat 	table1[2,`c'+1] = _se[`x']
		
		qui tab codigo_tot if e(sample)==1
		mat table1[8,`c'+1] = e(N) // obs
						
		sum `var' if e(sample)==1
		mat table1[7,`c'+1]  = r(mean)
		mat table1[3,`c'+1] = table1[1,`c'+1]/table1[7,`c'+1]
		mat table1[6,`c'+1] = table1[4,`c'+1]/table1[7,`c'+1]
		
		local c= `c'+2
		
		}
	
	}
	
	}
	
	mat list table1 
	putexcel set "Results\Tables paper matrices replication.xls", sheet("Raw Table A18") modify
	putexcel B2=mat(table1)
	
	
log close