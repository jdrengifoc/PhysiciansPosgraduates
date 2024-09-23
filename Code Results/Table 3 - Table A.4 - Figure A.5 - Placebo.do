	/*
	
	Replication package
	Table 3. Placebo test
	Table A.4. Placebo robustness checks
	Figure A.5. Placebo using all samples and average scores
	
	*/
	
    
clear 		all 
set 		more off

cap log close
log using "Logs/Table 3 - Table A4 - Figure A5.smcl", replace

********************************************************************************

	use 		"Data\base_hospital_cohorte_bebe", replace

	/* Note: this database contains treated and placebo babies. For this exercise, 
	we use the placebo babies, babies born 3 years before the SSO period we are
	analyzing started (2010-2012). */
    
	keep 		if p_emb_n4_obs==1 
	keep        if d_area_metro==0
	
	pca 	    ave_ecaes_med_administracion ave_ecaes_med_spublica, com(1)
	predict     comp1_health
	
	*Creating hospital covars (bw and prematurity 3 years before)
	egen tag                = tag(codigo10 codigo12)		
	egen p75_bw             = pctile(h_prop_low_weight)   if tag==1, p(75)
	egen p75_bw1		    = max(p75_bw)
	egen p75_prem           = pctile(h_prop_v_nacer_38_41) if tag==1,p(75)
	egen p75_prem1		    = max(p75_prem)
	egen p75_apgar          = pctile(h_prop_apgar1_mas7) if tag==1,p(75)
	egen p75_apgar1		    = max(p75_apgar)
	
	gen dummy_prematurotop  = (h_prop_v_nacer_38_41<p75_prem1  & h_prop_v_nacer_38_41!=.)
	gen dummy_bwtop         = (h_prop_low_weight>p75_bw1 & h_prop_low_weight!=.)
	gen dummy_apgartop      = (h_prop_apgar1_mas7>p75_apgar1 & h_prop_apgar1_mas7!=.)

	drop p75_*
	
	gen  dummy_1hosp        = (tot_hosp==1)
	gen  apgar1menor7       = apgar1min_7==0
	egen codigo_tot         = group(codigo12 codigo10)	
	
	sum pop100, det
	gen pop=(pop100>=r(p50))
			
	global 		c_b         babyfemale adolescent basicedu married 
	global 		c_m         pop100 dummy_1hosp i.area 
	global		c_cd        dummy_female 
	global      c_h         dummy_bwtop dummy_prematurotop dummy_apgartop
	global      outcomes    bpn nacer_menos_37 apgar1menor7
	
	replace     p_fechfi_obs_ave=p_fechfi_obs_ave-(365*4)
	replace     p_fechin_obs_ave=p_fechin_obs_ave-(365*4)
	gen 	    days_coinc=min(p_fechfi_obs_ave,fechanto)-max(v_fconcep,p_fechin_obs_ave)+1
	gen 	    w_doc=round(days_coinc)*n_docs
	egen 	    id_h=group(codigo12)
	egen 	    p_drawm=min(p_draw),by(id_nacido)
    
	collapse (mean) ave_prom_4 comp1_health ave_comp1 ave_prom_health ave_ecaes_med_administracion          ///
                    ave_ecaes_med_spublica ave_ecaes_med_reading ave_ecaes_med_quantitative (max)           ///
                    dummy_female dummy_top dummy_public [fw=w_doc],by($c_b pop100 p_drawm dummy_1hosp       ///
                    area $c_h $outcomes embarazos_madre d_area_metro id_nacido id_h year codigo_tot)
	
	
	/* Table */
	
	global p_res "d_area_metro==0"
	global res1  $c_b $c_m $c_cd $c_h       if         $p_res , abs(p_drawm) cluster(id_h)
	global res2  					        if s1==1 & $p_res , abs(p_drawm) cluster(id_h)
	
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
	mat 	table1 = J(5,4,.)
	
	local c=1
	foreach var in unhealthy $outcomes  {
		
		foreach x in ave_prom_health    {

            reghdfe `var' `x' if $p_res , abs(p_drawm) cluster(id_h)
            replace s1=(e(sample)==1)
                
            reghdfe `var' `x' $res2
            mat 	table1[1,`c']  = _b[`x']
            mat 	table1[2,`c'] = _se[`x']
                
            mat table1[5,`c'] = e(N) // obs
                            
            sum `var' if e(sample)==1
            mat table1[4,`c'] = r(mean)
            mat table1[3,`c'] = table1[1,`c']/table1[4,`c']
            
            local c= `c'+1

		}
	
	}
	
	mat list table1 
	putexcel set "Results\Tables paper matrices replication.xls", sheet("Raw Table 3") modify
	putexcel B2=mat(table1)
		
	drop    s1	
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
	putexcel set "Results\Tables paper matrices replication.xls", sheet("Raw Table A4") modify
	putexcel B2=mat(table1)


	
    clear 
    svmat   table1
    drop	if _n == 3 | _n == 6
    drop	in l


    forvalues x=1/8{
    local	m=table1`x' in l
    replace table1`x'=(table1`x')/`m' in 1/4
    }

    drop	in l
    gen 	type=mod(_n,2)
    gen 	i=int((_n-1)/2)+1
    reshape long table1,i(i type)j(j)
    ren 	table1 v
    reshape wide v,i(i j)j(type)
    ren 	v1 coef
    gen lb = coef - v0*1.96
    gen ub = coef + v0*1.96
    drop	v0

    gen 	var=int((j-1)/2)+1
    gen 	controls=i==2
    gen 	ave=mod(j,2)==1

    gen 	type=1 if controls==1 &ave==1
    replace	type=2 if controls==0 &ave==1
    replace	type=3 if controls==1 &ave==0
    replace	type=4 if controls==0 &ave==0

    sort 	var	type
    gen	 	x=var+((type-2.5)/8)

    label 	def x 1 "Unhealthy" 2 "LBW" 3 "Prematurity" 4 "Low Apgar",replace
    label	val x x

    tw		(rcap 	lb ub x if type==1,lc(black)msize(zero)lw(medium))                  ///
            (sc		coef x if type==1,m(O) mfc(white) mlc(black) lw(medium))            ///
            (rcap 	lb ub x if type==2,lc(black)msize(zero)lw(medium))                  ///
            (sc		coef x if type==2,m(T) mfc(white) mlc(black) lw(medium))            ///
            (rcap 	lb ub x if type==3,lc(black)lp(-)msize(zero)lw(medium))             ///
            (sc		coef x if type==3,m(O) mfc(white) mlc(black) lw(medium))            ///
            (rcap 	lb ub x if type==4,lc(black)lp(-)msize(zero)lw(medium))             ///
            (sc		coef x if type==4,m(T) mfc(white) mlc(black) lw(medium))            ///
            ,legend(c(4) position(6) region(lc(white)) nobox                            ///
            order(2 "With Controls" 4 "W/o Controls" 1 "Average" 5 "Principal Comp."))  ///
            graphr(c(white)) xtitle("") xlabel(1(1)4,valuelabel)yline(0.1,lc(gs15))     ///
            yline(0,lc(red)lp(S)lw(thin)) ylabel(-0.2(0.1)0.1,angle(h))                 ///
            note("95% confidence intervals") 
                       
    graph export "Results\Figure A5.png",replace wid(1446) hei(1050)


log close