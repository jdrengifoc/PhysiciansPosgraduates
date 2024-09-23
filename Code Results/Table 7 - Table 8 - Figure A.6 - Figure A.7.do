	/*
	
	Replication package
	Table 7 - Table 8 - Figure A.6 - Figure A.7 - Results by predicted probability of unhealthy
	Last modified: 02/10/2023 
	
	*/
	
clear 		all 
set 		more off


cap log close
log using "Logs/Table 7 - Table 8 - Figure A6 - Figure A7.smcl", replace
	
********************************************************************************

	use 		"Data\base_hospital_cohorte_bebe", replace

	keep 		if p_emb_obs_ave==1 
	keep        if d_area_metro ==0

	pca 	ave_ecaes_med_administracion ave_ecaes_med_spublica, com(1)
	predict comp1_health
	 
	cap drop tag median_bw median_bweight median_prem median_prematuro dummy_prematuro dummy_bw p75_bw p75_bweight p75_prem p75_prematuro dummy_prematuro75 dummy_bw75
	
	*Creating hospital covars (bw and prematurity 3 years before)
	egen tag               =  tag(codigo10 codigo12)		
	egen p75_bw            =  pctile(h_prop_low_weight)   if tag==1, p(75)
	egen p75_bw1		   =  max(p75_bw)
	egen p75_prem          =  pctile(h_prop_v_nacer_38_41) if tag==1,p(75)
	egen p75_prem1		   =  max(p75_prem)
	egen p75_apgar         =  pctile(h_prop_apgar1_mas7) if tag==1,p(75)
	egen p75_apgar1		   =  max(p75_apgar)
	egen p75_unhealthy 	   = pctile(h_prop_unhealthy) if tag==1, p(75)
	
	gen dummy_prematurotop  = (h_prop_v_nacer_38_41<p75_prem1  & h_prop_v_nacer_38_41!=.)
	gen dummy_bwtop         = (h_prop_low_weight>p75_bw1 & h_prop_low_weight!=.)
	gen dummy_apgartop      = (h_prop_apgar1_mas7>p75_apgar1 & h_prop_apgar1_mas7!=.)
	gen dummy_unhealthy     = (h_prop_unhealthy>p75_unhealthy & h_prop_unhealthy!=.)

	
	drop p75_*
	
	gen  dummy_1hosp = (tot_hosp==1)
	gen  apgar1menor7 = apgar1min_7==0
	egen codigo_tot= group(codigo12 codigo10)	
	
			
	global 		c_b      babyfemale adolescent basicedu married 
	global 		c_m      pop100 dummy_1hosp i.area 
	global		c_cd     dummy_female 
	global      c_h      dummy_bwtop dummy_prematurotop dummy_apgartop dummy_unhealthy
	global      outcomes bpn nacer_menos_37 apgar1menor7
	
	
	egen 	n_bebes=tag(id_nacido)

	* Crear base con una sola observacion por bebe 
	gen 	days_coinc=min(p_fechfi_obs_ave,fechanto)-max(v_fconcep,p_fechin_obs_ave)+1
	gen 	w_doc=round(days_coinc)*n_docs
	egen 	id_h=group(codigo12)
	egen 	p_drawm=min(p_draw),by(id_nacido)
    
	collapse (mean) ave_prom_4 comp1_health ave_comp1 ave_prom_health ave_ecaes_med_administracion ave_ecaes_med_spublica ave_ecaes_med_reading ave_ecaes_med_quantitative (max) dummy_female dummy_top dummy_public [fw=w_doc],by($c_b pop100 p_drawm dummy_1hosp area $c_h $outcomes embarazos_madre d_area_metro id_nacido id_h year codigo_tot numconsul4 n_bebes w_doc h_prop_unhealthy)
		
	/* Table */
	
	global p_res "d_area_metro==0"
	global res1  $c_b $c_m $c_cd $c_h       if         $p_res , abs(year p_drawm) cluster(id_h)
	global res2  					   if s1==1 & $p_res , abs(year p_drawm) cluster(id_h)

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
	
	

	mat 	table1 = J(10,18,.)
	
	gen 	unhealthy=1-(bpn==0&nacer_menos_37==0&apgar1menor7==0)
	
	egen tag_h = tag(codigo_tot)
		
	gen 	con_b4=1-numconsul4 	
	
	tab area, gen(area_)
	gen embarazo_unico=(embarazos_madre==1)
	
	global 		c_b      babyfemale adolescent basicedu married 
	global 		c_m      pop100 dummy_1hosp i.area 
	global		c_cd     dummy_female 
	global      c_h      h_prop_unhealthy
	global      outcomes bpn nacer_menos_37 apgar1menor7
	
	
		
	local	B=500
	set 	matsize `B'
	mat 	logit=J(`B',20,.)
	mat 	rf=J(`B',20,.)
	
	
	forvalues b=1/`B'{	
	dis "`b'"
	set seed `b'
	cap  drop  k_ k unhealthy_logit  d_unhealthy_logit  unhealthy_rf  d_unhealthy_rf
	gen  k_= int(runiform()*4)+1 if tag_h==1
	egen k=mean(k_),by(codigo_tot)
	tab  k
	gen 	unhealthy_logit=.
	gen 	unhealthy_rf=.
		
	forvalues k=1/4{
	logit   unhealthy $c_b $c_cd $c_h pop100 area_1 area_2 area_3 dummy_1hosp embarazo_unico n_bebes w_doc  if k!=`k'
	predict r, pr
	replace unhealthy_logit=r if k==`k'	
	drop	r
	
	rforest unhealthy $c_b $c_cd $c_h pop100 area_1 area_2 area_3 dummy_1hosp  embarazo_unico n_bebes w_doc  if k!=`k', type(reg) depth(3) iterations(50)
	predict r
	replace unhealthy_rf=r if k==`k'
	drop	r	
	}
	
	sum      unhealthy_logit, d
	gen		d_unhealthy_logit = (unhealthy_logit>=r(p75))

	sum      unhealthy_rf, d	
	gen		d_unhealthy_rf = (unhealthy_rf>=r(p75))
		
	
	reghdfe con_b4 ave_prom_health       if d_unhealthy_logit==0, abs(year p_drawm) cluster(id_h)
	mat 	logit[`b',1]  = _b[ave_prom_health]
	mat 	logit[`b',2] = _se[ave_prom_health]
	
			
	reghdfe con_b4 ave_prom_health    if d_unhealthy_logit==1, abs(year p_drawm) cluster(id_h)
	mat 	logit[`b',3]  = _b[ave_prom_health]
	mat 	logit[`b',4] = _se[ave_prom_health]
	
	
	reghdfe con_b4 ave_prom_health  $c_b $c_m $c_cd $c_h if d_unhealthy_logit==0, abs(year p_drawm) cluster(id_h)
	mat 	logit[`b',5]  = _b[ave_prom_health]
	mat 	logit[`b',6] = _se[ave_prom_health]
	
	reghdfe con_b4 ave_prom_health $c_b $c_m $c_cd $c_h if d_unhealthy_logit==1, abs(year p_drawm) cluster(id_h)
	mat 	logit[`b',7]  = _b[ave_prom_health]
	mat 	logit[`b',8] = _se[ave_prom_health]

	sum con_b4 if d_unhealthy_logit==0
	mat logit[`b',9]  = r(mean)

	sum con_b4 if d_unhealthy_logit==1
	mat logit[`b',10]  = r(mean)
	
	reghdfe unhealthy ave_prom_health       if d_unhealthy_logit==0, abs(year p_drawm) cluster(id_h)
	mat 	logit[`b',11]  = _b[ave_prom_health]
	mat 	logit[`b',12] = _se[ave_prom_health]
	

	reghdfe unhealthy ave_prom_health       if d_unhealthy_logit==1, abs(year p_drawm) cluster(id_h)
	mat 	logit[`b',13]  = _b[ave_prom_health]
	mat 	logit[`b',14] = _se[ave_prom_health]
	

	reghdfe unhealthy ave_prom_health   $c_b $c_m $c_cd $c_h    if d_unhealthy_logit==0, abs(year p_drawm) cluster(id_h)
	mat 	logit[`b',15]  = _b[ave_prom_health]
	mat 	logit[`b',16] = _se[ave_prom_health]
	

	reghdfe unhealthy ave_prom_health   $c_b $c_m $c_cd $c_h    if d_unhealthy_logit==1, abs(year p_drawm) cluster(id_h)
	mat 	logit[`b',17]  = _b[ave_prom_health]
	mat 	logit[`b',18] = _se[ave_prom_health]
	
	sum unhealthy if d_unhealthy_logit==0
	mat logit[`b',19]  = r(mean)

	sum unhealthy if d_unhealthy_logit==1
	mat logit[`b',20]  = r(mean)
	

	* Random Forest
		reghdfe con_b4 ave_prom_health       if d_unhealthy_rf==0, abs(year p_drawm) cluster(id_h)
	mat 	rf[`b',1]  = _b[ave_prom_health]
	mat 	rf[`b',2] = _se[ave_prom_health]
	
			
	reghdfe con_b4 ave_prom_health    if d_unhealthy_rf==1, abs(year p_drawm) cluster(id_h)
	mat 	rf[`b',3]  = _b[ave_prom_health]
	mat 	rf[`b',4] = _se[ave_prom_health]
	
	
	reghdfe con_b4 ave_prom_health  $c_b $c_m $c_cd $c_h if d_unhealthy_rf==0, abs(year p_drawm) cluster(id_h)
	mat 	rf[`b',5]  = _b[ave_prom_health]
	mat 	rf[`b',6] = _se[ave_prom_health]
	
	reghdfe con_b4 ave_prom_health $c_b $c_m $c_cd $c_h if d_unhealthy_rf==1, abs(year p_drawm) cluster(id_h)
	mat 	rf[`b',7]  = _b[ave_prom_health]
	mat 	rf[`b',8] = _se[ave_prom_health]

	sum con_b4 if d_unhealthy_rf==0
	mat rf[`b',9]  = r(mean)

	sum con_b4 if d_unhealthy_rf==1
	mat rf[`b',10]  = r(mean)
	
	reghdfe unhealthy ave_prom_health       if d_unhealthy_rf==0, abs(year p_drawm) cluster(id_h)
	mat 	rf[`b',11]  = _b[ave_prom_health]
	mat 	rf[`b',12] = _se[ave_prom_health]
	

	reghdfe unhealthy ave_prom_health       if d_unhealthy_rf==1, abs(year p_drawm) cluster(id_h)
	mat 	rf[`b',13]  = _b[ave_prom_health]
	mat 	rf[`b',14] = _se[ave_prom_health]
	

	reghdfe unhealthy ave_prom_health   $c_b $c_m $c_cd $c_h    if d_unhealthy_rf==0, abs(year p_drawm) cluster(id_h)
	mat 	rf[`b',15]  = _b[ave_prom_health]
	mat 	rf[`b',16] = _se[ave_prom_health]
	

	reghdfe unhealthy ave_prom_health   $c_b $c_m $c_cd $c_h    if d_unhealthy_rf==1, abs(year p_drawm) cluster(id_h)
	mat 	rf[`b',17]  = _b[ave_prom_health]
	mat 	rf[`b',18] = _se[ave_prom_health]
	
	sum unhealthy if d_unhealthy_rf==0
	mat rf[`b',19]  = r(mean)

	sum unhealthy if d_unhealthy_rf==1
	mat rf[`b',20]  = r(mean)
	
	
	}
	
	
	clear 
	svmat logit
	svmat rf
    
    save "Data\logit_rf_estimations", replace
	

use "Data\logit_rf_estimations", replace
mat 	table1 = J(6,4,.)
	
	local i=1
	
foreach var in logit rf {
sum `var'1 	
local j=r(mean)
mat table1[`i',1]=r(mean)


sum `var'2
mat table1[`i'+1,1]=r(mean)


sum `var'9
mat table1[`i'+2,1]=r(mean)

mat table1[`i'+2,1]=table1[`i',1]/table1[`i'+2,1]


sum `var'5 	
local j=r(mean)

mat table1[`i',2]=r(mean)

sum `var'6
mat table1[`i'+1,2]=r(mean)

sum `var'9
mat table1[`i'+2,2]=r(mean)

mat table1[`i'+2,2]=table1[`i',2]/table1[`i'+2,2]




sum `var'3 	
local j=r(mean)
mat table1[`i',3]=r(mean)

sum `var'4
mat table1[`i'+1,3]=r(mean)


sum `var'10 
mat table1[`i'+2,3]=r(mean)

mat table1[`i'+2,3]=table1[`i',3]/table1[`i'+2,3]



sum `var'7 	
local j=r(mean)
mat table1[`i',4]=r(mean)

sum `var'8
mat table1[`i'+1,4]=r(mean)


sum `var'10 
mat table1[`i'+2,4]=r(mean)

mat table1[`i'+2,4]=table1[`i',4]/table1[`i'+2,4]

local i=`i'+3
}

	mat list table1 
	putexcel set "Results\Tables paper matrices replication.xls", sheet("Raw Table 7") modify
	putexcel B2=mat(table1)
	
	
	
	

mat 	table1 = J(6,4,.)
	
	local i=1
	
foreach var in logit rf {
sum `var'11 	
local j=r(mean)
mat table1[`i',1]=r(mean)


sum `var'12
mat table1[`i'+1,1]=r(mean)


sum `var'19
mat table1[`i'+2,1]=r(mean)

mat table1[`i'+2,1]=table1[`i',1]/table1[`i'+2,1]


sum `var'15 	
local j=r(mean)

mat table1[`i',2]=r(mean)

sum `var'16
mat table1[`i'+1,2]=r(mean)

sum `var'19
mat table1[`i'+2,2]=r(mean)

mat table1[`i'+2,2]=table1[`i',2]/table1[`i'+2,2]




sum `var'13 	
local j=r(mean)
mat table1[`i',3]=r(mean)

sum `var'14
mat table1[`i'+1,3]=r(mean)


sum `var'20 
mat table1[`i'+2,3]=r(mean)

mat table1[`i'+2,3]=table1[`i',3]/table1[`i'+2,3]



sum `var'17 	
local j=r(mean)
mat table1[`i',4]=r(mean)

sum `var'18
mat table1[`i'+1,4]=r(mean)


sum `var'20 
mat table1[`i'+2,4]=r(mean)

mat table1[`i'+2,4]=table1[`i',4]/table1[`i'+2,4]

local i=`i'+3
}

	mat list table1 	
	putexcel set "Results\Tables paper matrices replication.xls", sheet("Raw Table 8") modify
	putexcel B2=mat(table1)
    
    
    
    /* Figures */
    
	gen 		n=_n
	gen 		zero=0
	sort 		n
	
	forvalues 	v=1(2)20{
        
        kdensity 	logit`v',g(v`v'_x v`v'_y)
        line 		v`v'_y v`v'_x
        sort 		logit`v'
        *global		lb`v'= logit`v' in 125
        *global		ub`v'= logit`v' in 4875
        global		lb`v'= logit`v' in 12
        global		ub`v'= logit`v' in 488
        sort 		n
    
	}
    	
	* Antenatal
	tw	(rarea v1_y zero v1_x if inrange(v1_x,${lb1},${ub1}),fc(black*0.2)lc(black*0.2)lw(thin))        ///
		(rarea v3_y zero v3_x if inrange(v3_x,${lb3},${ub3}),fc(blue*0.2)lc(blue*0.2)lw(thin))          ///
		(line v1_y v1_x,lc(black)lw(thin))                                                              ///
		(line v3_y v3_x,lc(blue)lw(thin)),                                                              ///
		graphr(c(white)) ylabel(none) yscale(off) xlabel(#10)                                           ///
        text(500 -0.01 "Higher P(Unhealthy)",c(blue))                                                   ///
		text(500 -0.0045 "Lower P(Unhealthy)",c(black)) legend(off)
        
	graph export "Results/Figure A6.png",replace wid(1446) hei(1050)
	
    * Unhealthy
    tw	(rarea v11_y zero v11_x if inrange(v11_x,${lb11},${ub11}),fc(black*0.2)lc(black*0.2)lw(thin))   ///
		(rarea v13_y zero v13_x if inrange(v13_x,${lb13},${ub13}),fc(blue*0.2)lc(blue*0.2)lw(thin))     ///
		(line v11_y v11_x,lc(black)lw(thin))                                                            ///
		(line v13_y v13_x ,lc(blue)lw(thin)) ,                                                          ///
		graphr(c(white)) yscale(off) ylabel(none) xlabel(#10)                                           ///
		text(990 -0.0052 "Higher P(Unhealthy)",c(blue))                                                 ///
		text(990 -0.0035 "Lower P(Unhealthy)",c(black)) legend(off)
	
	graph export "Results/Figure A7.png",replace wid(1446) hei(1050)
	
	
log close