	/*
	
	Replication package
	FIGURE A.1.  Local polynomial regression
	Last modified: 02/10/2023 
	
	*/

    
clear 		all 
set 		more off
    
cap log close
log using "Logs/Figure A1.smcl", replace


********************************************************************************

	use 		"Data\base_hospital_cohorte_bebe", replace
	set 		scheme white_tableau
	
	/* Note: this database contains treated and placebo babies. For this exercise, 
	we use the placebo babies, babies born 3 years before the SSO period we are
	analyzing started (2010-2012). */
	
	keep 	if p_emb_n4_obs==1 
	keep    if d_area_metro==0	
	keep    if semanas_gestacion > 23 & semanas_gestacion < 44
	
	egen 	    p_drawm=mode(p_draw),by(id_nacido) minmode
	collapse    (mean) bpn, by(semanas_gestacion p_drawm codigo12)
	
	lpoly 	bpn semanas_gestacion, degree(0) bw(2) n(12) pw(1000)				///
			noscatter ci l(90) legend(off) scheme(modern)						///
			xtitle(Gestational Weeks) ytitle("") title("")						///
			xlab(24(4)44, nogrid) ylab(0(0.2)1) kernel(gau) yaxis(off) 			///
			scheme(white_tableau)
			
	graph export "Results/Figure A1.png", replace wid(1200) hei(1000)
    
	
log close