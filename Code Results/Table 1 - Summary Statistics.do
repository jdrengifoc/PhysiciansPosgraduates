	/*
	
	Replication package
	Table 1. Descriptive Vital Statistics Registers main sample 2013-2016
	
	*/

    
clear 		all 
set 		more off

cap log close
log using "Logs/Table 1.smcl", replace
	

********************************************************************************

	use 	"Data\base_hospital_cohorte_bebe", replace
		
	keep 	if p_emb_obs_ave==1 

	egen 	tag_id_nacido = tag(id_nacido)
	gen 	mbpn          = peso_nacido<=2 
	gen     apgar1menor7  = apgar1min_7==0

	keep    if tag_id_nacido==1

	gen 	unhealthy=1-(bpn==0&nacer_menos_37==0&apgar1menor7==0)

	replace numconsul4=1-numconsul4
	
	global 	vars bpn nacer_menos_37 apgar1menor7 unhealthy babyfemale numconsul4 adolescent

	mean 	$vars
	keep 	if e(sample)==1

	global 	N: word count $vars
	mat 	table2 = J($N+1 , 4,.)


	local 	r=1
	foreach x of global vars {
        
		sum `x'
		mat table2[`r',1] = r(mean)
		mat table2[`r',2] = r(sd)
		
		sum `x' if d_area_metro==0
		mat table2[`r',3] = r(mean)
		mat table2[`r',4] = r(sd)
		
		local r = `r'+1
        
	}

	count 
	mat table2[$N+1, 1] = r(N)

	count if d_area_metro==0
	mat table2[$N+1, 3] = r(N)

	mat list table2

	putexcel set "Results\Tables paper matrices replication.xls", sheet("Raw Table 1") modify
	putexcel B2=mat(table2)
    
	
log close