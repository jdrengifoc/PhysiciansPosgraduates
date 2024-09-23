/*
Replication Package
Step 2:
Create variables at the hospital level.
Merge with the dataset at the physician level.
Save this dataset as "base_hospital_cohorte_bebe."

 
*/


clear 		all
set 		more off

cap log close
log using "Logs/Data - Step 2.smcl", replace


	****************************************
	* 		Codigo hospitales 2013		   *
	****************************************
	use			codigo12 mpio_nacimiento fechanto semanas_gestacion using "Data\Anonimizada\1_VSR",replace
	drop 		if mi(semanas_gestacion)
	gen 		p_codmundep=string(mpio_nacimiento)
	replace 	p_codmundep="0"+p_codmundep if length(p_codmundep)==4
	duplicates 	drop
	gen 		codigo10=substr(codigo12,1,10)
	gen 		p_date=ym(2013,1)
	joinby 		codigo10 p_codmundep p_date using "Data\base_cohorte_rural_con_fecha_inicio.dta"
	gen 		v_fconcep=fechanto-7*semanas_gestacion
	format 		v_fconcep %td
	gen 		p_emb=inrange(v_fconcep,p_fechin_obs_ave,p_fechfi_obs_ave)|inrange(fechanto,p_fechin_obs_ave,p_fechfi_obs_ave) 
	keep 		if p_emb==1 
	keep		codigo12 codigo10 p_codmundep 
	duplicates 	drop
	duplicates 	tag codigo10, gen(dup10) 				
	merge 		1:m codigo12 p_codmundep using "Data\base_cohorte_rural_con_fecha_inicio.dta",keep(1 3) keepusing(codigo12) 
	duplicates 	drop
	drop 		if _merge==1 & dup10>0 													
	duplicates 	tag codigo10,gen(N)
	keep		if N==0
	gen 		p_date=ym(2013,1)
	ren 		codigo12 codigo12_v 
	merge 		1:m codigo10 p_codmundep p_date using "Data\base_cohorte_rural_con_fecha_inicio.dta", update replace gen(match2)
	replace		codigo12=codigo12_v if mi(codigo12)
	drop		_merge N dup10 match2 codigo12_v
	keep 		if !mi(codigo12)
	*keep 		if prom_4!=. // Eliminar puntajes no disponibles
	tempfile 	provisional
	save 		`provisional'

	************************************************
	*    Union datos todos los bebes vs Rural	   *
	************************************************
	use 		id_nacido fechanto codigo12 mpio_nacimiento semanas_gestacion using "Data\Anonimizada\1_VSR", replace
	drop 		if mi(semanas_gestacion)
	gen 		v_fconcep=fechanto-7*semanas_gestacion
	gen 		p_codmundep=string(mpio_nacimiento)
	replace 	p_codmundep="0"+p_codmundep if length(p_codmundep)==4
	joinby 		codigo12 p_codmundep using `provisional' 
	duplicates 	report id_nacido codigo12 p_fechin_obs_ave p_codmundep p_date
	merge 		m:1 id_nacido using "Data\Anonimizada\1_VSR",keep(1 3) nogen

	************************************************
	*       Pegar informacion del municipio 	   *
	************************************************
	ren 		p_codmundep codmundep	
	merge 		m:1 codmundep using "Data\Anonimizada\SSO_poblacion_tratados",keep(1 3) nogen
	gen 		cod_dane= mpio_nacimiento

	* Areas metropolitanas
	recode 		cod_dane (11001 25099 25799 25286 25473 25377 25175 25295 25785 25754 ///
				25899 25269 25126 25817 25214 25740 25430 = 1 "Bogotá Met") ///
				(5001 5129 5380 5088 5079 5266 5212 5308 5360 5631 = 2 "Medellín Met") ///
				(76001 76520 76364 76892 76130 = 3 "Cali met") ///
				(8001 8758 8573 8433 8296 = 4 "Barranquilla Met") ///
				(13001 = 5 "Cartagena Met") (68001 68307 68276 68547 = 6 "Bucaramanga Met") ///
				(54001 54261 54553 54673 54405 54874 = 7 "Cúcuta Met") ///
				(66001 66170 66400 = 8 "Pereira Met") (17001 17873 = 9 "Manizales Met") ///
				(20001 20013 20443 20621 20750 = 10 "Valledupar") (52001 = 11 "Pasto Met") ///
				(50001 = 12 "Villavicencio Met") (73001 = 13 "Ibagué") (23001 = 14 "Montería") ///
				(15001 = 15 "Tunja Met") (63001 = 16 "Armenia Met") (47001 = 17 "Santa Marta") ///
				(18001 = 18  "Florencia") (19001 = 19 "Popayan") (27001 = 20 "Quibdo") ///
				(41001 = 21 "Neiva") (44001 = 22 "Riohacha") (70001 = 23 "Sincelejo") ///
				, gen(Areas_br) label(Areas_met_banrep)
	gen 		d_area_metro = (Areas_br < 24)

	************************************************
	* Relacion temporal bebe cohorte medicos	   *
	************************************************

	** Participa en embarazo ** 
	foreach x in "ave" "thumb" {
				gen 	p_emb_obs_`x'=inrange(v_fconcep,p_fechin_obs_`x',p_fechfi_obs_`x') | inrange(fechanto,p_fechin_obs_`x',p_fechfi_obs_`x')
				label	var p_emb_obs_`x'  "Cohorte participa en embarazo"

				** Participa en todo el embarazo ** 
				gen 	p_embT_obs_`x'=v_fconcep>=p_fechin_obs_`x' & fechanto<=p_fechfi_obs_`x'
				label	var p_embT_obs_`x'  "Cohorte participa en todo el embarazo"
	}
	compress

	ren 		codigo12 codigo 
	gen 		p_mes_inicio = ym(2010,1)
	merge 		m:1 codigo p_mes_inicio using "Data\Anonimizada\SSO_Hospital_Vitales", keep(1 3) gen(_mergeH)

	
	merge 		m:1 codigo p_mes_inicio using "Data\Anonimizada\SSO_Hospital_Vitales_ES", keep(1 3) nogen keepusing(h_prop_unhealthy)
	label 		var _mergeH "_merge with variables at hospital level"
	ren 		codigo codigo12 
	drop 		p_mes_inicio 	

	************************************************
	*       Controles usados anteriormente         *
	************************************************
	ren 		nacido_mujer babyfemale 
	gen 		adolescent 	= (edad_madre<=19) 
	gen 		basicedu  	= nivel_madre<=6 | nivel_madre==13
	gen 		married 	= estado_civil_madre==1 | estado_civil_madre==6
	gen 		subsidie 	= (regimen_madre==1) 
	egen 		tag_mun 	= tag(codmundep)
	egen 		tag_hosp 	= tag(codigo12)
	bys  		codmundep: 	egen tot_hosp = total(tag_hosp)
	gen  		pop100 		= c_population/100000
	global 		controls 	babyfemale adolescent basicedu married subsidie tot_hosp pop100
	sum 		$controls
	egen 		p_draw= group(p_state p_date)
	egen 		id_cohorte=group(codigo12 p_date)

	destring 	id_nacido,replace
	gen 		mes_parto=ym(year(fechanto),month(fechanto))

	replace 	edad_madre=0 if edad_madre==.
	gen 		edadm_mis=edad_madre==0

	replace 	edad_padre=0 if edad_padre==.
	gen 		edadp_mis=edad_padre==0

	gen 		edad2 =edad_madre*edad_madre
	gen 		edadp2=edad_padre*edad_padre

	***********************************************
	***  				Outcomes 				***
	***********************************************


	gen 		bpn 			=  peso_nacido<=3
	gen 		numconsul0 	 	= numero_consultas==0
	gen 		numconsul1_3 	= numero_consultas==1
	gen 		numconsul4 	 	= numero_consultas>=2 & numero_consultas!=999
	gen 		nacer_menos_37 	= semanas_gestacion<37
	gen 		parto_ces 		= tipo_parto==2
	gen	 		apgar1min_4 	= apgar_1min==1
	gen	 		apgar1min_5_6 	= apgar_1min==2
	gen	 		apgar1min_7 	= apgar_1min==3
	gen	 		apgar5min_4 	= apgar_5min==1
	gen	 		apgar5min_5_6 	= apgar_5min==2
	gen	 		apgar5min_7 	= apgar_5min==3



	foreach var in female top public acred top_income{
	gen dummy_`var' =(percent_`var'>0.5)
		}
  

	drop 		if mi(semanas_gestacion)

	*Placebo hacia adelante
	gen 		p_fechin_obs_ave1f = p_fechin_obs_ave+365
	gen 		p_fechfi_obs_ave1f = p_fechfi_obs_ave+365
	
	gen 		p_fechin_obs_ave2f = p_fechin_obs_ave+731
	gen 		p_fechfi_obs_ave2f = p_fechfi_obs_ave+731

	gen 		p_fechin_obs_ave3f = p_fechin_obs_ave+1096
	gen 		p_fechfi_obs_ave3f = p_fechfi_obs_ave+1096

	gen 		p_fechin_obs_ave4f = p_fechin_obs_ave+1461
	gen 		p_fechfi_obs_ave4f = p_fechfi_obs_ave+1461
	
	
	*Placebo anterior
	gen 		p_fechin_obs_ave2  = p_fechin_obs_ave-730
	gen 		p_fechfi_obs_ave2  = p_fechfi_obs_ave-730
	gen 		p_fechin_obs_ave25 = p_fechin_obs_ave-913
	gen 		p_fechfi_obs_ave25 = p_fechfi_obs_ave-913
	gen 		p_fechin_obs_ave3  = p_fechin_obs_ave-1095
	gen 		p_fechfi_obs_ave3  = p_fechfi_obs_ave-1095
	gen 		p_fechin_obs_ave4  = p_fechin_obs_ave-1460
	gen 		p_fechfi_obs_ave4  = p_fechfi_obs_ave-1460

	format 		p_fechin_obs_ave3 %td 
	format 		p_fechfi_obs_ave3 %td 
	format 		v_fconcep %td
	format 		p_date %tm

	forvalues i=1/4 {
	format 		p_fechin_obs_ave`i'f %td 
	format 		p_fechfi_obs_ave`i'f %td 

	gen 		p_emb_n`i'f_obs=inrange(v_fconcep,p_fechin_obs_ave`i'f,p_fechfi_obs_ave`i'f) | inrange(fechanto,p_fechin_obs_ave`i'f,p_fechfi_obs_ave`i'f)
	}

	gen 		p_emb_n2_obs =inrange(v_fconcep,p_fechin_obs_ave2,p_fechfi_obs_ave2)   | inrange(fechanto,p_fechin_obs_ave2,p_fechfi_obs_ave2)
	gen 		p_emb_n25_obs=inrange(v_fconcep,p_fechin_obs_ave25,p_fechfi_obs_ave25) | inrange(fechanto,p_fechin_obs_ave25,p_fechfi_obs_ave25)
	gen 		p_emb_n3_obs =inrange(v_fconcep,p_fechin_obs_ave3,p_fechfi_obs_ave3)   | inrange(fechanto,p_fechin_obs_ave3,p_fechfi_obs_ave3)
	gen 		p_emb_n4_obs =inrange(v_fconcep,p_fechin_obs_ave4,p_fechfi_obs_ave4)   | inrange(fechanto,p_fechin_obs_ave4,p_fechfi_obs_ave4)
	
	gen 		year =  year(fechanto)
	replace 	p_emb_n3_obs=0 if year==2013
	label		var p_emb_n3_obs "Partos 3 anos antes"
	keep 		if p_emb_n4_obs==1 | p_emb_n3_obs==1 | p_emb_n25_obs | p_emb_n2_obs | p_emb_n4f_obs==1 | p_emb_n3f_obs==1 | p_emb_n2f_obs==1 | p_emb_n1f_obs==1 | p_emb_obs_ave==1 | p_emb_obs_thumb==1

	compress 
	save 		"Data\base_hospital_cohorte_bebe.dta", replace

	log close