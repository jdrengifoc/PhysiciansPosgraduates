/*
Replication Package
Step 1:
Generate variables at the physician level concerning assignment, test scores, and socioeconomic factors. 
Merge the starting date of the physicians. 
Save this dataset as "base_cohorte_rural_con_fecha_inicio."
 
*/



clear 		all
set 		more off

cap log close
log using "Logs/Data - Step 1.smcl", replace



preserve 	
	import 	excel using "Data\Anonimizada\Universidades_Top_Medicine.xlsx", sheet("Both") clear firstrow
	ren 	Id Id_Inst 
	ren 	Nombre ecaes_institution 
	replace ecaes_institution = upper(ecaes_institution)
	tempfile topu
	save 	`topu'

	import excel "Data\Anonimizada\instituciones acreditadas.xlsx", sheet("Sheet1") firstrow clear
	tempfile acred
	save    `acred', replace
restore 


use 		"Data\Anonimizada\SSO_estimations_09092019", clear

merge 		m:1 ecaes_institution using `topu'

gen 		u_top = _merge==3
drop 		_merge

merge 		m:1 ecaes_institution using "`acred'", keep(1 3)

gen 		u_acred = acreditado==1
drop 		_merge


ren 		treat_num_doc Documento 
merge 		1:1 Documento draw_date using "Data\Anonimizada\Profesionales_Fecha_Inicio_Imp.dta", keep(1 3) gen(_mergeFI)

label 		var _mergeFI "_merge Fecha Inicio"

* Creamos el codigo de hospital con 12 digitos
gen 		codigo12 = substr(treat_cod_plaza,1,12)

* Creamos el codigo de hospital con 10 digitos
* Nota draw 2013-1 el codigo de plaza viene solo con 10 digitos, por tanto, para no perder información, se crea un identificador unicamente de 10 digitos
* tener presente a la hora de parea
* treat_cod_plaza es missing en 2013-1 por eso se usa treat_cod_habi
gen 		codigo10 = cond(length(substr(treat_cod_habi,1,10))==10,substr(treat_cod_habi,1,10),"0"+substr(treat_cod_habi,1,10)) 

drop 		ecaes_*

global 		vars_ecaes ecaes_family_size ecaes_headhouse ecaes_dependants ecaes_educ* ///
			ecaes_ocup* ecaes_stratum ecaes_sisben ecaes_floor ecaes_cellphone /// 
			ecaes_internet ecaes_tv ecaes_wash ecaes_oven ecaes_car ecaes_room ///
			ecaes_famincome ecaes_stwrk ecaes_interperscore ecaes_probscore /// 
			ecaes_institution ecaes_microwave ecaes_fridge ecaes_marstatus /// 
			ecaes_single ecaes_instype 


			
* Pegamos con base de Saber Pro para obtener el sexo del medico
merge 		1:1 id draw_date using "Data\Anonimizada\Base_ps", keep(1 3) nogen keepusing(ecaes_gender ecaes_med_* ecaes_fin* $vars_ecaes)

* conservamos solo los medicos (bacteriologos, enfermeras y odontologos no nos interesa, por ahora).
keep 		if draw_bachelor==1


* promedio de reading quantitative public health and health management
egen 		prom_4 = rowmean(ecaes_med_reading ecaes_med_quantitative  ecaes_med_administracion ecaes_med_spublica) ///
			if ecaes_med_reading != . &  ecaes_med_quantitative != . & ecaes_med_administracion != . & ecaes_med_spublica != .

label 		var prom_4 "Average of the 4 main tests: Reading, math, management and public health"

drop 		if mi(prom_4)

egen 		prom_acad 	= rowmean(ecaes_med_reading ecaes_med_quantitative)
egen 		prom_health = rowmean(ecaes_med_administracion ecaes_med_spublica)

***************************************************************
*|||||||||||||||||||| PRINCIPAL COMPONENT ||||||||||||||||||||* 
***************************************************************
pca 		ecaes_med_reading ecaes_med_quantitative  ecaes_med_administracion ecaes_med_spublica, com(1)
predict 	comp1 

pca 		ecaes_med_reading ecaes_med_quantitative, com(1)  
predict  	comp1_acad

pca 		ecaes_med_administracion ecaes_med_spublica, com(1)
predict 	comp1_health

* Dejamos unicamente las personas que fueron tratados
keep 		if treat_rural == 1



replace 	distancia_imp = 0 if distancia_imp<0


	/*Criterio 1: Imputar la fecha de inicio de la misma profesión, hospital y 
	cohorte de sorteo */
	sort codigo10 codigo12 draw_date 
	egen d1=mean(distancia_imp), by(codigo10 codigo12 draw_date)
	
	replace distancia_imp=d1 if distancia_imp==.

	/*Criterio 3: Imputar la fecha de inicio de la misma profesión y hospital*/	
	sort codigo10 codigo12 draw_date
	egen d3=mean(distancia_imp), by(codigo10 codigo12)
	
	replace distancia_imp=d3 if distancia_imp==.
	
	/*Criterio 4: Imputar la fecha de inicio de la misma profesión, sorteo y 
	departamento */
	egen d4=mean(distancia_imp), by(draw_date depto)
	
	replace distancia_imp=d4 if distancia_imp==.	
	
	
gen 		n_docs_temp 		= 1 
gen 		n_docs_women_temp 	= ecaes_gender==1
gen 		n_docs_men_temp 	= ecaes_gender==0
ren 		u_top n_docs_top_temp

gen 		n_docs_top_income_temp = ecaes_famincome>=4
tab 		ecaes_famincome n_docs_top_income_temp

ren 		ecaes_instype n_docs_u_publica_temp
ren 		u_acred n_docs_u_acred_temp 
global 		vars n_docs_temp n_docs_women_temp n_docs_men_temp n_docs_top_temp n_docs_top_income_temp n_docs_u_publica_temp n_docs_u_acred_temp 
		
foreach 	x in $vars {
	egen 	`x'_a = total(`x'), by(codigo10 codigo12 draw_date treat_codmundep)
	drop 	`x'
}
ren 	*_temp_a *


foreach x in prom_4 prom_acad prom_health comp1 comp1_acad comp1_health ecaes_med_reading ecaes_med_quantitative ecaes_med_administracion ecaes_med_spublica {
	egen 		ave_`x' = mean(`x')  , 			by(codigo10 codigo12 draw_date treat_codmundep)
	egen 		p25_`x' = pctile(`x'), p(25) 	by(codigo10 codigo12 draw_date treat_codmundep)
	egen 		p50_`x' = pctile(`x'), p(50) 	by(codigo10 codigo12 draw_date treat_codmundep)
	egen 		p75_`x' = pctile(`x'), p(75) 	by(codigo10 codigo12 draw_date treat_codmundep)
	egen 		min_`x' = min(`x'), 			by(codigo10 codigo12 draw_date treat_codmundep)
	egen 		max_`x' = max(`x'), 			by(codigo10 codigo12 draw_date treat_codmundep)
}

	egen ave_program = median(prom_health), by(draw_date ecaes_institution Profesion)
	
	
label 		var ave_prom_4 "Average score of the cohort (hospital-draw level)"
label 		var p25_prom_4 "Percentile 25 score of the cohort (hospital-draw level)"
label 		var p50_prom_4 "Percentile 25 score of the cohort (hospital-draw level)"
label 		var p75_prom_4 "Percentile 25 score of the cohort (hospital-draw level)"

* Labels
label 		variable treat_year 	"draw year"
label 		variable treat_month 	"draw month"
label 		variable Documento 		"treat id"
label 		variable codigo12 		"12 digits hospital code"

order 		codigo12 codigo10  
compress

* Guardo esta base de datos
order 		Documento codigo12 codigo10  /// 
			treat_year treat_month treat_codmundep 

egen 		tag_cohorte = tag(draw_date draw_state codigo10 codigo12)


sum 		distancia_imp if tag_cohorte==1, d

egen 		ave_distancia_imp_hc = mean(distancia_imp), by(draw_date draw_state codigo10 codigo12)
egen 		ave_distancia_imp_c  = mean(distancia_imp), by(draw_date draw_state)
egen 		ave_distancia_imp_d  = mean(distancia_imp), by(draw_date)

label 		var ave_distancia_imp_hc "promedio de la distancia draw-inicio por hospital-cohorte"
label 		var ave_distancia_imp_c  "promedio de la distancia draw-inicio por draw-state"
label 		var ave_distancia_imp_d  "promedio de la distancia draw-inicio por draw"



replace    	draw_day= mdy(1,25,2013)  if draw_date==ym(2013,1) & mi(draw_day)
replace 	draw_day= mdy(4,19,2013)  if draw_date==ym(2013,4) & mi(draw_day)
replace 	draw_day= mdy(7,19,2013)  if draw_date==ym(2013,7) & mi(draw_day)
replace 	draw_day= mdy(10,18,2013) if draw_date==ym(2013,10) & mi(draw_day)
replace 	draw_day= mdy(1,17,2014)  if draw_date==ym(2014,1) & mi(draw_day)
replace 	draw_day= mdy(4,22,2014)  if draw_date==ym(2014,4) & mi(draw_day)
replace 	draw_day= mdy(7,22,2014)  if draw_date==ym(2014,7) & mi(draw_day)
replace 	draw_day= mdy(10,22,2014) if draw_date==ym(2014,10) & mi(draw_day)
format  	draw_day %td
	
sum 		distancia_imp
gen 		p_fechin_obs_thumb = draw_day+int(`r(mean)')
gen 		p_fechfi_obs_thumb = p_fechin_obs_thumb+365

keep 		codigo12 codigo10 draw_date draw_state treat_codmundep *prom* *n_docs* *comp1* ave_dist*  draw_day p_fech* *_ecaes_med_* ave_program


drop 		prom_4 prom_acad prom_health comp1 comp1_acad comp1_health 

collapse (max) ave_program, by(codigo12 codigo10 draw_date draw_state treat_codmundep *prom* *n_docs* *comp1* ave_dist* draw_day p_fech* *_ecaes_med_*)


ren 		(draw_date draw_state treat_codmundep) (p_date p_state p_codmundep)
compress 

foreach x in ave  {
	gen 		p_fechin_obs_`x' = draw_day+`x'_distancia_imp_hc
	gen			p_fechfi_obs_`x' = p_fechin_obs_`x'+365
}



format 		p_date %tm
format 		p_fechin_obs_ave %td
format 		p_fechfi_obs_ave %td

gen 		percent_female 		= n_docs_women/n_docs
gen 		percent_top 		= n_docs_top/n_docs 
gen 		percent_top_income 	= n_docs_top_income/n_docs
gen 		percent_public 		= n_docs_u_publica/n_docs
gen 		percent_acred 		= n_docs_u_acred/n_docs	

save 		"Data\base_cohorte_rural_con_fecha_inicio.dta", replace 
log close