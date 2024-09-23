	/*
	
	Replication package
	Table A1.
	
	*/
	
clear 		all 
set 		more off


cap log close
log using "Logs/Table A1.smcl", replace

********************************************************************************

use "Data/Anonimizada/SaberPro_Medicos_Scores", clear 
duplicates drop

replace IND_administracion_puntaje = subinstr(IND_administracion_puntaje, ",", ".", .)
replace IND_diagnostico_puntaje    = subinstr(IND_diagnostico_puntaje, ",", ".", .)
replace IND_spublica_puntaje       = subinstr(IND_spublica_puntaje, ",", ".", .)

ren (Programa Institucion  ) /// 
	(med_Programa med_Institucion  )

rename (IND_administracion_puntaje IND_diagnostico_puntaje IND_spublica_puntaje /// 
		IND_lectura_puntaje IND_ingles_puntaje IND_razonamiento_puntaje IND_competencias_puntaje IND_escritura_puntaje) ///
	   (ecaes_med_administracion ecaes_med_diagnostico ecaes_med_spublica ecaes_med_reading ecaes_med_english /// 
		ecaes_med_quantitative ecaes_med_social ecaes_med_writing)

destring ecaes_med_administracion ecaes_med_diagnostico ecaes_med_spublica ecaes_med_reading ecaes_med_english /// 
		 ecaes_med_quantitative ecaes_med_social ecaes_med_writing, replace

egen prom_4 = rowmean(ecaes_med_reading  ecaes_med_quantitative  ecaes_med_administracion ecaes_med_spublica) ///
		   if ecaes_med_reading != .  & ecaes_med_quantitative != . ///
		   & ecaes_med_administracion != . & ecaes_med_spublica != .

tempfile ecaes_med
save     "`ecaes_med'"

use "Data\Anonimizada\SSO_estimations_09092019.dta", clear
merge m:1 id using "`ecaes_med'", nogen

keep if treated == 1

gen codmundep=substr(treat_cod_plaza,1,5)

replace treat_cod_habi="0"+treat_cod_habi if length(treat_cod_habi)==9
replace codmundep=substr(treat_cod_habi,1,5) if missing(codmundep)==1

merge 		m:1 codmundep using "Data\Anonimizada\SSO_poblacion_tratados",keep(1 3) nogen
destring codmundep, gen(cod_dane)
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
	
	* Aquí quitamos areas metropolitanas
	keep        if d_area_metro ==0
	
global covars  	ecaes_gender ecaes_car ecaes_family_size ecaes_college_father ///
				ecaes_college_mother ecaes_strata12 ecaes_strata456 ///
				ecaes_sisben12 ecaes_internet ecaes_income12 ///
				ecaes_income3 ecaes_pwork ecaes_wash /// 
				ecaes_tv ecaes_cellphone ecaes_properfloor ecaes_oven ///
				ecaes_med_administracion ecaes_med_spublica	ecaes_med_reading ecaes_med_quantitative prom_4
				
sum ecaes_gender ecaes_car ecaes_family_size ecaes_college_father ///
ecaes_college_mother ecaes_strata12 ecaes_strata3 ecaes_strata456 ///
ecaes_sisben12 ecaes_sisben345 ecaes_internet ecaes_income12 ///
ecaes_income3 ecaes_income4mas ecaes_pwork ecaes_wash /// 
ecaes_tv ecaes_cellphone ecaes_properfloor ecaes_oven ///
ecaes_med_reading ecaes_med_quantitative ecaes_med_administracion ecaes_med_spublica 


qui mean $covars 
cap drop power_uno
gen power_uno = 1 if e(sample) 
replace power_uno = 0 if power_uno ==.

* Solo tratados
keep if power_uno ==1



keep $covars draw_bachelor draw_date draw_state power_uno treated ///
ecaes_med_writing ecaes_med_social ecaes_med_english ecaes_med_diagnostico /// 
ecaes_med_spublica ecaes_med_quantitative id


duplicates tag id, gen(dup)
sort       id draw_date 



forvalues p = 1/1{

	preserve
	keep if draw_bachelor==`p' 
	mat vlrp_`p' = J(22,3,.)

	local c =1

foreach var of global covars{

	sum `var' 
	mat vlrp_`p'[`c',1] = r(mean) 
	mat vlrp_`p'[`c',2] = r(sd)
	mat vlrp_`p'[`c',3] = r(N)
		
		local c = `c'+1	

	dis "`p'"
}
	restore
	svmat vlrp_`p' // creates variables from matrix
	*matrix colnames vlrp_1 = mean_`p' sd_`p' N_`p'
	
}
				
	*matrix rownames vlrp_1 = $covars


	mat list vlrp_1 
	putexcel set "Results/Tables paper matrices replication.xls", sheet("Raw Table A1") modify
	putexcel B2=mat(vlrp_1)


log close




