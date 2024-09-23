	/*
	
	Replication package
	FIGURE A.3.  Distribution of physicians per municipalities
	Last modified: 02/10/2023 
	
	*/
    
    
clear 		all 
set 		more off
    
cap log close
log using "Logs/Figure A3.smcl", replace


********************************************************************************

    use "Data/Anonimizada/Weights_2012.dta", replace
    merge m:1 codmundep using "Data/Anonimizada/Municipios_1_hospital", keep(3) nogen

    gen     i=_n
    reshape long medico_, i(i)

    sum     medico_, det
    drop if medico_>r(p99)
    rename  medico_ medicos
    drop if medicos>20

    kdensity    medicos, bwidth(1.5) xtitle("Physicians per municipalities") title("") note("")
    graph       export "Results/Figure A3.png",replace wid(1446) hei(1050)
 

log close