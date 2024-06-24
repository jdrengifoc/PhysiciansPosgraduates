/*************************************************************************
 *************************************************************************			       	
					Summary statistics
			 
1) Created by: Daniel Márquez
			   Harvard Business School
			   dmarquezm20@gmail.com		
				
2) Date: April 2024

3) Objective: Calculate the summary statistics for our sample.
			  This do file can only be ran at BanRep's servers.

4) Output:	- 
*************************************************************************
*************************************************************************/	


****************************************************************************
*Required packages (uncomment if running for the first time)
****************************************************************************
*ssc install schemeplot, replace

****************************************************************************
*Globals and matrices rows and columns
****************************************************************************

* Working directory
if "`c(hostname)'" == "SM201439"{
	global pc "C:"
}

else {
	global pc "\\sm093119"
}

global data 	"${pc}\Proyectos\Banrep research\PhysiciansPosgraduates\Data"
global logs 	"${pc}\Proyectos\Banrep research\PhysiciansPosgraduates\Logs"
global output 	"${pc}\Proyectos\Banrep research\PhysiciansPosgraduates\Output"

set scheme white_tableau

cap log clos
log using "${logs}\descriptives.smcl", replace

****************************************************************************
**#						1. Data
****************************************************************************

use "${data}\Individual_balanced_all_RIPS", clear
rename year_RIPS fecha_pila
drop if mi(fechapregrado)
merge 1:1 personabasicaid fecha_pila using "${data}\Individual_balanced_all_PILA"

drop if _merge == 2 // Estos son los del 2008

keep if (year_grado >= 1995)
drop if (rethus_sexo != 1 & rethus_sexo != 2)
keep if inrange(posgrado_start, 2011, 2017) | mi(posgrado_start) 

* Create the distance-to-event dummies
gen dist = fecha_pila - posgrado_start
gen pre3 = dist < -2
gen pos6 = dist >  5

* Esto lo tenemos que borrar cuando corramos rips desde el comienzo
keep if rethus_codigoperfilpre1 == "P07"


****************************************************************************
**#						1. General descriptives
****************************************************************************

preserve 

	g posgrado_start_d	= (posgrado_start == fecha_pila)

	collapse (max) posgrado_start_d service_mental, by(personabasicaid)	
	
	g physicians_total 	= 1
	g p_posgrados 		= posgrado_start_d/physicians_total
	g p_mental 			= service_mental/physicians_total
	
	sum p_posgrados
	
	sum p_mental
	
	count if p_mental == 1 & p_posgrados == 0
	sum p_mental if p_posgrados == 0
	
	count if p_mental == 1 & p_posgrados == 1
	sum p_mental if p_posgrados == 1

restore


****************************************************************************
**#						2. Figures
****************************************************************************

* Enrollments by year
preserve

	g physicians_total 	= 1
	g posgrado_start_d	= (posgrado_start == fecha_pila)

	collapse (sum) physicians_total posgrado_start_d, by(fecha_pila)
	keep if inrange(fecha_pila, 2011, 2017)
	save "${output}\Tables\Des_enrollment", replace
	
	g p_posgrados = posgrado_start_d*100/physicians_total
	sum p_posgrados
	local mean: di %5.2fc r(mean)
	
	/*
	tw (bar physicians_total fecha_pila, barw(0.5) fcolor(gs11) lcolor(gs8)) 									///
	   (bar posgrado_start_d fecha_pila, barw(0.5) fcolor(gs7)  lcolor(gs4)),									///
		ytitle("Number of physicians")																			///
		xtitle("Year")																							///
		ylab(#10, format(%12.0fc))																				///
		xlab(2011(1)2017, val angle(360))																		///
		legend(order(1 "Physicians" 2 "Graduate enrollments") position(6) col(2))								///
		note("On average, `mean'% of physicians enrolled in a postgraduate degree during the sample period.")
	*/
	
	tw (bar posgrado_start_d fecha_pila, barw(0.5) fcolor(gs11) lcolor(gs8)),	 								///
		ytitle("Number of physicians")																			///
		xtitle("Enrollment per year")																			///
		ylab(0(200)1800, format(%12.0fc))																		///
		xlab(2011(1)2017, val angle(360))																		///
		note("On average, `mean'% of physicians observed each year enrolled in a postgraduate degree.")		
	graph export "${output}\Figures\Enroll_year.png", replace
	
restore

* Mental illness and working while studying, for each year since started postgraduate
preserve
	
	keep if inrange(posgrado_start, 2011, 2017)
	
	g physicians_total 	= 1
	g working1			= (pila_dependientes == 1 | pila_independientes == 1)
	g working2			= (p_cotizaciones == 1)
	
	collapse (sum) physicians_total service_mental working1 working2, by(dist)
	save "${output}\Tables\Des_outcomes", replace
	
	replace dist = dist + 1
	keep if inrange(dist, 1, 6)
	
	g p_mental 	= service_mental*100/physicians_total
	sum p_mental
	local mean: di %5.2fc r(mean)
	
	tw (bar p_mental dist, barw(0.5) fcolor(gs11) lcolor(gs8)),														///
		ytitle("Percent")																							///
		xtitle("Prevalence of Mental Illness by year in graduate program")											///
		ylab(0(0.4)2, format(%5.2fc))																				///
		xlab(1(1)6, val angle(360))																					///
		note("On average, `mean'% of physicians suffered any mental illness during (and after) graduate school.")
	graph export "${output}\Figures\Mental_year.png", replace
	
	g p_working1 = working1*100/physicians_total
	sum p_working1
	local mean: di %5.2fc r(mean)
	
	tw (bar p_working1 dist, barw(0.5) fcolor(gs11) lcolor(gs8)),													///
		ytitle("Percent")																							///
		xtitle("Probability of having a formal job by year in graduate program")						///
		ylab(0(20)100, format(%5.0fc))																				///
		xlab(1(1)6, val angle(360))																					///
		note("On average, `mean'% of physicians had a formal job during (and after) graduate school.")
	graph export "${output}\Figures\Working1_year.png", replace
	
	g p_working2 = working2*100/physicians_total
	sum p_working2
	local mean: di %5.2fc r(mean)
	
	tw (bar p_working2 dist, barw(0.5) fcolor(gs11) lcolor(gs8)),													///
		ytitle("Percent")																							///
		xtitle("Probability of having more than one cotization by year in graduate program")						///
		ylab(0(20)100, format(%5.0fc))																				///
		xlab(1(1)6, val angle(360))																					///
		note("On average, `mean'% of physicians had a formal job during (and after) graduate school.")
	graph export "${output}\Figures\Working2_year.png", replace
	
restore

log close

