
	/*
	
	Replication package
	Figure 1. Heterogeneity in SABER PRO scores in medicine program
	Figure A.4. Heterogeneity in quantitative and reading SABER PRO scores
	
	*/

	
clear 		all 
set 		more off
	
cap log close
log using "Logs/Figure 1 - Figure A4.smcl", replace
	

********************************************************************************	
	
	use "Data/Anonimizada/Base_ps", clear
	
	keep if draw_bachelor   ==  1	
	drop if draw_date       >=  ym(2014,10)	
	drop if draw_date       <   ym(2013,1)
	egen 	prom_health     =   rowmean(ecaes_med_administracion ecaes_med_spublica)

	global  puntaje ecaes_med_reading ecaes_med_english ecaes_med_quantitative ecaes_med_social         ///
                    ecaes_med_writing ecaes_med_diagnostico ecaes_med_spublica ecaes_med_administracion ///
					prom_health
					
	* Dropping IES with less than 2 obs
	levelsof med_Institucion, local(levels)
    
	foreach lev of local levels {      
        di      "** med_Institucion==`lev' **"       
        sum     ecaes_med_spublica if med_Institucion == "`lev'"
        drop if med_Institucion == "`lev'" & r(N)<2      
	}
	
	* Drop outlier (very small ci)
	drop if med_Institucion == "UNIVERSIDAD DE BOYACA"
	
    * Outcome matrix
	mat des=J(44,18,.)
    
    local col=1
	levelsof med_Institucion, local(levels)
	   
	foreach var in $puntaje {	
        
        local row=1
        foreach lev of local levels {
        
            sum `var' if med_Institucion=="`lev'"
            mat des[`row',`col']=r(mean)
            mat des[`row',`col'+1]=r(sd)
            
            local row=`row'+1   
            
        }	
        
	local col=`col'+2
    
	}
	
	* Column names
	mat colnames des =  med_reading med_reading_sd med_english med_english_sd                   ///
                        med_quantitative med_quantitative_sd med_social med_social_sd           ///
                        med_writing med_writing_sd med_diagnostico med_diagnostico_sd           ///
                        med_spublica med_spublica_sd med_administracion med_administracion_sd   ///
                        med_prom_health med_prom_health_sd
	
	mat list des

	gen         institucion = ""
	levelsof    med_Institucion, local(levels)
	local       l=1
    
	foreach lev of local levels {
        
        di `l'
        replace institucion="`lev'" in `l'
        local l=`l'+1
        
	}
	
	svmat   des, names(col)
		
	keep    institucion med_reading med_reading_sd med_english med_english_sd       ///
	        med_quantitative med_quantitative_sd med_social med_social_sd           ///
			med_writing med_writing_sd med_diagnostico med_diagnostico_sd           ///
			med_spublica med_spublica_sd med_administracion med_administracion_sd   ///
			med_prom_health med_prom_health_sd

	keep    in 1/44				
	
	
	/* Graphs */
	
	set     dp period
	
	global  outcomes    med_reading  med_english     med_quantitative  med_social           ///
                        med_writing  med_diagnostico med_spublica      med_administracion   ///
                        med_prom_health
	
	foreach outcome in $outcomes {

        gen ub_`outcome'=(`outcome')+(`outcome'_sd)
        gen lb_`outcome'=(`outcome')-(`outcome'_sd)

	}

	sort    med_spublica
	egen    ranking=fill(1/2)
	sum     med_spublica, det
	local   p=r(p50)
	
    * Figure 1
	twoway (rcapsym lb_med_spublica ub_med_spublica ranking, sort lcolor(gray*1.2)                      ///
            mcolor(gray*1.2) msize(0.4) msymbol(square) lwidth(0.25))                                   ///
		   (scatter med_spublica ranking, sort mcolor(black*1.2) msize(1) msymbol(circle)), name(gr3)   ///
			title("", size(medium) color(black)) subtitle("", size(2.8)) yline(`p', lpattern(dash))     ///
            ytitle("Disease prevention" , size(medsmall)) ylabel(#9, angle(horizontal) format(%12.1f)   ///
            labsize(vsmall) nogrid) xtitle("Universities", size(medsmall)) xlabel(none, labsize(3.5))   ///
            legend(on  rows(1) symy(1) symxsize(6) bm(small)  pos(6) size(3.2)                          ///
            order(1 "One SD. " 2 "Mean") region(lcolor(white))) graphregion(fcolor(white)               ///
            style(none) color(white)  margin(r=6 l=3 t=3 b=1)) plotregion(fcolor(white)                 ///
            margin(r=7))   note(" ", size(2.5))
	
	cap drop ranking
	sort med_administracion
	egen ranking=fill(1/2)
	sum med_administracion, det
	local p=r(p50)
	
	twoway (rcapsym lb_med_administracion ub_med_administracion ranking, sort lcolor(gray*1.2)              ///
            mcolor(gray*1.2) msize(0.4) msymbol(square) lwidth(0.25))                                       ///
		   (scatter med_administracion ranking, sort mcolor(black*1.2) msize(1) msymbol(circle)), name(gr4) ///
			title("", size(medium) color(black)) subtitle("", size(2.8)) yline(`p', lpattern(dash))         ///
            ytitle("Health care" , size(medsmall)) ylabel(#9, angle(horizontal) format(%12.1f)              ///
            labsize(vsmall) nogrid) xtitle("Universities", size(medsmall)) xlabel(none, labsize(3.5))       ///
            legend(on  rows(1) symy(1) symxsize(6) bm(small)  pos(6) size(3.2)                              ///
            order(1 "One SD. " 2 "Mean") region(lcolor(white))) graphregion(fcolor(white) style(none)       ///
            color(white)  margin(r=6 l=3 t=3 b=1)) plotregion(fcolor(white)                                 ///
            margin(r=7))   note(" ", size(2.5))


	grc1leg gr4 gr3, leg(gr4) graphregion(fcolor(white) style(none) color(white))
	graph export "Results\Figure 1.png", width(1600) height(800) replace
		
	cap     drop ranking
	sort    med_quantitative
	egen    ranking=fill(1/2)
	sum     med_quantitative, det
	local   p=r(p50)
	
    * Figure A4
	twoway (rcapsym lb_med_quantitative ub_med_quantitative ranking, sort lcolor(gray*1.2)                  ///
            mcolor(gray*1.2) msize(0.4) msymbol(square) lwidth(0.25))                                       ///
		   (scatter med_quantitative ranking, sort mcolor(black*1.2) msize(1) msymbol(circle)), name(gr1)   ///
			title("", size(medium) color(black)) subtitle("", size(2.8)) yline(`p', lpattern(dash))         ///
            ytitle("Quantitative" , size(medsmall)) ylabel(#9, angle(horizontal) format(%12.1f)             ///
            labsize(vsmall) nogrid) xtitle("Universities", size(medsmall)) xlabel(none, labsize(3.5))       ///
            legend(on  rows(1) symy(1) symxsize(6) bm(small)  pos(6) size(3.2)                              ///
            order(1 "One SD. " 2 "Mean"  ) region(lcolor(white))) graphregion(fcolor(white) style(none)     ///
            color(white)  margin(r=6 l=3 t=3 b=1)) plotregion(fcolor(white)                                 ///
            margin(r=7))   note(" ", size(2.5))
	
	cap     drop ranking
	sort    med_reading
	egen    ranking=fill(1/2)
	sum     med_reading, det
	local   p=r(p50)
	
	twoway (rcapsym lb_med_reading ub_med_reading ranking, sort lcolor(gray*1.2) mcolor(gray*1.2)           ///
            msize(0.4) msymbol(square) lwidth(0.25))                                                        ///
		   (scatter med_reading ranking, sort mcolor(black*1.2) msize(1) msymbol(circle)), name(gr2)        ///
			title("", size(medium) color(black)) subtitle("", size(2.8)) yline(`p', lpattern(dash))         ///
            ytitle("Reading" , size(medsmall)) ylabel(#9, angle(horizontal) format(%12.1f) labsize(vsmall)  ///
            nogrid) xtitle("Universities", size(medsmall)) xlabel(none, labsize(3.5))                       ///
            legend(on  rows(1) symy(1) symxsize(6) bm(small)  pos(6) size(3.2)                              ///
            order(1 "One SD. " 2 "Mean"  ) region(lcolor(white))) graphregion(fcolor(white) style(none)     ///
            color(white)  margin(r=6 l=3 t=3 b=1)) plotregion(fcolor(white)                                 ///
            margin(r=7))   note(" ", size(2.5))
	
	grc1leg gr1 gr2  , leg(gr2)  graphregion(fcolor(white) style(none) color(white))
	graph export "Results\Figure A4.png", width(1600) height(800) replace
    
	
log close