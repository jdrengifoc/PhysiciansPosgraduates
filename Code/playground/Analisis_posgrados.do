cd "C:\Users\danie\Dropbox\Stuff\Data Salud\Rethus\RETHUS"

use "RETHUS_procesada", clear


* Grupo de interes

g year_pregrado = year(rethus_fechagradopre1)
tab year_pregrado, m
keep if (year_pregrado >= 1995 & year_pregrado != .)		//  Desde el 95. Son el 85%

tab rethus_codigoperfilpre1, m
keep if rethus_codigoperfilpre1 == "P07" 					// Medicos. Son el 10.4% del THS
	
tab rethus_codigoperfilpre2, m
keep if rethus_codigoperfilpre2 == "" 						// Sin segunda carrera. Son el 99.6%

tab rethus_codigoperfilpos1, m
keep if rethus_codigoperfilpos1 != "" 						// Con posgrado. Son el 26.4%

tab rethus_tipoprogramapos1
keep if rethus_tipoprogramapos1 == 5 						// Especialistas. Son el 99.2%. 31,096 personas

tab rethus_origentitulopos1
keep if rethus_origentitulopos1 == 1 						// Locales. Son el 74.4%. 23,121 personas

tab rethus_codigoperfilpos2, m
keep if rethus_codigoperfilpos2 == "" 						// Sin segundo posgrado. Son el 81.5%. 18,845 personas


* Tipos de posgrados

gen letter = substr(rethus_perfilpos1, 1, 1)
tab letter
tab rethus_perfilpos1

/*

     letter |      Freq.     Percent        Cum.
------------+-----------------------------------
          D |      1,111        5.90        5.90
          E |        333        1.77        7.66
          M |     11,155       59.19       66.86
          Q |      6,246       33.14      100.00
------------+-----------------------------------
      Total |     18,845      100.00

Los codigos inician por D, E, M y Q. Por los nombres, podemos deducir que:
	- D: Diagnostica
	- E: Multidisciplinaria
	- M: Clinica
	- Q: Quirurgica
	
	  
                     pos1 rethus_perfil |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
                 D01 - MEDICINA NUCLEAR |         22        0.12        0.12
                        D02 - PATOLOGÍA |        319        1.69        1.81
D03 - RADIOLOGÍA E IMÁGENES DIAGNÓSTI.. |        764        4.05        5.86
                D0303 - NEURORADIOLOGÍA |          1        0.01        5.87
    D0304 - RADIOLOGÍA INTERVENCIONISTA |          4        0.02        5.89
    D99 - OTRA ESPECIALIDAD DIAGNÓSTICA |          1        0.01        5.90
     E01 - SALUD FAMILIAR Y COMUNITARIA |        122        0.65        6.54
E99 - OTRA ESPECIALIDACIÓN MULTIDISCI.. |        183        0.97        7.51
E99P03 - OTRA ESPECIALIZACIÓN ENFERME.. |          1        0.01        7.52
E99P10 - OTRA ESPECIALIZACIÓN OPTOMET.. |         27        0.14        7.66


Diagnostica y multi parecen ser pocos, es posible que funcionen diferente y por eso no los vemos en PILA
Clinica y quirurgica si deberian reportarse en PILA

*/

* Otras variables relacionadas al posgrado no presentan heterogeneidad

tab rethus_tipoinstitucionpos1
/*

            pos1 rethus_tipoinstitucion |      Freq.     Percent        Cum.
----------------------------------------+-----------------------------------
                 1 - EDUCACIÓN SUPERIOR |     18,845      100.00      100.00
----------------------------------------+-----------------------------------
                                  Total |     18,845      100.00
*/


* Fechas
mdesc rethus_fechagradopos1 rethus_fechaconvpos1 rethus_fechaactoadmpos1
/*

    Variable    |     Missing          Total     Percent Missing
----------------+-----------------------------------------------
   rethu~dopos1 |           0         18,845           0.00
   rethus~vpos1 |      18,845         18,845         100.00
   rethus~mpos1 |           0         18,845           0.00
----------------+-----------------------------------------------
*/

* Si existe una razon estructural para los que no observamos en PILA, estara ligada al tipo de posgrado


















