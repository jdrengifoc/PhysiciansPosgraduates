# PhysiciansPosgraduates

This project aims to determine the causal effects of enrolling in specialization in medicine in Colombia on health and labor variables. For this we employ three databases, one crossectional [ReTHUS](https://www.sispro.gov.co/central-prestadores-de-servicios/Pages/ReTHUS-Registro-de-Talento-Humano-en-Salud.aspx) and two panel data at individual level ([RIPS](https://www.minsalud.gov.co/proteccionsocial/Paginas/rips.aspx) and [PILA](https://www.minsalud.gov.co/proteccionsocial/Paginas/pila.aspx)).

The workflow is divided in two parts. The first one was developed in R, which:

- process ReTHUS;
- identify the objetive population;
- make eight different possible designs;
- retrieve the RIPS and PILA history for the objetive population. 

Then, the second part developed in Stata,
- create new variables in RIPS and PILA data;
- balance the panel data of RIPS and PILA;
- make a full event study, i.e., DiD with and without controls for NT, NET and ET.

## Todo

- [ ] Gráfica Christian y Nico (martes)
- [ ] Escribir lo que ya se hizo (domingo)
- [ ] Presentación (Lunes)
- [ ] Escribir matemática (Lunes)
- [ ] Matching (Sara 1)
- [ ] Regresiones (Sara 1)
- [ ] Pasar resultados preliminares (viernes)
- [ ] Finalizar diapositivas


### Literature Review - Mental Health in Posgraduates:

A quick literature review was carried out to understand how researchers usually study mental health.

| **Author**      | **Title**      | **Year** | **Goal**        | **Data**         | **Effects**      | **Link**         |
|-----------------|----------------|----------|-----------------|------------------|------------------|------------------|