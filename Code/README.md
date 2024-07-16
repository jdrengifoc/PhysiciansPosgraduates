# Codes Explanation

The analysiss is carried out in 9 R and Stata files. The folder playground contains code for explaratory analysis and summary statistics. The files *main.do*, *main.R*, and *requirements.R* set the enviroment need to run the entire project. 

**To do**

- Harmonize code names in order to know the running order. 
- Add a description of each code file. 
- Enunciates is there is still work-in-progress.
## 0_ReTHUS.R

This code processes the ReTHUS data which is a cross data that contains all active health worker data in Colombia. Moreover it: 

1. Select and rename variables 
2. Keep physicians graduated since 1995
3. Get demographics for each physician
4. **Juan David** puedes completar este punto, por favor. **lines 41 to 68**


## 1_get_mini_PILA.R 

This file creates a PILA dataset keeping main variables. We keep two type of variables: (1) identifier variables to merge with other datasets, (2) desing variables to construct the treatment status. This R code: 

1. Loops through ... monthly PILA
2. Creates a function where it keeps *personabasicaid*, *fecha_cobertura* and, *tipo_cotiz*
3. Unify variable names and classes
4. Save required data


## 2_design.R 

This following file focus in the construction of the project design. The following is a brief list of the data conditionals. 

- Physicians who did its bachelor in Colombia since 1995.
- If they have a speciality, it was done in Colombia. 
- 8 different designs: 
    
    1. 1a: Cohorts who did their first posgraduate between 2011-2017.
    2. 1b: Cohorts who did their first posgraduate between 2011-2017 + at least 4 contributions with code 21 in the first year.
    3. 1c: Cohorts who did their first posgraduate between 2011-2017 + not having contributed with code 21 before the date of graduation of the undergraduate degree in ReTHUS.
    4. 1d: Cohorts who did their first posgraduate between 2011-2017 + 1b + 1c. 
    5. 2a: Cohorts who did their first posgraduate between 2013-2017.
    6. 2b: Cohorts who did their first posgraduate between 2013-2017 + at least 4 contributions with code 21 in the first year.
    7. 2c: Cohorts who did their first posgraduate between 2013-2017 + not having contributed with code 21 before the date of graduation of the undergraduate degree in ReTHUS.
    8. 2d: Cohorts who did their first posgraduate between 2013-2017 + 2b + 2c.

The code has the following steps: 

**Juan David puedes hacer esta sección por favor**

## 3_get_history.R

This code recovers the main RIPS and PILA variables to contruct the outcomes. In RIPS we keep mental health information and in PILA we keep labor market variables. The code follows the following structure: 

1. We start by getting the RIPS history keeping person identifiers, diagnosis and date of the health contact. 

2. Then we flag each contact as consultation, urgency, hospitalization or procedure. 

3. We save the data. 

4. We get the PILA history with person ids, wage, number of working days, among others. 

5. After, we harmonize and save the data. 

## 4_connect_with_stata.R

This file converts the parquet datasets into stata format in order to run the analysis.  