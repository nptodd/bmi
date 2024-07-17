# Projection of US adult obesity trends based on individual BMI trajectories

## Data

Data from the National Health and Nutrition Examination Survey (NHANES) are freely available at [https://www.cdc.gov/nchs/nhanes/](https://www.cdc.gov/nchs/nhanes/). Details on the variables used here may be found in script `R_preparation/variables_description.R`.

## Preparation of data (`R_preparation`)

The data for functional data analysis is prepared executing `main_prep.R`, in the `R_preparation` folder. The MATLAB code (folder `matlab/nhanes/`)  is called from within R.

The source code of the **BFDA toolbox** is modified only so as to save to csv files the posterior sample of individual trajectories. 

NB: Dependencies of the BFDA toolbox (e.g. mcmcdiag) are **not** included in this repository. They can all be downloaded from [BFDA's homepage](https://github.com/yjingj/BFDA).

## Analysis of results (`R_analysis`)

Analysis of results of BFDA (e.g. computation of prevalence obese or of time spent obese) is conducted in the `R_analysis` folder. Start from the `main.R` script.

## `bmi.utils`

A minimal R package that groups functions used for the preparation and analysis steps. 
