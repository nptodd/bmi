# devtools::document("../bmi.utils")
# devtools::build("../bmi.utils")
# # installation to default library
# install.packages("../bmi.utils_1.0.tar.gz", repos=NULL)


#############################################################################
#                                                                           #
#                            Choice of parameters                           #
#                                                                           #
#############################################################################

#### Choose included ages ---------------------------------------------------
ages_study <- 25:55


#### Choose included cohorts ------------------------------------------------
## Main analysis: all cohorts at some age in ages_study
cohorts_study  <- (1999-max(ages_study)-1):(2018-min(ages_study))
## Remove older cohorts effect
# cohorts_study  <- 1955:(2018-min(ages_study))


#### Variables that will define strata  --------------------------------------
Strata_def_var <- c("sex", "race", "educ", "smokerAgeMin")
# Strata_def_var <- c("sex")

abrev_strata <- F # in the case of Sex*Rac*Edu*Smo, may make sense
# to merge some strata with few individuals

#### should figures for supplementary appendix be drawn ----------------------
SUPAPPENDIX <- T


#############################################################################
#                                                                           #
#                                Load packages                              #
#                                                                           #
#############################################################################

library(bmi.utils)
library(data.table)
library(survey)

#############################################################################
#                                                                           #
#                         Load and clean NHANES data                        #
#                                                                           #
#############################################################################

# Strata_def_var_abrev and typeStrata defined based on Strata_def_var
Strata_def_var_abrev <- paste0(toupper(substr(Strata_def_var, 1, 1)), 
                               substr(Strata_def_var, 2,stop = 3))

typeStrata <- paste0(Strata_def_var_abrev,  # used as a folder name
                     collapse="")


if(abrev_strata){  typeStrata <- paste0(typeStrata, "_abr")   }


######## Comments only. Info on variables used
source("variables_description.R")


######## Load all cycles of continous NHANES in a list of data.tables
nhanes_l <- lapply(list.files("../data/nhanes/"), 
                   import_nhanes_f, 
                   path_a = "../data/nhanes/")
names(nhanes_l) <- list.files("../data/nhanes/")

# number of continuous NHANES cycles used
n_cycles =  length(nhanes_l)

######## Add weights for analysis of all cycles taken together
do.call(c, lapply(nhanes_l, add_weights_f ))

######## Clean data and create variables 
mapply(clean_nhanes_f, 
       data_a    = nhanes_l, 
       verbose_a = c(T, rep(F, n_cycles-1)), 
       height_a  = "measured")


#############################################################################
#                                                                           #
#                     Study recall of BMI 1yr, 10yr, at 25                  #
#                                                                           #
#############################################################################

# load nhanes II and III
source("load_other_nhanes/nhanesII.R")
source("load_other_nhanes/nhanesIII.R")
nhanesII  <- nhanesII[age %in% ages_study]
nhanesIII <- nhanesIII[age %in% ages_study]
# NB: no need to melt, since we only use current values 


#################################
################ BMI 1yr and 10 y

nhanes_recall = rbindlist(nhanes_l, fill = T)
nhanes_recall = nhanes_recall[age %in% ages_study]
nhanes_recall = melt_nhanes_f(nhanes_recall, bmis_a = "deb1_only") # deb2 not yet defined...
nhanes_recall = nhanes_recall[type_recall != "whichmax"]

D1 = isObs_f(0) & isObs_f(1)
D2red = isObs_f(0) & isObs_f(10) # reduced, without considering nhanesIII data
D2 = (isObs_f(0)|isObs_f(0, minYr_a = 1989, maxYr_a = 1994)) & isObs_f(10) # we can include nhanesIII data

# plot area for which comparison of measured and reported BMIs can be assessed
if(SUPAPPENDIX){source("regions_comparison.R")} # For SUPPLEMENTARY APPENDIX


# generate the models
make_Glist = function(weight_a = T){
  list( 
    curr   = G_f(type_recall_a = "curr", weighted_a = weight_a,
                 data_a=nhanes_recall, dataIII_a=nhanesIII),
    min1   = G_f(type_recall_a = "min1", weighted_a = weight_a, 
                 data_a=nhanes_recall, dataIII_a=nhanesIII),
    min10  = G_f(type_recall_a = "min10", weighted_a = weight_a, 
                 data_a=nhanes_recall, dataIII_a=nhanesIII),
    
    currM  = G_f(type_recall_a = "curr", weighted_a = weight_a, sex_a = "m", 
                 data_a=nhanes_recall, dataIII_a=nhanesIII),
    min1M  = G_f(type_recall_a = "min1", weighted_a = weight_a, sex_a = "m", 
                 data_a=nhanes_recall, dataIII_a=nhanesIII),
    min10M = G_f(type_recall_a = "min10", weighted_a = weight_a, sex_a = "m", 
                 data_a=nhanes_recall, dataIII_a=nhanesIII),
    
    currF  = G_f(type_recall_a = "curr", weighted_a = weight_a, sex_a = "f", 
                 data_a=nhanes_recall, dataIII_a=nhanesIII),
    min1F  = G_f(type_recall_a = "min1", weighted_a = weight_a, sex_a = "f", 
                 data_a=nhanes_recall, dataIII_a=nhanesIII),
    min10F = G_f(type_recall_a = "min10", weighted_a = weight_a, sex_a = "f", 
                 data_a=nhanes_recall, dataIII_a=nhanesIII) 
  )
}

Glist <- make_Glist() 

# graphically investigate misreporting of weight 1yr and 10 yrs ago
# source("graph_recall1y10y.R")


# Create deb2 bmis 1y & 10y ago, with cohort-level corrections for misreporting.
do.call(c, lapply(X = nhanes_l,  
                  FUN = add_bias_f, 
                  by_sex_a = T, Glist_a = Glist))



##########################
################ BMI at 25

gen25_f = function(data_a, wt_a, tot_a){
  # removes pregnant women : they should not contribute to the estimation
  # of the true curve of BMI at 25 (since recalled BMIs exclude pregnant women)
  # define rescaled weights and age 25 + or - 1yr to mix nhanes II, III and continous
  # nhanes III covers 6 years (1989-1994) = equivalent of 3 2-yr cycles
  # nhanes III covers 4 years (1976-1979) = equivalent of 2 2-yr cycles
  data_a[pregnant==0,
         .(weight, 
           weight_rescaled=weight*wt_a/tot_a, 
           age, yob, sex, pregnant, BMIcur_deb1,
           about25= (abs(age-25)<=1))] }

nhanes25= rbindlist(list(gen25_f(data_a = clear_labels(rbindlist(nhanes_l, fill = T)), # continuous NHANES data
                                 wt_a = n_cycles, tot_a=(n_cycles+3+2)),
                         gen25_f(data_a = nhanesIII, wt_a = 3, tot_a=(n_cycles+3+2)), # nhanes III
                         gen25_f(data_a = nhanesII,  wt_a = 2, tot_a=(n_cycles+3+2)) ) ) # nhanesII


G25list = list(a0 = G25_f(nhanes25_a = nhanes25), # a0, m0 and f0 with original weights (not rescaled)
               m0 = G25_f(nhanes25_a = nhanes25, sex_a="m"),
               f0 = G25_f(nhanes25_a = nhanes25, sex_a="f"),
               a  = G25_f(nhanes25_a = nhanes25, w_a="rescaled"),
               m  = G25_f(nhanes25_a = nhanes25, sex_a="m", w_a="rescaled"),
               f  = G25_f(nhanes25_a = nhanes25, sex_a="f", w_a="rescaled"))

##### Assess mean BMI at 25 using past weights
##### declared (=reported) during continuous nhanes
nhanes_dec25 <- rbindlist(nhanes_l, fill = T)
nhanes_dec25 <- nhanes_dec25[age %in% ages_study]
nhanes_dec25 <- nhanes_dec25[, .(cycle, cycle_miny, weight, age,
                                 yob, sex, BMI25_deb1)]


## investigate graphically bias in weight at 25 series
# source("graph_recall25.R")

# use both yob and age for bias correction
gam25dec_f  = function(sex_a=c("m", "f"), k_a=3){
  
  mgcv::gam(BMI25_deb1 ~ te(yob, age, k=k_a), 
            data = nhanes_dec25,
            family=Gamma(link=log), 
            subset = (sex %in% sex_a),
            weights = weight) 
}

G25_dec = list(a=gam25dec_f(),
               m=gam25dec_f(sex_a = "m"),
               f=gam25dec_f(sex_a = "f"))

# Add debiased series of weights at 25
do.call(c, lapply(X = nhanes_l,  
                  FUN = add_bias25_f, 
                  by_sex_a = T, 
                  G25list_a = G25list))



#############################################################################
#                                                                           #
#                            Descriptive statistics                         #
#                                                                           #
#############################################################################

#################################
################# Suppl. Appendix

if(SUPAPPENDIX){
  
  #### plot examples of individual bmi trajectories
  # source("descriptive_stat/plot_bmi_traj_f.R")
  # source("descriptive_stat/plot_bmi_traj.R")
  
  nhanes_descr <- rbindlist(nhanes_l, fill = T)
  
  # nhanes_descr[,table(stratum, cycle)]
  
  nhanes_descr <- nhanes_descr[,.(seqn, cycle, stratum, psu, weight, sex, age,
                                  yob, race, educ, smok_curr, smokerAgeMin)]
  nhanes_descr[, bc:=(as.integer(yob/5)*5)]
  # nhanes_descr[bc==1950, table(yob)] # check
  nhanes_descr[, end_age:= (age %in% (max(ages_study)-4):max(ages_study))]
  nhanes_descr[, aboveAgeMin:= (age > min(ages_study))]
  
  nhanes_descr[, smok_curr_log := NA]
  nhanes_descr[smok_curr %in% c(1L,2L), smok_curr_log := T]
  nhanes_descr[smok_curr == 3L, smok_curr_log := F] 
  nhanes_descr[, nonSmokerAgeMin:=(!smokerAgeMin)] # just for order of in graphs
  
  nhanes_descr[, is_hsl:=(educ=="hsl")] 
  nhanes_descr[, is_hslscd:=(educ=="hsl"|educ=="scd")] 
  
  nhanes_descr[, b:=(race=="b")] 
  nhanes_descr[, bo:=(race=="b"|race=="o")] 
  nhanes_descr[, boh:=(race=="b"|race=="o"|race=="h")] 
  
  nhanes_descr_l = list()
  
  for(cycle_loc in nhanes_descr[, unique(cycle)]){
    
    nhanes_descr_l[[cycle_loc]] <-   svydesign(id=~psu, strat=~stratum,
                                             weight=~weight,
                                             nest=TRUE, data=nhanes_descr[cycle==cycle_loc])
  }
  
  # inverse-variance weighting of estimates for the same cohorts but using successive NHANES cycles
  source("descriptive_stat/change_education.R")
  source("descriptive_stat/change_smoking.R")
  source("descriptive_stat/smoking_continuation.R")
  source("descriptive_stat/change_ethnicity.R")
  
  
} # end of if(SUPAPPENDIX)



#############################################################################
#                                                                           #
#                                     BFDA                                  #
#                                                                           #
#############################################################################

####################################
################# Prepare data.table 

nhanes_BFDA <- make_bfda_data_f(data_a = nhanes_l, 
                                ages_study_a = ages_study,
                                cohorts_study_a = cohorts_study)

# check available cohorts
# nhanes_BFDA[,.(min=min(yob), max=max(yob))]

# strata with few observations
# nhanes_BFDA[,.N,keyby=.(stratum)][N<250]

# group smokers college graduates hispanic+blacks. Too few observations
if(typeStrata=="SexRacEduSmo_abr"){
  nhanes_BFDA[stratum %in% c("f-b-cg-1", "f-h-cg-1"), stratum:="f-bh-cg-1"]
  nhanes_BFDA[stratum %in% c("f-b-scd-1", "f-h-scd-1"), stratum:="f-bh-scd-1"]
  nhanes_BFDA[stratum %in% c("m-b-cg-1", "m-h-cg-1"), stratum:="m-bh-cg-1"]
  nhanes_BFDA[stratum %in% c("m-b-scd-1", "m-h-scd-1"), stratum:="m-bh-scd-1"]
}

##############################################################
################# Info. for Table 1 and Supplementary Appendix

# TABLE S1
nhanes_BFDA[, .(M  = round(mean(bmi_deb2-bmi), 2), 
                SD = round(sd(bmi_deb2-bmi), 2)), 
            keyby=.(sex, type_recall)]

# Table S2
# Check how each correction changed obesity prevalence
nhanes_BFDA[,table(bmi>30)]
nhanes_BFDA[,.(ORIG = mean(bmi>30)*100, 
               DEB1 = mean(bmi_deb1>30)*100, 
               DEB2 = mean(bmi_deb2>30)*100), by=type_recall]


nhanes_BFDA[,.(unique(seqn))][,.N] # Number of individuals

# number of observations / individual
nhanes_BFDA[,.N, by=.(seqn, sex)][,.(M=round(mean(N), 1), SD=round(sd(N), 1) ), by=sex]



nhanes_BFDA_unique <- unique(nhanes_BFDA, by = "seqn" )

nhanes_BFDA_unique[,.N, by=sex] # Check number of individuals


### currently obese, F
nhanes_BFDA[type_recall=="curr" & sex=="f", table(bmi_deb2>30)]
nhanes_BFDA_unique[sex=="f", .N]-nhanes_BFDA[type_recall=="curr" & sex=="f", .N]
# check who's missing (pregnant women cycle A)
nhanes_BFDA[, sum(type_recall=="curr"), by=.(seqn, pregnant, cycle)][V1==0, table(pregnant, cycle)]
why_missing <- nhanes_BFDA[, sum(type_recall=="curr"), by=.(seqn, pregnant, cycle)][V1==0 & cycle!="A", seqn]
nhanes_l$D[seqn %in% why_missing, .(pregnant, BMImeas, BMIcur)]
rm(why_missing)

### currently obese, M
nhanes_BFDA[type_recall=="curr" & sex=="m", table(bmi_deb2>30)]
nhanes_BFDA_unique[sex=="m", .N]-nhanes_BFDA[type_recall=="curr" & sex=="m", .N]
### obese at 25, F
nhanes_BFDA[age == 25 & sex=="f", table(bmi_deb2>30)]
nhanes_BFDA_unique[sex=="f", .N]-nhanes_BFDA[age == 25 & sex=="f", .N]
### obese at 25, M
nhanes_BFDA[age == 25 & sex=="m", table(bmi_deb2>30)]
nhanes_BFDA_unique[sex=="m", .N]-nhanes_BFDA[age == 25 & sex=="m", .N]



nhanes_BFDA_unique[, .(M=round(mean(yob)), MIN=min(yob), MAX=max(yob)), by=sex]

nhanes_BFDA_unique[sex=="f", table(race)]
round(nhanes_BFDA_unique[sex=="f", prop.table(table(race))]*100, 1)
nhanes_BFDA_unique[sex=="m", table(race)]
round(nhanes_BFDA_unique[sex=="m", prop.table(table(race))]*100, 1)

nhanes_BFDA_unique[sex=="f", weighted.mean(race=="h", weight)]*100
nhanes_BFDA_unique[sex=="f", weighted.mean(race=="b", weight)]*100
nhanes_BFDA_unique[sex=="f", weighted.mean(race=="w", weight)]*100
nhanes_BFDA_unique[sex=="f", weighted.mean(race=="o", weight)]*100
nhanes_BFDA_unique[sex=="m", weighted.mean(race=="h", weight)]*100
nhanes_BFDA_unique[sex=="m", weighted.mean(race=="b", weight)]*100
nhanes_BFDA_unique[sex=="m", weighted.mean(race=="w", weight)]*100
nhanes_BFDA_unique[sex=="m", weighted.mean(race=="o", weight)]*100



nhanes_BFDA_unique[sex=="f", table(educ)]
round(nhanes_BFDA_unique[sex=="f", prop.table(table(educ))]*100, 1)
nhanes_BFDA_unique[sex=="m", table(educ)]
round(nhanes_BFDA_unique[sex=="m", prop.table(table(educ))]*100, 1)

nhanes_BFDA_unique[sex=="f", weighted.mean(educ=="hsl", weight)]*100
nhanes_BFDA_unique[sex=="f", weighted.mean(educ=="scd", weight)]*100
nhanes_BFDA_unique[sex=="f", weighted.mean(educ=="cg", weight)]*100
nhanes_BFDA_unique[sex=="m", weighted.mean(educ=="hsl", weight)]*100
nhanes_BFDA_unique[sex=="m", weighted.mean(educ=="scd", weight)]*100
nhanes_BFDA_unique[sex=="m", weighted.mean(educ=="cg", weight)]*100


nhanes_BFDA_unique[sex=="f", table(smokerAgeMin)]
round(nhanes_BFDA_unique[sex=="f", prop.table(table(smokerAgeMin))]*100, 1)
nhanes_BFDA_unique[sex=="m", table(smokerAgeMin)]
round(nhanes_BFDA_unique[sex=="m", prop.table(table(smokerAgeMin))]*100, 1)

nhanes_BFDA_unique[sex=="f", weighted.mean(smokerAgeMin==0, weight)]*100
nhanes_BFDA_unique[sex=="f", weighted.mean(smokerAgeMin==1, weight)]*100
nhanes_BFDA_unique[sex=="m", weighted.mean(smokerAgeMin==0, weight)]*100
nhanes_BFDA_unique[sex=="m", weighted.mean(smokerAgeMin==1, weight)]*100


##################################
################# Export BFDA data

# get (final) list of strata
strata_bfda <- nhanes_BFDA[,sort(unique(stratum))]
# nhanes_BFDA[,.N,keyby=.(stratum)]
# nhanes_BFDA[,length(unique(age)),keyby=.(stratum)][,table(V1)]


#### Make directories and export data used in Matlab ------------------------

# create directory for stratified data later used by BFDA
dir_data_matlab <- paste0("../data/for_matlab/", typeStrata, 
                          "/ages", 
                          min(ages_study), "_", 
                          max(ages_study), 
                          "/cohorts", 
                          gsub("^19", "", min(cohorts_study)), "_",
                          gsub("^19", "", max(cohorts_study)), "/")
dir.create(dir_data_matlab, 
           recursive = T, 
           showWarnings = F)

prod(sapply(X = strata_bfda,
            FUN = export_bfda_f, 
            data_a = nhanes_BFDA,
            directory_a=dir_data_matlab))


#### Define set of values ---------------------------------------------------

prefix <- paste0("matlab -r \"",  
                 "ageMin=", min(ages_study), ";",
                 "ageMax=", max(ages_study), ";", 
                 "cohortMin=", min(cohorts_study), ";",
                 "cohortMax=", max(cohorts_study), ";",
                 "typeStrata='", typeStrata, "';")

# see matlab script for default values of other tunable parameters
paramSet_com <- c( "w=1.0; ws=1.0; ",
                   "w=1.0; ws=1.0; log_transform=true;",
                   "w=1.0; ws=1.0; deb_bmi_nb=0;", # ,
                   "w=0.1; ws=1.0;",
                   "w=1.0; ws=0.1; ",
                   "w=0.1; ws=0.1; " )

suffix <- paste0("main_matlab; exit\"")

paramSet_com <- paste0(prefix, paramSet_com, suffix)

# quick results : just first set of parameters
paramSet_com <- paramSet_com[1]

#### Launch Matlab ----------------------------------------------------------

setwd("../matlab/nhanes/")

for(param in paramSet_com){
 
  system(param)
  # delay next call for 3hrs (if not last call)
  Sys.sleep( 3600*3*(param != paramSet_com[length(paramSet_com)]) )
}





