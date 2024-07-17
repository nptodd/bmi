# devtools::document("../bmi.utils")
# devtools::build("../bmi.utils")
# # installation to default library
# install.packages("../bmi.utils_1.0.tar.gz", repos=NULL)

Sys.setenv(LANGUAGE="EN")

library(bmi.utils)
library(data.table)
library(ggplot2)
library(mgcv)
# also needed: bayesplot, Hmisc


### Result Sets analyzed
RS1 = list(typeStrata="SexRacEduSmo",
           method="bhm",
           ageMin = 25, 
           ageMax = 55,
           bmi = "Deb2",
           cov = "stat", 
           w = "1.0",
           ws="1.0",
           coh_all = 1943:1993,
           coh = "43_93",  # should match coh_all !!!!!!!
           log = "no_log" # log or no_log
)
RS2 = RS1
RS2$typeStrata = "Sex" 

RS3 = RS1
RS3$coh_all = 1955:1993
RS3$coh <- "55_93"

RS4 = RS1
RS4$w = "0.1"
RS4$ws = "0.1"

RS5 = RS1
RS5$log = "log"


###############################################################################
#                                                                             #
#                             Preliminary loading                             #
#                                                                             #
###############################################################################

############### Main model
# list all strata
STRATA1 <- gsub(pattern = ".csv", replacement = "", 
                x = list.files(path_data_f(RS1)),
                fixed = "T" )
# load nhanes data
nhanes_bfda1 <- rbindlist( lapply(X = sapply(STRATA1, path_data_f, RS_a=RS1),
                                  FUN = fread ))
nhanes_bfda1[, seqn_long:=paste0("seqn", seqn)]
# Completed the age series with imputations
sapply(STRATA1, complete_Zage_f, RS_a=RS1)


############### Sex-only model
STRATA2 <- gsub(pattern = ".csv", replacement = "", 
                x = list.files(path_data_f(RS2)),
                fixed = "T" )
nhanes_bfda2 <- rbindlist( lapply(X = sapply(STRATA2, path_data_f, RS_a=RS2),
                                  FUN = fread ))
nhanes_bfda2[, seqn_long:=paste0("seqn", seqn)]
# Completed the age series with imputations
sapply(STRATA2, complete_Zage_f, RS_a=RS2)


############### Older cohorts removed
# list all strata
STRATA3 <- gsub(pattern = ".csv", replacement = "", 
                x = list.files(path_data_f(RS3)),
                fixed = "T" )
# load nhanes data
nhanes_bfda3 <- rbindlist( lapply(X = sapply(STRATA3, path_data_f, RS_a=RS3),
                                  FUN = fread ))
nhanes_bfda3[, seqn_long:=paste0("seqn", seqn)]
# Completed the age series with imputations
sapply(STRATA3, complete_Zage_f, RS_a=RS3)


############### Reduced w and ws
# list all strata
STRATA4 <- gsub(pattern = ".csv", replacement = "", 
                x = list.files(path_data_f(RS4)),
                fixed = "T" )
# load nhanes data
nhanes_bfda4 <- rbindlist( lapply(X = sapply(STRATA4, path_data_f, RS_a=RS4),
                                  FUN = fread ))
nhanes_bfda4[, seqn_long:=paste0("seqn", seqn)]
# Completed the age series with imputations
sapply(STRATA4, complete_Zage_f, RS_a=RS4)



############### Log BMI modelled
# list all strata
STRATA5 <- gsub(pattern = ".csv", replacement = "", 
                x = list.files(path_data_f(RS5)),
                fixed = "T" )
# load nhanes data
nhanes_bfda5 <- rbindlist( lapply(X = sapply(STRATA5, path_data_f, RS_a=RS5),
                                  FUN = fread ))
nhanes_bfda5[, seqn_long:=paste0("seqn", seqn)]
# Completed the age series with imputations
sapply(STRATA5, complete_Zage_f, RS_a=RS5)



###############################################################################
#                                                                             #
#                       Analysis of group-level patterns                      #
#                                                                             #
###############################################################################

# RUN ONLY IF SMOKING IS AMONG VARIABLES THAT DEFINE STRATA
png("../ms/figs_supApp/mu_allF.png",
    width = 3.25, height = 3.25,
    units = "in", res    = 1200, pointsize = 4)
# pdf("../ms/figs_supApp/mu_allF.png",
#     width = 13.25, height = 14.25)
par(mar=c(5.1,6.1,4.1,2.1))
muSRES_all_f(sex_a="f", RS1, strata_a=STRATA1)
mtext("Females", 3, 1, T, font=2, cex=2)
dev.off()
png("../ms/figs_supApp/mu_allM.png",
    width = 3.25, height = 3.25,
    units = "in", res    = 1200, pointsize = 4)
par(mar=c(5.1,6.1,4.1,2.1))
muSRES_all_f(sex_a="m", RS1, strata_a=STRATA1)
mtext("Males", 3, 1, T, font=2, cex=2)
dev.off()

png("../ms/figs/mu_selected.png",
    width = 8, height = 4,
    units = "in", res    = 1200, pointsize = 5)
par(mfrow=c(1, 2), mar=c(5.1,5.1,4.1,2.1))

muSRES_race_f(sex_a="f", educ_a = "hsl", smok_a = "0", 
              strata_a = STRATA1, RS_a = RS1, CI_a = T )
mtext("Females", 3, line=-3, at = 45, font=2, cex=1.8)

muSRES_race_f(sex_a="m", educ_a = "hsl", smok_a = "0", 
              strata_a = STRATA1, RS_a = RS1, CI_a = T )
mtext("Males", 3, line=-3, at = 45, font=2, cex=1.8)

mtext("B. Group level trajectory for selected strata",side =  3,
      line = 2, at = 20, cex=2, font=2) # panel number
dev.off()

# muSRES_interactive_f("m", RS_a = RS1, strata_a = STRATA1)
# muSRES_interactive_f("f", RS_a = RS1, strata_a = STRATA1)
# pdf("../figs_divers/sigma.pdf") 
# par(mfrow=c(3,3),oma = c(1, 1 ,2, 0), mar=c(3,2,3,1))
# analyze_sigma_f(sex_a="m", strata_a = STRATA1, RS_a = RS1)
# dev.off()


###### INDIVIDUAL TRAJECTORIES

# Pass mean posterior and 95%CI to plot_traj_f (avoid reloading)
Z = lapply(X = STRATA1, FUN = load_results_f, RS_a = RS1,
           var_a="Z", res_type_a = "short", var_type_a = "csv") 
Z_CL = lapply(X = STRATA1, FUN = load_results_f, RS_a = RS1,
              var_a="Z_CL", res_type_a = "short", var_type_a = "csv")
Z_UL =  lapply(X = STRATA1, FUN = load_results_f, RS_a = RS1,
               var_a="Z_UL", res_type_a = "short", var_type_a = "csv") 
Z    = Reduce(function(...){merge(..., all = TRUE)}, Z)
Z_CL = Reduce(function(...){merge(..., all = TRUE)}, Z_CL)
Z_UL = Reduce(function(...){merge(..., all = TRUE)}, Z_UL)

# check the number of subjects is as expected
cat("Number of subjects (via Z)             : ", ncol(Z)-1, # first col is tau
    "\nNumber of subjects (via nhanes_bfda) :",
    nhanes_bfda1[, .(unique(seqn))][,.N], "\n")


png("../ms/figs/individual_traj.png",
    width = 5, height = 5,
    units = "in", res    = 1200, pointsize = 5)
par(mfrow=c(2, 2), mar=c(4, 4, 0, 1), oma=c(2, 2, 3, 2))

set.seed(4)
for(AGES in list(46:RS1$ageMax, 38:45, 32:37, RS1$ageMin:31)){
  
  seqn_loc = sample(nhanes_bfda1[stratum== "f-b-hsl-0" & 
                                   age_screening %in% AGES, 
                                 unique(seqn_long)], 1)
  plot_traj_f(seqn_v_a = seqn_loc, 
              result_set_a = RS1,
              data_a = nhanes_bfda1,
              Z_a = Z, Z_CL_a = Z_CL, Z_UL_a = Z_UL,
              ylim=c(15, 70),
              verbose_a = F)
}
mtext("A. Individual-level reconstruction of BMI trajectories",
      adj = 0, side =  3, outer = T, cex=2, font=2)
dev.off()


pdf("../figs_misc/all_traj.pdf")
par(mfrow=c(2, 2), mar=c(4, 4, 0, 1), oma=c(2, 2, 2, 2))
plot_traj_f(seqn_v_a = unique(nhanes_bfda1[,seqn_long]),
            result_set_a = RS1,
            data_a = nhanes_bfda1,
            Z_a = Z, Z_CL_a = Z_CL, Z_UL_a = Z_UL,
            print_seqn_a=T)
dev.off()


# Compare with model 4
Z4 = lapply(X = STRATA4, FUN = load_results_f, RS_a = RS4,
            var_a="Z", res_type_a = "short", var_type_a = "csv") 
Z4_CL = lapply(X = STRATA4, FUN = load_results_f, RS_a = RS4,
               var_a="Z_CL", res_type_a = "short", var_type_a = "csv")
Z4_UL =  lapply(X = STRATA4, FUN = load_results_f, RS_a = RS4,
                var_a="Z_UL", res_type_a = "short", var_type_a = "csv") 
Z4    = Reduce(function(...){merge(..., all = TRUE)}, Z4)
Z4_CL = Reduce(function(...){merge(..., all = TRUE)}, Z4_CL)
Z4_UL = Reduce(function(...){merge(..., all = TRUE)}, Z4_UL)


# compare model 1 and model 4
SEQN <- sample(nhanes_bfda1[,seqn_long], 1)
par(mfrow=c(1,2))
plot_traj_f(seqn_v_a = SEQN,
            result_set_a = RS1,
            data_a = nhanes_bfda1,
            Z_a = Z, Z_CL_a = Z_CL, Z_UL_a = Z_UL,
            print_seqn_a = T,
            verbose_a = T)
plot_traj_f(seqn_v_a = SEQN,
            result_set_a = RS4,
            data_a = nhanes_bfda4,
            Z_a = Z4, Z_CL_a = Z4_CL, Z_UL_a = Z4_UL, 
            print_seqn_a = T,
            verbose_a = T)
nhanes_bfda1[seqn_long==SEQN]
rm(SEQN)



###############################################################################
#                                                                             #
#                     Check observed - model based values                     #
#                                                                             #
###############################################################################

### model 1
nhanes_check = copy(nhanes_bfda1)
Z_check = melt(Z, id.vars = "tau", measure.vars = names(Z)[-1], 
               variable.name = "seqn_long", value.name = "bmi_m")
nhanes_check[Z_check, on=c(seqn_long="seqn_long", age="tau"), bmi_mod:=bmi_m]

nhanes_check[, summary(bmi_deb2-bmi_mod)]

# nhanes_check[, plot(bmi_deb2, bmi_deb2-bmi_mod, pch=20, cex=0.4, col="dimgrey")]
round(100*nhanes_check[, .(Overweight_obs = mean(bmi_deb2>25), 
                                   Overweight_model = mean(bmi_mod>25),
                                   Obese_obs = mean(bmi_deb2>30), 
                                   Obese_model = mean(bmi_mod>30),
                                   SevereObese_obs = mean(bmi_deb2>40),
                                   SevereObese_model = mean(bmi_mod>40) )], 1)
rm(nhanes_check) ; rm(Z_check)


### model 5
Z5 = lapply(X = STRATA5, FUN = load_results_f, RS_a = RS5,
            var_a="Z", res_type_a = "short", var_type_a = "csv") 
Z5    = Reduce(function(...){merge(..., all = TRUE)}, Z5)
nhanes_check = copy(nhanes_bfda5)
Z_check = melt(Z5, id.vars = "tau", measure.vars = names(Z5)[-1], 
               variable.name = "seqn_long", value.name = "bmi_m")
nhanes_check[Z_check, on=c(seqn_long="seqn_long", age="tau"), bmi_mod:=bmi_m]

nhanes_check[, summary(bmi_deb2-bmi_mod)]


# nhanes_check[, plot(bmi_deb2, bmi_deb2-bmi_mod, pch=20, cex=0.4, col="dimgrey")]
round(100*nhanes_check[, .(Overweight_obs = mean(bmi_deb2>25), 
                                   Overweight_model = mean(bmi_mod>25),
                                   Obese_obs = mean(bmi_deb2>30), 
                                   Obese_model = mean(bmi_mod>30),
                                   SevereObese_obs = mean(bmi_deb2>40),
                                   SevereObese_model = mean(bmi_mod>40) )], 1)
rm(nhanes_check)
rm(Z_check)



###############################################################################
#                                                                             #
#                                 Projections                                 #
#                                                                             #
###############################################################################

######### Obesity prevalence
source("prevalence.R")
source("prevalence_race.R")
source("prevalence_young.R")



######### Time spent obese

# parameters for prediction, computation of individual times spent obese
PP = list(
  limit = c(30,40), # which bmi limits to investigate
  age_span=5  # age span of cohort groups
)

# vectors of breaks for cohorts
coh_l = make_breaks_f(age_a = 55,  # any age would be ok in this context
                      age_span_a = PP$age_span, 
                      data_a = nhanes_bfda1)

gc()

ncores = 33
grappe = parallel::makeCluster(ncores)
do.call(cbind, parallel::clusterEvalQ(grappe, library(data.table)))
do.call(cbind, parallel::clusterEvalQ(grappe, library(bmi.utils)))
parallel::clusterExport(grappe, c("nhanes_bfda1", "RS1") )
Sys.time()
timeSpentObese_0 <- parallel::parLapply(cl = grappe, 
                                        X = STRATA1, 
                                        fun = tso_f,  
                                        data_a = nhanes_bfda1,
                                        result_set_a = RS1, 
                                        limit_a=PP$limit, 
                                        N_iterations_a=1e4L)
parallel::stopCluster(grappe)
Sys.time()
timeSpentObese <- rbindlist(timeSpentObese_0)
Sys.time()
source("tso.R")
Sys.time()
source("tso_obese.R") # time spent obese by obese people
Sys.time()



