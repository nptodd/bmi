# list of parameters
param_pred = list(
  age_proj = 55,     # age at which prediction is needed
  age_span = 5,           # age span of cohort groups
  bmi_limit = c(30, 40)   # which obesity category we project
)

P1 <- prevalence_f(PP_a = param_pred, 
                   data_a = nhanes_bfda1, strata_a = STRATA1, 
                   RS_a = RS1)
P2 <- prevalence_f(PP_a = param_pred, 
                   data_a = nhanes_bfda2, strata_a = STRATA2, 
                   RS_a = RS2)
P3 <- prevalence_f(PP_a = param_pred, 
                   data_a = nhanes_bfda3, strata_a = STRATA3, 
                   RS_a = RS3)
P4 <- prevalence_f(PP_a = param_pred, 
                   data_a = nhanes_bfda4, strata_a = STRATA4, 
                   RS_a = RS4)


#######################################
########################### Get numbers

colnames(P1$prev_mat[["limit_30"]])

# obesity
round(mean(P1$prev_mat[["limit_30"]][,"f-1959-63"]), 1)
round(quantile(P1$prev_mat[["limit_30"]][,"f-1959-63"], 
               probs = c(2.5, 97.5)/100), 1)

round(mean(P1$prev_mat[["limit_30"]][,"f-1984-88"]), 1)
round(quantile(P1$prev_mat[["limit_30"]][,"f-1984-88"], 
         probs = c(2.5, 97.5)/100), 1)

round(mean(P1$prev_mat[["limit_30"]][,"m-1984-88"]), 1)
round(quantile(P1$prev_mat[["limit_30"]][,"m-1984-88"], 
               probs = c(2.5, 97.5)/100), 1)

# severe obesity
round(mean(P1$prev_mat[["limit_40"]][,"f-1959-63"]), 1)
round(quantile(P1$prev_mat[["limit_40"]][,"f-1959-63"], 
               probs = c(2.5, 97.5)/100), 1)

round(mean(P1$prev_mat[["limit_40"]][,"f-1984-88"]), 1)
round(quantile(P1$prev_mat[["limit_40"]][,"f-1984-88"], 
               probs = c(2.5, 97.5)/100), 1)

round(mean(P1$prev_mat[["limit_40"]][,"m-1959-63"]), 1)
round(quantile(P1$prev_mat[["limit_40"]][,"m-1959-63"], 
               probs = c(2.5, 97.5)/100), 1)

round(mean(P1$prev_mat[["limit_40"]][,"m-1984-88"]), 1)
round(quantile(P1$prev_mat[["limit_40"]][,"m-1984-88"], 
               probs = c(2.5, 97.5)/100), 1)



#######################################
############################ Draw plots

# obesity
bayesplot::color_scheme_set("blue")
F1 <- graphs_prev_f(prev_a  = P1, 
                    bmi_l_a = "limit_30", 
                    sex_a   = "f", 
                    TITLE_a="A- Females", 
                    xlim_L  = 25, xlim_U = 66,
                    present_L = 35, present_U = 66)
M1 <- graphs_prev_f(prev_a  = P1, 
                    bmi_l_a = "limit_30", 
                    sex_a   = "m", 
                    TITLE_a="B- Males", 
                    xlim_L  = 25, xlim_U = 66,
                    present_L = 35, present_U = 66)
# severe obesity
bayesplot::color_scheme_set("red")
F2 <- graphs_prev_f(prev_a  = P1, 
                    bmi_l_a = "limit_40", 
                    sex_a   = "f", 
                    TITLE_a="", 
                    xlim_L  = 0, xlim_U = 23,
                    present_L = 4, present_U = 15)
M2 <- graphs_prev_f(prev_a  = P1, 
                    bmi_l_a = "limit_40", 
                    sex_a   = "m", 
                    TITLE_a="", 
                    xlim_L  = 0, xlim_U = 23,
                    present_L = 2, present_U = 11)

gridExtra::grid.arrange(F1, M1,F2, M2, ncol=2)

ggsave(plot = gridExtra::grid.arrange(F1, M1,F2, M2, ncol=2),
       filename = paste0("../ms/figs/obesityAt", param_pred$age_proj, ".png"),
       device = "png",
       width = 10, height = 10, dpi = 1000)


################################################
############################### MODEL COMPARISON

Compare_30 <- rbindlist(list(full=store_prevalence_f(P1$prev_mat[["limit_30"]]),
                             sex_only=store_prevalence_f(P2$prev_mat[["limit_30"]]),
                             oldcoh_rmvd=store_prevalence_f(P3$prev_mat[["limit_30"]]),
                             ws_w_red=store_prevalence_f(P4$prev_mat[["limit_30"]])
                             ), 
                        idcol="model")
Compare_30 <- dcast(Compare_30, cohort_name ~ model, value.var = "p")
Compare_40 <- rbindlist(list(full=store_prevalence_f(P1$prev_mat[["limit_40"]]),
                             sex_only=store_prevalence_f(P2$prev_mat[["limit_40"]]),
                             oldcoh_rmvd=store_prevalence_f(P3$prev_mat[["limit_40"]])), 
                        idcol="model")
Compare_40 <- dcast(Compare_40, cohort_name ~ model, value.var = "p")

Compare_30[,sex:=substr(cohort_name, 1, 1)]
Compare_40[,sex:=substr(cohort_name, 1, 1)]


xlsx::write.xlsx(Compare_30, 
                 file = paste0("../tables_summary/prev_obese",param_pred$age_proj,".xlsx"))
xlsx::write.xlsx(Compare_40, 
                 file = paste0("../tables_summary/prev_severeobese55",param_pred$age_proj,".xlsx"))


# plot(Compare_30[sex=="f", full], pch=20, 
#      axes=F, ylim=c(35,60), 
#      xlab="Year of birth", 
#      ylab="Obesity prevalence")
# axis(2, las=2, tck=0.03)
# axis(1, at = 1:length(Compare_30[sex=="f", full]),
#      labels = Compare_30[sex=="f", cohort_name ]) 
# points(Compare_30[sex=="f", oldcoh_rmvd], pch=20, col="red")
# mtext("Females", side = 3)
# 
# plot(Compare_30[sex=="m", full], pch=20, 
#      axes=F, ylim=c(25,60), 
#      xlab="Year of birth", 
#      ylab="Obesity prevalence")
# axis(2, las=2, tck=0.03)
# axis(1, at = 1:length(Compare_30[sex=="f", full]),
#      labels = Compare_30[sex=="f", cohort_name ]) 
# points(Compare_30[sex=="m", oldcoh_rmvd], pch=20, col="red")
# mtext("Males", side = 3)



###################################################
############################ Oldest cohorts removed

# obesity
bayesplot::color_scheme_set("blue")
F1 <- graphs_prev_f(prev_a  = P3, 
                    bmi_l_a = "limit_30", 
                    sex_a   = "f", 
                    TITLE_a="A- Females", 
                    xlim_L  = 25, xlim_U = 66,
                    present_L = 35, present_U = 65)
M1 <- graphs_prev_f(prev_a  = P3, 
                    bmi_l_a = "limit_30", 
                    sex_a   = "m", 
                    TITLE_a="A- Males", 
                    xlim_L  = 25, xlim_U = 66,
                    present_L = 35, present_U = 65)
# severe obesity
bayesplot::color_scheme_set("red")
F2 <- graphs_prev_f(prev_a  = P3, 
                    bmi_l_a = "limit_40", 
                    sex_a   = "f", 
                    TITLE_a="", 
                    xlim_L  = 0, xlim_U = 23,
                    present_L = 4, present_U = 15)
M2 <- graphs_prev_f(prev_a  = P3, 
                    bmi_l_a = "limit_40", 
                    sex_a   = "m", 
                    TITLE_a="", 
                    xlim_L  = 0, xlim_U = 23,
                    present_L = 2, present_U = 11)

gridExtra::grid.arrange(F1, M1,F2, M2, ncol=2)

ggsave(plot = gridExtra::grid.arrange(F1, M1,F2, M2, ncol=2),
       filename = paste0("../ms/figs_supApp/obesityAt", param_pred$age_proj, "_oldest_rmvd.png"),
       device = "png",
       width = 10, height = 10, dpi = 1000)

