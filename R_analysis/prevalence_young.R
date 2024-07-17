# list of parameters
param_pred = list(
  age_proj = 35, # 55     # age at which prediction is needed
  age_span = 5,           # age span of cohort groups
  bmi_limit = c(30, 40)   # which obesity category we project
)

P1 <- prevalence_f(PP_a = param_pred, 
                   data_a = nhanes_bfda1, strata_a = STRATA1, 
                   RS_a = RS1)


#######################################
############################ Draw plots

# obesity
bayesplot::color_scheme_set("blue")
F1 <- graphs_prev_f(prev_a  = P1, 
                    bmi_l_a = "limit_30", 
                    sex_a   = "f", 
                    TITLE_a="A- Females", 
                    xlim_L  = 15, xlim_U = 53,
                    present_L = 35, present_U = 53)
M1 <- graphs_prev_f(prev_a  = P1, 
                    bmi_l_a = "limit_30", 
                    sex_a   = "m", 
                    TITLE_a="B- Males", 
                    xlim_L  = 15, xlim_U = 53,
                    present_L = 35, present_U = 53)
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
       filename = paste0("../ms/figs_supApp/obesityAt", param_pred$age_proj, ".png"),
       device = "png",
       width = 10, height = 10, dpi = 1000)


hist( P1$prev_mat[["limit_40"]][,"m-1943-48"])
round(mean(P1$prev_mat[["limit_40"]][,"m-1943-48"]), 1)
round(quantile(P1$prev_mat[["limit_40"]][,"m-1943-48"], 
               probs = c(2.5, 97.5)/100), 1)
