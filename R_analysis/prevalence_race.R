# list of parameters
param_pred = list(
  age_proj = 55,        # age at which prediction is needed
  age_span = 5,         # age span of cohort groups
  bmi_limit = c(30, 40) # which obesity category we project
)

races_dt = data.table(col = c("navy", "red", "orange", "forestgreen"), 
                      race = c("w", "h", "b", "o"), 
                      leg = c("White", "Hispanic", "Black", "Other"),
                      eps = c(-0.2, 0.2, 0, 0))


P1_race <- prevalence_f(PP_a = param_pred, 
                        data_a = nhanes_bfda1, strata_a = STRATA1, 
                        RS_a = RS1, keyby_a = c("sex", "cob", "race"))

P2_race <- prevalence_f(PP_a = param_pred,
                        data_a = nhanes_bfda2, strata_a = STRATA2,
                        RS_a = RS2, keyby_a = c("sex", "cob", "race"))

P3_race <- prevalence_f(PP_a = param_pred, 
                        data_a = nhanes_bfda3, strata_a = STRATA3, 
                        RS_a = RS3, keyby_a = c("sex", "cob", "race"))



Compare_30_race <- rbindlist(list(full=store_prevalence_f(P1_race$prev_mat[["limit_30"]]),
                                  # sex_only=store_prevalence_f(P2_race$prev_mat[["limit_30"]]),
                                  oldcoh_rmvd=store_prevalence_f(P3_race$prev_mat[["limit_30"]])), 
                             idcol="model")
Compare_30_race <- dcast(Compare_30_race, cohort_name ~ model, value.var = c("p", "p2.5", "p97.5"))


Compare_30_race[, sex:= substr(cohort_name, 1,1)]
Compare_30_race[, coh_beg:= as.numeric( substr(cohort_name, 3,6) )]
Compare_30_race[, coh_end:= as.numeric( paste0(19, substr(cohort_name, 8,9)) )]
Compare_30_race[, cohort:= substr(cohort_name, 3,9)]
Compare_30_race[, cohort_mid:= (coh_beg+coh_end)/2]
Compare_30_race[, race:=substr(cohort_name, 11,11)]
Compare_30_race[races_dt, on=.(race), col:=col]
Compare_30_race[races_dt, on=.(race), eps_loc:=eps]

Compare_30_race[, cohort_mid_plot:= cohort_mid+eps_loc]
Compare_30_race[, eps_loc := NULL]

Compare_30_race <- Compare_30_race[race!="o"]
races_dt <- races_dt[race!="o"]


png(paste0("../ms/figs/obesityAt", param_pred$age_proj, "_race.png"),
    width = 1200, height = 760)
par(mfrow=c(1,2), mar=c(7, 5, 2, 2))
sapply(c("f", "m"),  
       function(sex_a){
         Hmisc::errbar( x = Compare_30_race[sex==sex_a, cohort_mid_plot], 
                        y = Compare_30_race[sex==sex_a, p_full], 
                        yplus = Compare_30_race[sex==sex_a, p97.5_full],
                        yminus = Compare_30_race[sex==sex_a, p2.5_full],
                        col = Compare_30_race[sex==sex_a, col], 
                        errbar.col = Compare_30_race[sex==sex_a, col], cap=0,
                        cex=2, lwd=2,
                        ylim=c(20, 90), axes=F, xlab="", ylab="")
         
         abline(h=10*(2:9), lty=2, col="darkgrey")
         
         # !is.na(p_full) in case of mismatch between full and
         # older cohorts removed models
         axis(1, at = Compare_30_race[!is.na(p_full), cohort_mid], 
              labels=Compare_30_race[!is.na(p_full), cohort], 
              las=2, cex.axis=1.5)
         mtext("Year of birth", 1, 6, cex = 1.5)
         
         axis(2, las=2, tck=0.03, at=10*(2:9), cex.axis=1.5)
         mtext(paste0("Proportion obese at ", P1_race$pp$age_proj), 2, 3, cex=1.5)
         
         segments(x0 =P1_race$coh_l$loc,
                  x1 =P1_race$coh_l$loc,
                  y0=10, y1=78,  col="darkgrey")
         
         txt_loc <- ifelse(sex_a=="f", "Females", "Males")
         mtext(txt_loc, side = 3, font=2, cex=2)
         
         legend(1950, 92, legend=races_dt[,leg], 
                col = races_dt[,col],  
                x.intersp = 0.8, cex=2.5,
                pch=20, pt.cex=4, bty="n", ncol = 1)
         
         return(0) 
       }
)
dev.off()


# plot(Compare_30_race[,.(p_full, p_oldcoh_rmvd)], 
#      col=Compare_30_race[,col], pch=ifelse(Compare_30_race[,sex=="f"], 15, 16),
#      xlab="Prev. Obesity 55, stratified model", 
#      ylab = "Prev. Obesity 55, untratified model")
# legend("topleft", legend=races_dt[,leg],
#        col = races_dt[,col], pch=15, pt.cex=2, bty="n", ncol = 2)
# abline(0, 1, lwd=1.5, col="darkgrey")               



xlsx::write.xlsx(Compare_30_race, 
                 file = paste0("../tables_summary/prev55_race.xlsx"))

Compare_30_race[coh_beg==1979 & sex=="f",
                .(cohort_name, p_full, p2.5_full, p97.5_full)]



