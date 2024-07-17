
############################################################################
################ Computation of individual times spent obese ###############


# compute mean posterior and uncertainty interval 
# for tso, by group, and plot graphs.
# Arguments ... are for seqn_subset_f
get_tso_f <- function(bmi_l_a = 30,

                      race_a = NULL,
                      coh_l_a = coh_l,
                      result_set_a = RS1,
                      timeSpentObese_a = timeSpentObese,
                      data_a = nhanes_bfda1,

                      TITLE_a="",
                      xlim_L=4, xlim_U=17,
                      present_L=NULL, present_U=NULL,
                      id_col_name_a=NULL,
                      ...){

  if(is.null(present_L))  present_L <- xlim_L + (xlim_U-xlim_L)/5
  if(is.null(present_U))  present_U <- xlim_U - (xlim_U-xlim_L)/5

  Seqn = mapply(seqn_subset_f, coh_l_a$beg, coh_l_a$end,
                MoreArgs = list(data_a = data_a, race_a=race_a, ...))

  names(Seqn) = paste0(coh_l_a$beg, "-", (coh_l_a$end-1900))

  Seqn <- rev(Seqn) # for order

  timeSpent_final <- do.call(cbind,
                             lapply(Seqn,
                                    timeSpent_f,
                                    timeSpentObese_a=timeSpentObese_a[limit==bmi_l_a]) )

  colnames(timeSpent_final) <- names(Seqn)

  GRAPH <- bayesplot::mcmc_areas(timeSpent_final,
                                 point_est = c("mean"),
                                 prob=0.50, prob_outer=0.95) +
    geom_segment(aes(x    = present_L,
                     xend = present_U,
                     y    = coh_l_a$fno_pos-0.1,
                     yend = coh_l_a$fno_pos-0.1), colour = "darkgrey") +
    coord_flip()

  if(bmi_l_a == 30){
    xlab_loc <- paste0("Years obese ",
                       result_set_a$ageMin, "-",
                       result_set_a$ageMax)
  } else if (bmi_l_a == 40){
    xlab_loc <- paste0("Years severely obese ",
                       result_set_a$ageMin, "-",
                       result_set_a$ageMax)
  } else{
    xlab_loc <- "unknown limit"
  }

  GRAPH <- GRAPH + labs(title = TITLE_a,
                        subtitle = make_char_pred_f(bmi_l_a),
                        x= xlab_loc,
                        y= "Year of birth") +
    theme(axis.text.x = element_text(face = "bold", hjust=0, angle=315),
          axis.title.y = element_text(face = "bold", colour = "dimgrey"),
          axis.title.x = element_text(face = "bold", colour = "dimgrey"),
          plot.title = element_text(face="bold")) +
    xlim(xlim_L, xlim_U)


  results_dt <- data.table(cohort=paste0("Cohort ", names(Seqn)),
                           cohort_short=names(Seqn),
                           prev=NA_real_,
                           prev2.5=NA_real_,
                           prev97.5=NA_real_)
  if(!is.null(id_col_name_a)){results_dt[,id_col:=id_col_name_a]}

  for(I in seq_along(Seqn)){

    results_dt[I, `:=`(prev     = mean(timeSpent_final[,I]),
                       prev2.5  = quantile(timeSpent_final[,I], 2.5/100),
                       prev97.5 = quantile(timeSpent_final[,I], 97.5/100)
    )]
  }
  return(list(dt=results_dt, graph=GRAPH))
}


############################### ESTIMATES BY SEX

bayesplot::color_scheme_set("blue")
F1 <- get_tso_f(sex_a="f", bmi_l_a = 30,  TITLE_a="A- Females", id_col_name_a = "females_obese")
M1 <- get_tso_f(sex_a="m", bmi_l_a = 30, TITLE_a="B- Males", id_col_name_a = "males_obese")
bayesplot::color_scheme_set("red")
F2 <- get_tso_f(sex_a="f", bmi_l_a = 40, xlim_L = 0, xlim_U = 5.5, id_col_name_a = "females_severeob")
M2 <- get_tso_f(sex_a="m", bmi_l_a = 40, xlim_L = 0, xlim_U = 5.5, id_col_name_a = "males_severeob")

gridExtra::grid.arrange(F1$graph, M1$graph, F2$graph, M2$graph, ncol=2)

ggsave(plot = gridExtra::grid.arrange(F1$graph, M1$graph, F2$graph, M2$graph, ncol=2),
       filename = paste0("../ms/figs/tso",
                         PP$age_proj, RS1$ageMin, "_",RS1$ageMax, ".png"),
       device = "png",
       width = 10,
       height = 10,
       dpi = 1000)

xlsx::write.xlsx(rbindlist(list(F1$dt, M1$dt, F2$dt, M2$dt)), 
                 paste0("../tables_summary/tso",
                        PP$age_proj, RS1$ageMin, "_",RS1$ageMax, ".xlsx"))

# quick alternative plot
error.bar <- function(x, y, upper, lower=upper, length=0.0,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

ALL = rbindlist(l = list(F1$dt, M1$dt, F2$dt, M2$dt))
ALL[, `:=`(sex=substr(id_col, 1, 1),
           col="blue",
           beg_year=as.numeric(substr(cohort_short, 1, 4)))]
ALL[grepl("severe", id_col), col:="red"]
pdf("../ms/figs/tso25_55_alt.pdf", width = 20, height = 13)
par(mfrow=c(1,2))
sapply(c("f", "m"), FUN = function(sex_loc){ 
  plot(ALL[sex==sex_loc, .(beg_year, prev)], 
       col=ALL[sex==sex_loc,col],
       pch=20, cex=1.2, 
       xlab="Year of birth", ylab="Years obese 25-50", cex.lab=1.3,
       axes=F,
       ylim=c(0, 12))
  axis(1, at=ALL[sex==sex_loc, beg_year], cex.lab=1.4,
       labels = ALL[sex=="m", cohort_short])
  axis(2, las=2, tck=0.02)
  error.bar(x= ALL[sex==sex_loc, beg_year], 
            y = ALL[sex==sex_loc, prev], 
            upper = ALL[sex==sex_loc, prev97.5-prev],
            lower = ALL[sex==sex_loc, prev-prev2.5], 
            col=ALL[sex==sex_loc, col])
  if(sex_loc=="m"){TXT="Males"}else{TXT="Females"}
  mtext(TXT, side = 3, line = -1, adj = 0, font=2)
})
dev.off()



############################### RACE SPECIFIC ESTIMATES

races_dt = data.table(col = c("navy", "red", "orange", "forestgreen"), 
                      race = c("w", "h", "b", "o"), 
                      leg = c("White", "Hispanic", "Black", "Other"),
                      eps = c(-0.2, 0.2, 0, 0))

bayesplot::color_scheme_set("blue")

# fw = get_tso_f(race_a="w", sex_a="f", bmi_l_a=30, xlim_L=0, xlim_U=20)

tso_race <- c(lapply(races_dt[, race], get_tso_f, sex_a="f", bmi_l_a=30, xlim_L=0, xlim_U=20),
              lapply(races_dt[, race], get_tso_f, sex_a="m", bmi_l_a=30, xlim_L=0, xlim_U=20))
names(tso_race) <- c(paste0("f-", races_dt[, race]), paste0("m-", races_dt[, race]))

tso_race <- lapply(tso_race, function(x){x$dt})
tso_race <- rbindlist(tso_race, idcol = "id")

tso_race[, cohort:=gsub("^Cohort ", "", cohort)]
tso_race[,coh_beg:= as.numeric(substr(cohort, 1, 4))]
tso_race[,coh_end:= 1900+as.numeric(substr(cohort, 6, 7))]
tso_race[, cohort_mid:= (coh_beg+coh_end)/2]
tso_race[,`:=`(sex=substr(id, 1, 1),
                     race=substr(id, 3, 3))]
tso_race[races_dt, on=.(race), col:=col]
tso_race[races_dt, on=.(race), eps_loc:=eps]

tso_race[, cohort_mid_plot:= cohort_mid+eps_loc]
tso_race[, eps_loc := NULL]

setcolorder(tso_race, c("cohort", "sex", "race"))



png("../ms/figs_supApp/tso_race.png", width = 1200, height = 760)
par(mfrow=c(1,2), mar=c(7, 5, 2, 2))
sapply(c("f", "m"),  function(sex_a){
  Hmisc::errbar( x = tso_race[sex==sex_a, cohort_mid_plot], 
                 y = tso_race[sex==sex_a, prev], 
                 yplus = tso_race[sex==sex_a, prev97.5],
                 yminus = tso_race[sex==sex_a, prev2.5],
                 col = tso_race[sex==sex_a, col], 
                 errbar.col = tso_race[sex==sex_a, col], cap=0,
                 cex=2, lwd=2,
                 ylim=c(0, 22), axes=F, xlab="", ylab="")
  
  abline(h=5*(1:5), lty=2, col="darkgrey")
  
  axis(1, at = tso_race[, cohort_mid], 
       labels=tso_race[, cohort], 
       las=2, cex.axis=1.5)
  mtext("Year of birth", 1, 6, cex = 1.5)
  
  axis(2, las=2, tck=0.03, at=5*(0:4), cex.axis=1.5)
  mtext(paste0("Time spent obese"), 2, 3, cex=1.5)
  
  segments(x0 = coh_l$loc,
           x1 = coh_l$loc,
           y0=5, y1=16,  col="darkgrey")
  
  txt_loc <- ifelse(sex_a=="f", "Females", "Males")
  mtext(txt_loc, side = 3, font=2, cex=2)
  
  legend(1950, 19, legend=races_dt[,leg], 
         col = races_dt[,col],  
         x.intersp = 0.6, cex=2,
         pch=20, pt.cex=4, bty="n", ncol = 2)
  
  return(0) 
}
)
dev.off()


xlsx::write.xlsx(tso_race, 
                 file = paste0("../tables_summary/tso_race",
                               PP$age_proj, RS1$ageMin, "_",RS1$ageMax, ".xlsx"))


