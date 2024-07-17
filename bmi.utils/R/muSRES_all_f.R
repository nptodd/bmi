#' Plot all average posterior mean BMI functions for given sex 
#' 
#' Should only be run if strata are defined based on :
#' Sex, Race, Education and Smoking
#'
#' @param sex_a Either female (\code{"f"}) or male (\code{"m"})
#' @param RS_a set of parameters analyzed
#' @param strata_a strata analyzed
#' 
#' @return None
#'
#' @export 
muSRES_all_f=function(sex_a, RS_a, strata_a){
  
  if(RS_a$typeStrata!="SexRacEduSmo"){stop("Incorrect type of strata")}
  
  races_dt = data.table(
    col = c("navy", "red","orange", "forestgreen", "pink"),
    race = c("w", "h", "b", "o", "bh"),
    leg = c( "White", "Hispanic", "Black",  "Other", "Bl. & Hisp."),
    add=c(F, T, T, T, T) )
  
  educ_v = c("cg", "scd", "hsl")
  educ_leg_v = c("College graduates", "Some college degree", "High school or less")
  
  CI = F
  x_leg = 33
  y_leg = 25
  
  sex_m <- ifelse(sex_a=="m", "Males", "Females")
  
  par(mfrow=c(3,2),oma = c(1, 1 ,5, 0), mar=c(3,4,2,2))
  
  # loading results
  age_mat = lapply(X = strata_a, 
                   FUN = load_results_f, 
                   RS_a=RS_a,
                   res_type_a = "short",
                   var_a="param_uc",
                   var_type_a = "mat")
  
  age_mat <- lapply( age_mat, function(ages){ ages$param.uc[,1,1]$tau } )
  
  out_grid_l = lapply(X = strata_a, 
                      FUN = load_results_f, 
                      RS_a=RS_a,
                      res_type_a = "short",
                      var_a="out_ucgrid_reduced",
                      var_type_a = "mat") 
  
  mu_mat = lapply(X = out_grid_l, 
                  FUN = function(O){ O$`out.ucgrid.reduced`[,,1][["mu"]] } )
  muCI1_mat = lapply(X = out_grid_l, 
                     FUN = function(O, b){ O$`out.ucgrid.reduced`[,,1][["mu.CI"]][,b] },
                     b=1)
  muCI2_mat = lapply(X = out_grid_l, 
                     FUN = function(O, b){ O$`out.ucgrid.reduced`[,,1][["mu.CI"]][,b] },
                     b=2)
  
  names(mu_mat) <- names(muCI1_mat) <- names(muCI2_mat) <-  names(age_mat) <- strata_a
  
  
  plot_mu=function(strat_a, col_a="black", lwd_a=2, add=F, CI=F){
    
    age_mat_loc <-age_mat[[strat_a]]
    
    # only keep selected age range (remove first one (24) if reconstruction starts at 25)
    selected_ages <- which(age_mat_loc %in% RS_a$ageMin:RS_a$ageMax)
    
    age_mat_loc <- age_mat_loc[selected_ages]
    mu_mat_loc <-  mu_mat[[strat_a]][selected_ages]
    muCI1_mat_loc <- muCI1_mat[[strat_a]][selected_ages]
    muCI2_mat_loc <- muCI2_mat[[strat_a]][selected_ages]
    
    
    if(add){
      
      lines(age_mat_loc, mu_mat_loc, col=col_a, lwd=lwd_a)
      
    }else{
      
      plot(age_mat_loc, mu_mat_loc, col=col_a, lwd=lwd_a, 
           type="l", xlab="", ylab="", cex.lab=1.5, ylim=c(20, 36), axes=F) 
      mtext("Mean BMI", 2, 3)
      abline(h = 18:40, lty = 3, lwd=0.7, col = "darkgrey")
      abline(h = c(18.5, 25, 30, 40), lty = 2, lwd=2, col = "darkgrey")
      axis(1); axis(2, las=2)
      
    }
    
    if(CI){
      lines(age_mat_loc, muCI1_mat_loc, lty=3, lwd=lwd_a/0.9, col=col_a)
      lines(age_mat_loc, muCI2_mat_loc, lty=3, lwd=lwd_a/0.9, col=col_a)  }
  }
  
  
  for(Educ in seq_along(educ_v)){
    
    races_dt_loc1 <- copy(races_dt) # non-smokers
    races_dt_loc2 <- copy(races_dt) # smokers
    races_dt_loc1[, strata :=  paste0(sex_a, "-", race, "-", educ_v[Educ], "-", "0")]
    races_dt_loc2[, strata :=  paste0(sex_a, "-", race, "-", educ_v[Educ], "-", "1")]     
    
    races_dt_loc1 <- races_dt_loc1[strata %in% strata_a]
    races_dt_loc2 <- races_dt_loc2[strata %in% strata_a]
    
    
    ### Non-smokers
    do.call('prod',  mapply(plot_mu, 
                            strat_a=races_dt_loc1[, strata],
                            col_a=races_dt_loc1[, col], 
                            add=races_dt_loc1[,add], 
                            CI=CI))
    mtext("Non-smokers", side = 3, cex=1.4, at = 33, line = -3, font = 2)
    
    legend(x_leg, y_leg, races_dt_loc1[,leg], bty="n", col= races_dt_loc1[,col], cex=2.1,
           pch=15, pt.cex=2.4, x.intersp=0.6, y.intersp=0.9, ncol=2)
    
    mtext(educ_leg_v[Educ], cex = 1.4, line = 0, at = 25, adj = 0, font = 2)
    
    ### Smokers
    do.call('prod',  mapply(plot_mu, strat_a=races_dt_loc2[, strata],
                            col_a=races_dt_loc2[, col], add=races_dt_loc2[,add], CI=CI))
    mtext("Smokers", side = 3, cex=1.4, at = 29, line = -3, font = 2)
    
    legend(x_leg,y_leg, races_dt_loc2[,leg], bty="n", col= races_dt_loc2[,col],  cex=2,
           pch=15, pt.cex=2.4, x.intersp=0.6, y.intersp=0.9, ncol=2)
  }
  
  mtext("Age", side = 1, outer = TRUE, line=-0.5, cex = 1.4, font = 1, at=0.25+0.03)
  mtext("Age", side = 1, outer = TRUE, line=-0.5, cex = 1.4, font = 1, at=0.75+0.03)
  
}
