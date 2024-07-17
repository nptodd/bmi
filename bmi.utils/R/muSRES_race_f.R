#' Plot average posterior mean BMI functions for all races, given sex,
#' education and smoking status
#' 
#' Should only be run if strata are defined based on :
#' Sex, Race, Education and Smoking
#'
#' @param sex_a \code{"f"} or \code{"m"}
#' @param educ_a educational attainment
#' @param smok_a \code{0} or \code{1}
#' @param RS_a set of parameters analyzed
#' @param strata_a strata analyzed
#' 
#' @return None
#'
#' @export 
muSRES_race_f=function(sex_a, educ_a, smok_a, RS_a, 
                       strata_a, CI_a=F){
  
  if(RS_a$typeStrata!="SexRacEduSmo"){stop("Incorrect type of strata")}
  
  educ_v = c("cg", "scd", "hsl")
  educ_leg_v = c("College graduates", "Some college degree", "High school or less")
  
  strata_loc <- grep(paste0("-", educ_a, "-"),  strata_a, value = T)
  strata_loc <- strata_loc[substr(strata_loc, 1, 1)==sex_a]
  strata_loc <- strata_loc[grep(paste0(smok_a, "$"), strata_loc)]
  
  # graphical parameters
  races_dt = data.table(
    col = c("navy", "red","orange", "forestgreen", "pink"),
    race = c("w", "h", "b", "o", "bh"),
    leg = c( "White", "Hispanic", "Black",  "Other", "Bl. & Hisp."),
    add=c(F, T, T, T, T)
  )
  races_dt[, strata :=  paste0(sex_a, "-", race, "-", educ_a, "-", smok_a)]
  races_dt = races_dt[strata %in% strata_loc]
  
  # loading results
  age_mat = lapply(X = strata_loc, 
                   FUN = load_results_f, 
                   RS_a=RS_a,
                   res_type_a = "short",
                   var_a="param_uc",
                   var_type_a = "mat")
  age_mat <- lapply(age_mat, 
                    function(ages){ ages$param.uc[,1,1]$tau }   )
  
  out_grid_l = lapply(X = strata_loc, 
                      FUN = load_results_f, 
                      RS_a=RS_a,
                      res_type_a = "short",
                      var_a="out_ucgrid_reduced",
                      var_type_a = "mat") 
  
  mu_mat = lapply(X = out_grid_l, 
                  FUN = function(outgrid){
                    outgrid$`out.ucgrid.reduced`[,,1][["mu"]]} )
  muCI1_mat = lapply(X = out_grid_l, 
                     FUN = function(outgrid, b){
                       outgrid$`out.ucgrid.reduced`[,,1][["mu.CI"]][,b] }, b=1)
  muCI2_mat = lapply(X = out_grid_l, 
                     FUN = function(outgrid, b){
                       outgrid$`out.ucgrid.reduced`[,,1][["mu.CI"]][,b] }, b=2)
  
  names(mu_mat) <- names(muCI1_mat) <- names(muCI2_mat) <-  names(age_mat) <- strata_loc
  
  
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
           type="l", xlab="", ylab="", cex.lab=1.5, axes=F, ylim=c(20,37)) 
      abline(h = 18:40, 
             lty = 3, lwd=0.7, col = "darkgrey")
      abline(h = c(18.5, 25, 30, 40), 
             lty = 2, lwd=2, col = "darkgrey")
      axis(1, cex.axis=1.3); axis(2, las=2, tck=0.02, cex.axis=1.3)
    }
    
    if(CI){
      
      polygon(c(age_mat_loc, rev(age_mat_loc)),
              c(muCI1_mat_loc,rev(muCI2_mat_loc)),
              col    = adjustcolor(col_a, alpha.f = 0.3), 
              border = NA )
      
    }
  }
  
  do.call('prod',  mapply(plot_mu, strat_a=races_dt[,strata],
                          col_a=races_dt[, col], add=races_dt[,add], CI=CI_a))
  
  legend(24.5, 37, races_dt[,leg], box.lty = 0, col= races_dt[,col],
         cex=2,bg = "white",
         pch=15, pt.cex=3, ncol=2)
  
  mtext("Mean BMI", 2, 3, cex=2)
  mtext("Age", side = 1, line = 2.5, cex=2)
}
