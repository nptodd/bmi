#' Plot variance-covariance matrix for each stratum
#' 
#' @param strata_a  stratum individuals belong to .
#' @param result_set_a set of parameters analyzed.
#' 
#' @return None.
#' @export
analyze_sigma_f=function(strata_a, RS_a){
  
  age_mat = lapply(X = strata_a, 
                   FUN = load_results_f, 
                   RS_a=RS_a,
                   res_type_a = "short",
                   var_a="param_uc",
                   var_type_a = "mat")
  age_mat <- lapply(age_mat, 
                    function(ages){ ages$param.uc[,1,1]$tau }   )
  
  
  out_grid_l = lapply(X = strata_a, 
                      FUN = load_results_f, 
                      RS_a=RS_a,
                      res_type_a = "short",
                      var_a="out_ucgrid_reduced",
                      var_type_a = "mat") 
  
  sigma_mat = lapply(X = out_grid_l, 
                     FUN = function(O){ O$`out.ucgrid.reduced`[,,1][["Sigma"]] } )
  sigmaCI1_mat = lapply(X = out_grid_l, 
                        FUN = function(O, b){ O$`out.ucgrid.reduced`[,,1][["Sigma.CL"]][,b] }, b=1)
  sigmaCI2_mat = lapply(X = out_grid_l, 
                        FUN = function(O, b){ O$`out.ucgrid.reduced`[,,1][["Sigma.UL"]][,b] }, b=2)
  
  names(sigma_mat) <- names(sigmaCI1_mat) <- 
    names(sigmaCI2_mat) <-  names(age_mat) <- strata_a
  
  
  plot_corr=function(strat_a, add=F){
    
    cor_loc <-  cov2cor(sigma_mat[[strat_a]])
    
    dimnames(cor_loc) = list(age_mat[[strat_a]], 
                             age_mat[[strat_a]])
    
    # exclude ageMin - 1
    cor_loc<- cor_loc[ dimnames(cor_loc)[[1]]%in% RS_a$ageMin:RS_a$ageMax,
                       dimnames(cor_loc)[[2]]%in% RS_a$ageMin:RS_a$ageMax]
    
    corrplot::corrplot(cor_loc, method="color", type="lower", tl.col = "black", tl.cex = 0.7)
    mtext(text =  strat_a, side = 3, line=-1, at = 20, font = 2)
    mtext(text =  paste0("(N = ", nhanes_bfda_unique[stratum==strat_a,.N],")") , 
          side = 3, line=-3, at = 20, font = 2)
  }
  
  # test
  # plot_corr("m-h-hsl-0")
  # plot_corr("m-b-hsl-1", add = T)
  
  do.call('prod',  mapply(plot_corr, strat_a=strata_a))
}
