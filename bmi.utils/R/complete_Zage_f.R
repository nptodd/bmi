#' Complete Zage series
#'
#' In case some ages were not attained and thus not reconstructed, then impute them.
#'
#' @param stratum_a vector of strata for which imputation is potentially needed.
#' @param RS_a set of parameters analyzed.
#' 
#' @return \code{0} for correct execution.
#'
#' @export
complete_Zage_f = function(stratum_a, RS_a){
  
  core_path_loc <- paste0("../results/", 
                          core_res_name_f(RS_a), 
                          "_long/", stratum_a, "/")

  age_mat <- load_results_f(stratum_a = stratum_a, 
                            RS_a = RS_a,
                            res_type_a = "short",
                            var_a = "param_uc",
                            var_type_a = "mat")
  
  age_mat <- as.vector(age_mat$param.uc[,1,1]$tau)
  
  full_age_range <- RS_a$ageMin:RS_a$ageMax
  
  missing_ages <- setdiff(full_age_range, age_mat)
  
  if (length(missing_ages)>0){
    
    for(mis_age in missing_ages){
      
      min_dist <- min(abs(age_mat-mis_age))
      
      closest_ages <- age_mat[abs(age_mat-mis_age)==min_dist]
      
      if(length(closest_ages)==1){
        
        mcmc_imput <- load_results_f(var_a = paste0("Zout_age", closest_ages),
                                     stratum_a=stratum_a, 
                                     RS_a=RS_a, 
                                     res_type_a="long",
                                     var_type_a = "csv")
        
      }else{  # 2 closest ages
        
        mcmc_list <- lapply(paste0("Zout_age", closest_ages),
                            FUN = load_results_f,
                            stratum_a = stratum_a,
                            RS_a=RS_a, 
                            res_type_a="long",
                            var_type_a = "csv")  
        
        # average closest ages
        mcmc_imput <- (mcmc_list[[1]]+mcmc_list[[2]])/2
        
      }
      
      path_loc <- paste0(core_path_loc, "withImput_Zout_age", mis_age, ".csv")
      
      # we remove seqn and iter names  for compatibility. Will be reloaded with non imputed data
      mcmc_imput[, seqn:=NULL]
      fwrite(mcmc_imput, path_loc, col.names = F)
    }
    
  }
  
  return(0) # correct execution
}