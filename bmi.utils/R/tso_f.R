#' Compute weighted total time spent obese by members of a stratum
#' 
#' For each limit \code{l} in \code{limit_a}, the time spent above \code{l} (weighted using
#' survey weights) is computed for each individual of  \code{stratum_a} in each posterior sample. Results for each element of
#' \code{limit_a} are the stacked. The matrix returned thus has
#' \code{N_individuals x length(limit)} rows and \code{N_iterations_a} columns.
#' 
#' @param stratum_a ids of individuals for which proportion should be computed.
#' @param data_a  data.table of individuals studied (contains \code{seqn}, \code{weight}).
#' @param result_set_a set of parameters analyzed.
#' @param limit_a vector of lower limits of the obesity categories investigated.
#' 
#' @return A matrix. 
#' @export
tso_f = function(stratum_a, data_a, result_set_a, limit_a, N_iterations_a=1e4L){
  
  # original data used in bfda
  id <- unique( data_a[stratum==stratum_a, .(seqn, weight)] )
  
  N_indiv <- id[,.N] # number of individuals in stratum
  W <- id[,weight]   # vector of survey weights
  S <- id[,seqn]     # vector of seqn 
  
  
  # retrieve available ages
  age_mat <-  load_results_f(stratum_a = stratum_a, 
                             RS_a = result_set_a, 
                             res_type_a = "short",
                             var_a="param_uc", 
                             var_type_a="mat")$param.uc[,1,1]$tau
  # remove ageMin-1
  age_mat <- age_mat[age_mat != (result_set_a$ageMin-1)]
  
  # determine ages that were imputed
  missing_ages_loc <- setdiff(result_set_a$ageMin:result_set_a$ageMax, age_mat)
  
  if(length(missing_ages_loc)>0){
    all_variables <- c(paste0("Zout_age", age_mat),
                       paste0("withImput_Zout_age", missing_ages_loc))
    # reorder according to age
    all_variables_order <- as.numeric(gsub("[[:alpha:]]+|_", "", all_variables))
    all_variables_order <- order(all_variables_order)
    all_variables <- all_variables[all_variables_order]
  }else{
    all_variables <- paste0("Zout_age", age_mat)
  }
  
  folder <- paste0("../results/", 
                   core_res_name_f(result_set_a), 
                   "_long/", stratum_a, "/")
  
  # create matrix with same dim as mcmc_list, which are the weights
  WEIGHTS <- matrix(data  = W,
                    nrow  = N_indiv,
                    ncol  = N_iterations_a,
                    byrow = F)
  
  # avoid using fread for parallelization issue : we avoid using load_results_f
  mcmc_list <- lapply(X = paste0(folder, all_variables, ".csv"),
                      FUN = readr::read_csv, col_names = F, 
                      col_types = paste0(rep("d", N_iterations_a), collapse = "") ) 
  
  if (!all(sapply(mcmc_list, function(x){nrow(x)==N_indiv }))){
    stop("incorrect dimension mcmc")}
  
  names(mcmc_list) <- all_variables
  
  all_results <- list() 
  
  for(LIM in limit_a){
    all_results[[as.character(LIM)]] <- Reduce('+',
                                               lapply(mcmc_list, 
                                                      function(x){1L*(x > LIM)}))
  }
  
  weighted_time_spent_abv_lim <- lapply(all_results, 
                                        function(x){as.data.table(WEIGHTS*x)} )
  
  sapply(weighted_time_spent_abv_lim,  
         function(x){setnames(x, old = names(x),
                              new=paste0("iter", seq_along(x) ))
           return(0)
         })
  
  sapply(weighted_time_spent_abv_lim,  
         function(x){ x[, `:=`(weight = W, seqn = S) ]; return(0)
         })
  
  sapply(weighted_time_spent_abv_lim,  
         function(x){ setcolorder(x, c("seqn", "weight")); return(0)}
  )
  
  weighted_time_spent_abv_lim <- rbindlist(weighted_time_spent_abv_lim, 
                                           idcol = "limit")
  
  # weighted_time_spent_abv_lim[,table(limit)]   # check
  
  return(weighted_time_spent_abv_lim)
}


