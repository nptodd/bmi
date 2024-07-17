#' Load results from BFDA
#'
#' Load results from the folders storing the results of the functional data analysis
#'
#' @param stratum_a vector of strata for which results are required
#' @param RS_a set of parameters analyzed
#' @param res_type_a either \code{long} (individual level posterior samples) 
#'       or \code{short} (e.g. mean posterior values)
#' @param var_a the variable to be retrieved  
#' @param var_type_a the file format (either \code{mat} or \code{csv})
#' 
#' @return Either data.table of results or list returned by \code{R.matlab::readMat}
#' @export
#' @importFrom data.table :=
load_results_f = function(stratum_a, 
                          RS_a, 
                          res_type_a="short", 
                          var_a, 
                          var_type_a="csv"){
  
  if(! res_type_a %in% c("short", "long")){ stop("Incorrect type of results") }
  if(! var_type_a %in% c("csv", "mat")){    stop("Incorrect file type")  }
  
  path = paste0("../results/",  core_res_name_f(RS_a), "_", res_type_a)
  path = paste0(path, "/", stratum_a, "/", var_a, ".", var_type_a)
  
  if  (!file.exists(path) & grepl("^Zout_age", var_a) ){
    path =  gsub(pattern = "Zout_age", 
                 replacement = "withImput_Zout_age", 
                 x = path, 
                 fixed = "T")
  }

  if(var_type_a == "mat") {
    
    return( R.matlab::readMat(path) )  
    
  } else { # csv file
    
    cat("......Reading ", path, "......\n")
    res = fread(path)
    
    # original data used in bfda
    nhanes_data_loc = fread(path_data_f(RS_a, stratum_a ))
    
    # recursive call to load_results_f
    param_uc_loc = load_results_f(stratum_a, RS_a, res_type_a="short", 
                                  var_a="param_uc", var_type_a="mat")$param.uc[,1,1]  
    
    if(res_type_a=="short"){
      
      setnames(res, names(res), paste0("seqn", nhanes_data_loc[, unique(seqn)]) )
      
      TAU <- as.vector(param_uc_loc$tau)
      
      res[, `:=`(tau=TAU)]
      
      setkey(res, tau)
      
      setcolorder(res, "tau")

    }  else{ # long results, i.e. mcmc iterations
      
      setnames(res, 
               old = names(res), 
               new = paste0("iter", seq_along(res)) )
      
      SEQN <- nhanes_data_loc[, unique(seqn)] 
      
      res[, `:=`(seqn=SEQN)]
      
      setcolorder(res, "seqn")
      
    }
    
    return(res)  
  }
}