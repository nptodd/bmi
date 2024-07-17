#' Returns the core character string of the files of results' names
#'
#' @param RS_a set of parameters analyzed.
#' @param pt_a separator.
#' 
#' @return A character string.
core_res_name_f = function(RS_a, pt_a="/"){
  
  if(! RS_a$bmi %in% c("Uncor", "Deb1", "Deb2")){stop("Incorrect BMI argument")}
  
  return(
    paste0(RS_a$typeStrata, pt_a, 
           "ages", RS_a$ageMin, "_", RS_a$ageMax, pt_a,
           "cohorts", RS_a$coh, pt_a, 
           RS_a$method, pt_a,
           RS_a$log, pt_a,
           "bmi", RS_a$bmi,
           "_", RS_a$cov, "Cov", 
           "_w", RS_a$w, 
           "_ws", RS_a$ws)
  )
}



#' Return the path to a csv file used by bfda
#'
#' @param RS_a set of parameters analyzed.
#' @param stratum_a vector of strata for which imputation is potentially needed. If \code{NULL}, all strata.
#' 
#' 
#' @return A character string.
#'
#' @export
path_data_f=function(RS_a, stratum_a=NULL){
  
  if( is.null(stratum_a) ){
    return(
      paste0("../data/for_matlab/", 
             RS_a$typeStrata, 
             "/ages", RS_a$ageMin, "_", RS_a$ageMax,
             "/cohorts", RS_a$coh)  )
  } else {
    return(
      paste0("../data/for_matlab/", 
             RS_a$typeStrata, 
             "/ages", RS_a$ageMin, "_", RS_a$ageMax,
             "/cohorts", RS_a$coh, 
             "/", stratum_a, ".csv")  )
  }
}


#' Return the name of the overweight/obesity category passed as an argument
#'
#' @param bmi_limit_a BMI limit (25, 30, 35, 40).
#' 
#' @return A character string.
#' @export
make_char_pred_f = function(bmi_limit_a){
  c("Overweight + obesity", "Obesity", "Obesity class II+III", 
    "Severe obesity")[c(25,30,35,40) %in% bmi_limit_a]
}



#' Create sensible cohort breaks
#' 
#' Avoid mixing cohorts already observed at \code{age_a} and cohorts for 
#' which the estimate of obesity at \code{age_a} given is a true prediction
#'
#' @param age_a the age for which projection is required.
#' @param age_span_a the desired size of birth cohorts returned.
#' @param data_a data.table of individuals studied.
#' 
#' @return A list of 4 elements: 
#' \code{loc} (last observed cohort); 
#' \code{fno_pos} position of the first non observed cohort (in \code{beg} and \code{end});
#' \code{beg} : vector of  cohorts' starting years of birth;
#' \code{end} : vector of  cohorts' ending years of birth.
#' @export
make_breaks_f = function(age_a, age_span_a, data_a){
  
  # last cohort observed at age age_a
  last_obs_coh = data_a[age==age_a, max(yob)] 
  
  miny = data_a[,min(yob)]
  maxy = data_a[,max(yob)]
  
  cohorts_b = (-100:100)*age_span_a+last_obs_coh+1 # generate excess beginning cohorts
  cohorts_b = cohorts_b[cohorts_b %in% miny:maxy]
  cohorts_b[1]  <- miny     # make sure starting point is miny
  
  if(cohorts_b[length(cohorts_b)] != (maxy - age_span_a +1) ) {
    cohorts_b <- cohorts_b[-length(cohorts_b)] }
  
  cohorts_e <- c( (cohorts_b-1)[2:length(cohorts_b)], maxy)
  
  pos_first_not_obs =  which(cohorts_b==(last_obs_coh+1)) # group the 1st non observed cohort is in 
  
  # cohorts_b = rev(cohorts_b) # we want older cohort on the left of graphs
  # cohorts_e = rev(cohorts_e)
  
  return(list(loc=last_obs_coh, 
              fno_pos = pos_first_not_obs, 
              beg=cohorts_b, 
              end=cohorts_e ))
}



#' Create a vector of seqn according to cohort of birth
#' 
#' @param beg_a first year of birth included.
#' @param end_a last year of birth included.
#' @param sex_a restrict to this sex if given.
#' @param race_a restrict to this race/ethnicity if given.
#' @param educ_a restrict to this education group if given.
#' 
#' @return Vector of seqn of individuals born \code{beg_a}-\code{end_a}.
#' @export
seqn_subset_f = function(beg_a, end_a, 
                         sex_a = NULL, 
                         race_a = NULL,
                         educ_a = NULL,
                         data_a){
  res <- data_a[yob %in% beg_a:end_a]
  
  if(!is.null(sex_a)){res <- res[sex %in% sex_a]}
  if(!is.null(race_a)){res <- res[race %in% race_a]}
  if(!is.null(educ_a)){res <- res[educ %in% educ_a]}
  
  return(res[, unique(seqn)])
}



#' Compute time spent obese in sample restricted to \code{seqn_a}
#'
#' @param seqn_a ids of individuals for which proportion should be computed.
#' @param timeSpentObese_a matrix of times spent obese (returned by \code{\link{tso_f}}).
#' 
#' @return Vector of times spent obese in each posterior sample.
#' @export
timeSpent_f = function(seqn_a, timeSpentObese_a){
  
  timeSpent_loc <- timeSpentObese_a[seqn %in% seqn_a]
  
  wtot <- timeSpent_loc[,sum(weight)]
  
  iter_col = grep("^iter", names(timeSpentObese_a), value=T) # returns name of iter columns
  
  res <-  t(timeSpent_loc[ , lapply(.SD, sum), .SDcols = iter_col]/wtot)
  
  return(res)
}

