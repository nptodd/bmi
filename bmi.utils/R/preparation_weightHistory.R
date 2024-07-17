#' Region of the cohort x age plane measurements (current, 1yr or 10yrs ago) pertain to
#'
#' @param recall_a type of recall 
#' @param cohort_span_a cohorts considered
#' @param age_span_a age span considered
#' @param minYr_a first year of observation
#' @param maxYr_a last year of observation
#' @details \code{recall_a} = 0 (current_bmi), 1 (bmi 1y ago) or 10 (bmi 10y ago)
#' @return A matrix of booleans
#' @export
isObs_f = function(recall_a, minYr_a=1999, maxYr_a=2018,
                   cohort_span_a = 1940:1995, age_span_a = 15:60
                   ){
  
  # can somebody in birth cohort c_a be observed declaring
  # bmi at age a_a as its weight r_a years ago ?
  observ_f = function(c_a, a_a, r_a) {
    
    if(! r_a %in% c(0, 1, 10)){stop("Incorrect recall argument")}
    
    age_declar_a = a_a + r_a
    year_declar_a = c_a + a_a + r_a
    
    if ( (age_declar_a<16) | (age_declar_a <36 & r_a==10) ){
      return(FALSE)
    }
    
    if( (year_declar_a %in% (minYr_a-1):maxYr_a) & (age_declar_a %in% ages_study) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  }
  
  grid = expand.grid(a_a=age_span_a, c_a=cohort_span_a) # grid on which to apply observ_f
  
  is_observed  = 
    matrix(mapply(observ_f, grid$c_a, grid$a_a, r_a=recall_a),
           nrow=length(unique(grid$c_a)), 
           ncol=length(unique(grid$a_a)), 
           byrow = T)
  
  colnames(is_observed) = age_span_a
  rownames(is_observed) = cohort_span_a
  
  return(is_observed)
}



#' Estimation of the BMI(age, yob) surface using the different measures
#'
#' Estimation of the age x yob -> E(BMI | age, yob) mapping using either current BMI, BMI 1y ago or BMI 10y ago
#' @param recall_a type of recall 
#' @param sex_a  c("m", "f"), "m" or "f"
#' @param weighted_a if true, use survey weights
#' @param K_a dimension of each basis of the tensor product smooth
#' @param data_a continuous NHANES data
#' @param include_nhanesIII If true, NHANES III data is also used in the estimation
#' @param dataIII_a NHANES III data
#' @details Pregnant women are removed if recall_a == 0 (current BMI),  
#' because past weights are asked before pregnancy
#' @return A Generalized Additive Model
#' @export
G_f = function(type_recall_a="curr", sex_a = c("m", "f"),
               weighted_a=T, K_a = 3, data_a, 
               include_nhanesIII=T, dataIII_a=NULL){
  
  nhanes_loc = data_a[ type_recall == type_recall_a & sex %in% sex_a,
                       .(weight, pregnant, yob, age, bmi_deb1)]
  
  if(type_recall_a=="curr"){
    
    if(include_nhanesIII){
      
      nhanes_loc <- clear_labels(nhanes_loc) # for binding with nhanesIII_loc
      
      nhanes3_loc = nhanesIII[sex %in% sex_a, .(weight, pregnant, yob, age, bmi_deb1)]
      
      nhanes_loc[,  weight:= weight* length(nhanes_l)/(3+length(nhanes_l))]
      
      nhanes3_loc[, weight:= weight* 3/(3+length(nhanes_l))]  # nhanes III represents 6 yrs (3*2yr cycles)
      
      nhanes_loc = rbindlist(list(nhanes_loc, nhanes3_loc), use.names=TRUE )
    }
    
    nhanes_loc = nhanes_loc[pregnant==0] # exclude pregnant women if estimates are based on current BMI
  }
  
  if(weighted_a){
    
    nhanes_loc[ , new_weight:= weight/mean(weight)]
    
    # te : tensor product smooth of cubic regression spline bases
    gam_model = mgcv::gam(bmi_deb1 ~ te(yob, age, k=K_a), 
                          family=Gamma(link=log), data = nhanes_loc, 
                          weights = new_weight)
  }else{
    gam_model = mgcv::gam(bmi_deb1 ~ te(yob, age, k=K_a), 
                          family=Gamma(link=log), data = nhanes_loc)
  }
  
  return(gam_model)
}


#' Region of the cohort x age plane measurements (current, 1yr or 10yrs ago) pertain to
#'
#' @param d_a data.table
#' @param by_sex_a whether the bias is sex-specific (\code{TRUE}) or not (\code{FALSE})
#' @param Glist_a list of models to use to compute bias
#' @return 0n for correct execution
#' @export
add_bias_f = function(d_a, by_sex_a=T, Glist_a){
  
  if(by_sex_a)  {
    
    d_loc = d_a[,.(age=(age-1), yob=yob, sex=sex)]
    
    bias2_1y_m= mgcv::predict.gam(Glist_a$currM, d_loc, type = "response")- 
      mgcv::predict.gam(Glist_a$min1M, d_loc, type = "response")
    bias2_1y_f= mgcv::predict.gam(Glist_a$currF, d_loc, type = "response")- 
      mgcv::predict.gam(Glist_a$min1F, d_loc, type = "response")
    
    d_loc = d_a[,.(age=(age-10), yob=yob, sex=sex)]
    
    bias2_10y_m= mgcv::predict.gam(Glist_a$currM, d_loc, type = "response")-
      mgcv::predict.gam(Glist_a$min10M, d_loc, type = "response")
    bias2_10y_f= mgcv::predict.gam(Glist_a$currF, d_loc, type = "response")-
      mgcv::predict.gam(Glist_a$min10F, d_loc, type = "response")
    
    bias2_1y =  ifelse(d_loc[,sex=="m"], bias2_1y_m,  bias2_1y_f)
    bias2_10y = ifelse(d_loc[,sex=="m"], bias2_10y_m, bias2_10y_f)
    
    d_a[, BMI1y_deb2:=(BMI1y_deb1+bias2_1y)]
    
    d_a[, BMI10y_deb2:=(BMI10y_deb1+bias2_10y)]   
    
  } else { # correction not sex-specific
    
    d_loc = d_a[,.(age=(age-1), yob=yob)]
    
    bias2_1y= mgcv::predict.gam(Glist_a$curr, d_loc, type = "response")- 
      mgcv::predict.gam(Glist_a$min1, d_loc, type = "response")
    
    d_loc = d_a[,.(age=(age-10), yob=yob)]
    
    bias2_10y= mgcv::predict.gam(Glist_a$curr, d_loc, type = "response")-
      mgcv::predict.gam(Glist_a$min10, d_loc, type = "response")
    
    d_a[, BMI1y_deb2:=(BMI1y_deb1+bias2_1y)]
    
    d_a[, BMI10y_deb2:=(BMI10y_deb1+bias2_10y)]
    
  }
  return(0)
}


#' Function to estimate BMI at 25 based on measured data
#'
#' @param sex_a c("m", "f"), "m" or "f"
#' @param w_a weight argument : "original" or "rescaled"
#' @param k_a dimension of the smooth term's basis
#' @param nhanes25_a dataset used
#' @return A Generalized Additive Model
#' @export
G25_f  = function( sex_a=c("m", "f"), w_a = "original", k_a=6, nhanes25_a){
  
  if(!w_a %in% c("original","rescaled")){stop("Incorrect weight argument w_a")}
  
  if(w_a=="original"){ 
    nhanes25_a[,w_loc:=weight] 
  } else{
    nhanes25_a[,w_loc:=weight_rescaled]}
  
  RES = mgcv::gam(BMIcur_deb1~ s(yob, k = k_a), data = nhanes25_a[(about25)],
                  family=Gamma(link=log),
                  subset = (sex %in% sex_a),
                  weights = w_loc)
  
  nhanes25_a[,w_loc:=NULL]
  
  return(RES)
}

#' Create deb2 bmis (bmis with cohort-level corrections) for bmi at 25 
#'
#' @param d_a data.table
#' @param by_sex_a whether the bias is sex-specific (\code{TRUE}) or not (\code{FALSE})
#' @param G25list_a list of models to use to compute bias
#' @return 0 for correct execution
#' @export
add_bias25_f = function(d_a, by_sex_a=T, G25list_a){ 
  
  if(by_sex_a){
    bias2_25_m = mgcv::predict.gam(G25list_a$m, d_a, type = "response")-  mgcv::predict.gam(G25_dec$m, d_a, type = "response")
    bias2_25_f = mgcv::predict.gam(G25list_a$f, d_a, type = "response")-  mgcv::predict.gam(G25_dec$f, d_a, type = "response")
    
    bias2_25 = ifelse(d_a[,sex=="m"], bias2_25_m, bias2_25_f)
    
  } else{
    
    bias2_25 = mgcv::predict.gam(G25list_a$a, d_a, type = "response")-  mgcv::predict.gam(G25_dec$a, d_a, type = "response")
    
  }
  
  d_a[, BMI25_deb2:=(BMI25_deb1+bias2_25)]  
  
  return(0)
}


