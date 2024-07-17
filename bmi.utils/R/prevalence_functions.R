#' Compute proportion obese in each posterior sample
#'
#' @param seqn_a ids of individuals for which proportion should be computed.
#' @param mcmc_a matrix of posterior samples (rows : individuals ; columns : samples).
#' @param limit_a lower limit of the obesity category investigated.
#' @param weighted_a whether proportions should be weighted using sample weighted.
#' 
#' @return Vector of proportions in each posterior sample.
#' @export
obese_prop_f = function(seqn_a,  mcmc_a, limit_a, weighted_a=T){
  
  iter_col = grep("^iter", names(mcmc_a), value=T) # returns name of iter columns
  
  if(weighted_a){
    as.vector( t(mcmc_a[seqn %in% seqn_a, 
                        lapply(.SD, function(x){weighted.mean((x>limit_a), weight)}), 
                        .SDcols = iter_col]))
  }else
  {
    as.vector( t(mcmc_a[seqn %in% seqn_a, 
                        lapply(.SD, function(x){mean(x>limit_a)}), 
                        .SDcols = iter_col]))
  }
}


#' Compute age-specific prevalence of obesity
#' 
#' @param PP_a parameters for prediction, list of 3: \code{age_proj}, \code{age_span}, \code{bmi_limit}
#' @param data_a a \code{data.table} storing original data
#' @param strata_a strata analyzed
#' @param RS_a set of parameters analyzed
#' @param keyby_a factors defining groups for which prevalence is to be computed

#' @return A list, including (first element) a list of matrices storing 
#' prevalences of obesity (one / value in \code{PP_a$bmi_limit})
#'
#' @export 
prevalence_f <- function(PP_a, data_a, strata_a, RS_a, keyby_a=c("sex", "cob")){
  
  if(!(keyby_a[1] == "sex" & keyby_a[2] == "cob")){
    stop("sex and cohort of birth (cob) must come first")}
  
  # vectors of breaks for cohorts
  coh_l = make_breaks_f(age_a = PP_a$age_proj, 
                        age_span_a = PP_a$age_span, 
                        data_a = data_a)
  
  data_loc <- copy(data_a)
  
  # compute cob in data_loc
  data_loc[, cob:=NA_character_]
  for(I in seq_along(coh_l$beg)) {
    
    cob_loc <- paste0(coh_l$beg[I], "-", 
                      gsub("^19", "", coh_l$end[I]) )
    
    data_loc[yob %in% coh_l$beg[I]:coh_l$end[I], 
             cob:= cob_loc] 
  }
  
  # define group each individual belongs to
  group_v <- apply(data_loc[, keyby_a, with=F], 1 , paste , collapse = "-" )
  data_loc[, group:=group_v]
  
  # get list of groups
  GROUPS <- data_loc[, .N, keyby= keyby_a]
  GROUPS[, N:=NULL]
  GROUPS <- apply( GROUPS , 1 , paste , collapse = "-" )
  
  GROUPS <- rev(GROUPS) # for order of graphs
  
  # Each element in Seqn is the vector of seqn of individuals in the group
  Seqn <- lapply(GROUPS, function(x){unique(data_loc[group == x, seqn])})
  
  names(Seqn) <-  GROUPS
  
  # retrieve mcmc iterations
  mcmc = rbindlist( lapply(strata_a, 
                           FUN = load_results_f,
                           RS_a = RS_a,
                           var_a = paste0("Zout_age", PP_a$age_proj), 
                           res_type_a="long" )  )
  # retrieve weights from data_loc
  mcmc[data_loc, on=.(seqn), weight:=weight]
  # place weight as second column
  setcolorder(mcmc, c("seqn", "weight")) 
  
  # check weights have been correctly attributed
  # mcmc[1:5,1:5]
  # data_a[seqn==1391, .(seqn, weight)]
  # mcmc[,.N, by=is.na(weight)] 
  
  # call obese_prop_f on each group of individuals
  prev_mat <- list()
  for(LIM in PP_a$bmi_limit){
    prev_mat[[paste0("limit_", LIM)]] <-  100*do.call('cbind', 
                                                      lapply(Seqn, obese_prop_f,
                                                             mcmc_a=mcmc, 
                                                             limit_a=LIM,
                                                             weighted_a=T))
    colnames(prev_mat[[paste0("limit_", LIM)]]) <- names(Seqn)
  }
  
  return(list(prev_mat  = prev_mat, 
              pp        = PP_a,
              coh_l     = coh_l))
}


#' Plot prevalence of obesity by cohort of birth
#' 
#' @param prev_a matrix of a list returned by \code{\link{prevalence_f}}
#' @param bmi_l_a which bmi limit (should be present in \code{prev_a})
#' @param sex_a either "f" or "m"
#' @param TITLE_a title given to the plot
#' @param xlim_L lower limit of xaxis (reversed)
#' @param xlim_U upper limit of xaxis (reversed)
#' @param present_L lower limit of 'present' line
#' @param present_U upper limit of 'present' line
#' @return ggplot object
#' @export 
graphs_prev_f <- function(prev_a, 
                          bmi_l_a, 
                          sex_a, 
                          TITLE_a="", 
                          xlim_L, xlim_U, 
                          present_L=NULL, 
                          present_U=NULL){
  
  param_pred <- prev_a$pp
  
  FNO_POS <- prev_a$coh_l$fno_pos
  
  prev_loc <- prev_a$prev_mat[[bmi_l_a]]
  
  prev_loc <- prev_loc[, grep(paste0("^", sex_a), colnames(prev_loc))]
  
  # retrieve year of birth
  colnames(prev_loc) <- substr(colnames(prev_loc), 3, 9)
  
  
  GRAPH <- bayesplot::mcmc_areas(prev_loc,
                                 point_est = c("mean"),
                                 prob=0.50, prob_outer=0.95)
  
  if(is.null(present_L))  present_L <- xlim_L + (xlim_U-xlim_L)/5
  if(is.null(present_U))  present_U <- xlim_U - (xlim_U-xlim_L)/5
  
  GRAPH <- GRAPH + geom_segment(aes(x =    present_L,
                                    xend = present_U, 
                                    y    = FNO_POS-0.1, 
                                    yend = FNO_POS-0.1), 
                                colour = "darkgrey") + coord_flip() 
  
  GRAPH <- GRAPH + labs(title = TITLE_a,
                        subtitle = make_char_pred_f(bmi_l_a), 
                        x=paste0(make_char_pred_f(bmi_l_a), 
                                 " at ", param_pred$age_proj, " (%)"),
                        y= "Year of birth") +
    theme(axis.text.x  = element_text(face = "bold", hjust=0, angle=315),
          axis.title.y = element_text(face = "bold", colour = "dimgrey"),
          axis.title.x = element_text(face = "bold", colour = "dimgrey"),
          plot.title = element_text(face="bold")) +
    xlim(xlim_L, xlim_U)
  
  return(GRAPH)
}



#' Get mean and quantiles of results by group 
#' 
#' @param x a matrix of posterior samples
#' @return a data.table with mean, 2.5\% and 97.5\% quantiles of posterior distribution
#' @export 
store_prevalence_f <- function(x){
  
  data.table(cohort_name = colnames(x),
             sex = substr(colnames(x), 1, 1),
             p = apply(x, 2, mean),  
             p2.5 = apply(x, 2, quantile, probs = 2.5/100), 
             p97.5 = apply(x, 2, quantile, probs = 97.5/100))
  
}
