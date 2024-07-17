

# retrieve obesity status at 55
obese55 = rbindlist(lapply(STRATA1, FUN = load_results_f, RS_a = RS1, 
                           var_a = paste0("Zout_age", RS1$ageMax),
                           res_type_a = "long"))

in_cols <- grep("^iter", names(obese55), value = T)

# it very long mcmc (>10 000) remove unnecessary sampling
rmv_col <- setdiff(in_cols, paste0("iter", 1:1e4))
obese55[,c(rmv_col) :=NULL]
gc()
in_cols <- grep("^iter", names(obese55), value = T)
new_col <- gsub("^iter", "obese", in_cols)
obese55[,c(new_col) :=lapply(.SD, function(x){x>30}), .SDcols=in_cols]
# obese55[,.(iter1, obese1, iter244, obese244)] # check


get_tso_obese_f <- function(bmi_l_a = 30,
                            coh_l_a = coh_l,
                            result_set_a = RS1,
                            obese_a = obese55,
                            timeSpentObese_a = timeSpentObese,
                            data_a = nhanes_bfda1,
                            ...){
  
  obese_loc <- copy(obese_a)
  
  Seqn = mapply(seqn_subset_f, coh_l_a$beg, coh_l_a$end, 
                MoreArgs = list(data_a = data_a, ...))
  
  names(Seqn) = paste0(coh_l_a$beg, "-", (coh_l_a$end-1900))
  
  # Seqn <- rev(Seqn) # for order
  
  timeSpentObese_a <-timeSpentObese_a[limit==bmi_l_a]
  
  func_loc = function(i){ # argument i is iteration number
    if(!(i %% 1000)){print(i)}
    included_loc <- obese_loc[, c("seqn", paste0("obese", i)), with=F]
    setnames(included_loc, paste0("obese", i), "obese")
    
    tso_loc  <- timeSpentObese_a[, c("seqn", "weight",  paste0("iter", i)), with=F]
    
    setnames(tso_loc, paste0("iter", i), "iter")
    
    tso_loc[included_loc, on=.(seqn), ob50:=obese]
    tso_loc <- tso_loc[(ob50)]
    
    res <- rep(NA, length(Seqn) )
    
    for(i_seq in seq_along(Seqn)){
      res[i_seq] <- tso_loc[seqn %in% Seqn[[i_seq]], sum(iter)] / tso_loc[seqn %in% Seqn[[i_seq]], sum(weight)]
    }
    
    return(res)
  } # end func_loc
  
  
  RES <- lapply(seq_along(in_cols), func_loc)
  RES <- do.call(rbind, RES)
  
  colnames(RES) <- names(Seqn)
  
  return(RES)
}

Sys.time()
tso_obese_F1 <- get_tso_obese_f(sex_a="f", bmi_l_a = 30)
tso_obese_M1 <- get_tso_obese_f(sex_a="m", bmi_l_a = 30)
tso_obese_F2 <- get_tso_obese_f(sex_a="f", bmi_l_a = 40)
tso_obese_M2 <- get_tso_obese_f(sex_a="m", bmi_l_a = 40)
Sys.time()

write.table(tso_obese_F1, "../tables_summary/tso_obese_F1.csv")
write.table(tso_obese_F2, "../tables_summary/tso_obese_F2.csv")
write.table(tso_obese_M1, "../tables_summary/tso_obese_M1.csv")
write.table(tso_obese_M2, "../tables_summary/tso_obese_M2.csv")

tso_obese_F1 <- read.table("../tables_summary/tso_obese_F1.csv")

apply(tso_obese_F1, 2, mean)
apply(tso_obese_F1, 2, quantile, probs=2.5/100)
apply(tso_obese_F1, 2, quantile, probs=97.5/100)


