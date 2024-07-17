#' Prepare dataset used in functional data analysis
#'
#' @param data_a list of \code{data.table}s
#' @param ages_study_a cohorts considered
#' @param cohorts_study_a age span considered
#' @details  This function calls \code{\link{melt_nhanes_f}} and returns a data.table in long format where :
#'      \itemize{ 
#'      \item{only those aged ageMin to ageMax are kept,}
#'      \item{only those MEC examined are kept,}
#'      \item{those with missing educ, bmi and smoking are excluded.}
#'      }
#' @return A \code{data.table}
#' @export
make_bfda_data_f=function(data_a, ages_study_a, cohorts_study_a){
  
  #### all nhanes taken together in a single data.table
  nhanes = rbindlist(data_a, fill = T)
  
  #### Keep individuals in selected ages at interview and cohorts
  nhanes = nhanes[age %in% ages_study_a & yob %in% cohorts_study_a]
  
  #### Keep individuals who went through MEC examination
  #### as some of some variables analyzed (bmis, debiased bmis...) are
  #### based on  measures taken at MEC exam (measured height, weight)
  #### others could be kept, but missing data would de facto exclude them
  nhanes = nhanes[examined==1]
  
  nhanes_BFDA = melt_nhanes_f(input_data_a = nhanes, bmis_a="all")
  
  ### Remove maximum BMIs : they will not be used
  nhanes_BFDA = nhanes_BFDA[type_recall != "whichmax"]
  
  # remove current bmis for pregnant women of cycle A : contrary to 
  # later cycles, in cycle A pregnant women were asked their 
  # current weight without taking into account their being pregnant
  nhanes_BFDA = nhanes_BFDA[!(cycle=="A" & pregnant==1 & type_recall=="curr")]
  
  
  # reorder the table according to seqn then age : this is vital
  # in order to avoid creating a mess when analyzing bfda results
  setkey(nhanes_BFDA, seqn, age)
  
  ############# ------------> exclusions ahead will be due missing data 
  
  cat("\n\n\n............... REMOVAL OF MISSING DATA ...............\n")
  
  cat("Is debiased bmi missing ?\n")
  print(nhanes_BFDA[, summary(is.na(bmi_deb1))])
  cat("..... Removal of observations with debiased bmi missing .....\n")
  nhanes_BFDA <- nhanes_BFDA[!is.na(bmi_deb1)]
  
  cat("Is educational attainment missing ?\n")
  print(nhanes_BFDA[, summary(is.na(educ))])
  cat("..... Removal of individuals with educational attainment missing .....\n")
  nhanes_BFDA <- nhanes_BFDA[!is.na(educ)]
  
  cat("Is smoking status at ageMin missing ?\n")
  print(nhanes_BFDA[, summary(is.na(smokerAgeMin))])
  cat("..... Removal of individuals with smoking status at ageMin missing .....\n")
  nhanes_BFDA <- nhanes_BFDA[!is.na(smokerAgeMin)]
  
  cat("........................\nTotal losses due to missing data :\n ")
  cat("(MEC-)examined individuals in age span of interest : ", nhanes[,.N], "\n",
      "Individuals included in BFDA : ", nhanes_BFDA[,.(unique(seqn))][,.N], "\n",
      "Percentage of losses due to missing data : ",
      round(100-100*nhanes_BFDA[,.(unique(seqn))][,.N]/nhanes[,.N], 2), "%\n" )
  
  
  ########### define strata for analysis
  
  # create stratum variable, prudently in two steps 
  Stratum_v <- apply(nhanes_BFDA[, Strata_def_var, with=F], 1, paste, collapse="-")
  
  nhanes_BFDA[, stratum:=Stratum_v]
  
  return(nhanes_BFDA)
}



#' Prepare dataset used in functional data analysis
#'
#' @param stratum_a which stratum to save
#' @param data_a a \code{data.table}
#' @param directory_a where to save the \code{csv} file
#' @details The dataset saved is restricted to columns that are absolutely needed by Matlab to avoid writing
# a new importfile Matlab function whenever there is a change to nhanes_BFDA
#' @return 0 for correct execution
#' @export
export_bfda_f = function(stratum_a, data_a, directory_a){
  
  
  data.table::fwrite(x = data_a[stratum == stratum_a, 
                    .(seqn, cycle, cycle_miny, cycle_maxy,
                      stratum, 
                      sex, race, educ, smokerAgeMin, smok_curr,
                      type_recall, age_screening, age,
                      weight, yob, pregnant, bias,
                      bmi, bmi_deb1, bmi_deb2)],
         file = paste0(directory_a, stratum_a, ".csv")
  )
  return(0);
}
