#' Load NHANES data
#'
#' @param directory_a directory of data
#' @param path path to \code{directory_a}
#' @return A \code{data.table}
#' @importFrom data.table copy setDT setkey setnames :=
#' @export
import_nhanes_f = function(directory_a, path_a=""){
  
  # load data into a single list of data.frames 
  loc_nhanes_l <- lapply(list.files(paste0(path_a, directory_a), full.names = T), 
                         Hmisc::sasxport.get
  )
  
  # set names 
  # NB: files of cycle A are not suffixed _A.XPT, but simply .XPT
  names(loc_nhanes_l) <- tolower(gsub("(_.)?\\.XPT$", "", list.files(paste0(path_a, directory_a))))
  
  # convert data.frames to data.tables
  lapply(loc_nhanes_l, setDT)
  
  # set seqn (participant identification number) as key in all data.tables
  lapply(loc_nhanes_l, setkey, seqn)
  # check key setting
  # sapply(loc_nhanes_l, key)
  
  # number of subjects by data.table
  # sort(sapply(loc_nhanes_l, nrow), decreasing = T)
  
  # For each dataset, prefix all variables by dataset name (except seqn)
  # Also create a variable in_[component] that records if the individual is 
  # in component. 
  # Caveat : for some components (e.g. DEMO) some individuals are non-eligible
  # to specific questions (e.g. in DEMO, DMDEDUC2 is the education Level of adults 20+ :
  # those < 20 have value . (Missing)) but are in the file storing the data for this component
  
  add_pref=function(data, var_name){
    OLD = names(data)[names(data)!="seqn"]
    setnames(data, old = OLD, new = paste(var_name, OLD, sep="_")) }
  add_elig=function(data, var_name){
    data[,c(paste0("in_", var_name)):=TRUE]}
  
  mapply(add_pref, loc_nhanes_l, names(loc_nhanes_l))
  mapply(add_elig, loc_nhanes_l, names(loc_nhanes_l))
  
  # merge all data.tables (based on seqn)
  nhanes = Reduce(function(...){merge(..., all = TRUE)}, loc_nhanes_l)
  
  # replace NAs values in in_[component] variables with FALSE
  elig_cols = grep(names(nhanes), pattern = "^in_", value=T)
  nhanes[, c(elig_cols) := lapply(.SD, function(x){ifelse(is.na(x), FALSE, x)}),
         .SDcols = elig_cols]
  
  # record nhanes cycle individuals belong to (in case all data.tables are merged)
  nhanes[, cycle:=directory_a]
  
  # convert cycle (A, B, C...) into its number in a secure way (code still valid if new cycle added)
  cycle_numb = which.max(directory_a == LETTERS)
    
  
  # years of cycle
  nhanes[,cycle_miny:= (1999L + 2L*(cycle_numb-1L))]
  nhanes[,cycle_maxy:= (2000L + 2L*(cycle_numb-1L))]
  
  return(nhanes)
  
}



#' Compute weights for the analysis of all nhanes cycles taken together
#'
#' @param data_a a \code{data.table} of nhanes data, including weights 
#' (either \code{demo_wtmec2yr} or \code{demo_wtmec4yr})
#' @details Weights used are MEC weights, since the analysis involves measures
#' taken during MEC examination (e.g. measured height), and is therefore 
#' restricted to people both interviewed and MEC examined (RIDSTATR==2, which
#' has become examined == 1)
#' @return None.
#' @export
add_weights_f = function(data_a){
  
  if(data_a[, unique(cycle)] %in% c("A", "B") ){
    
    data_a[, weight := demo_wtmec4yr*2/n_cycles ]
    
  } else {
    
    data_a[, weight := demo_wtmec2yr/n_cycles ]  
    
  } 
  
  return(0)
}



#' Clean nhanes data
#'
#' @param data_a a \code{data.table} of nhanes data.
#' @param height_a which height to use in BMI computation. Either "measured" or "self-reported". 
#' @param verbose_a whether details should be printed to screen.
#' @details This function : 
#' \itemize{
#' \item{changes some variable names;}
#' \item{changes values of sex : 1->m and 2-> f;}
#' \item{converts self-reported weights and height in kgs and cms;}
#' \item{adds new variable (current bmi, etc.);}
#' \item{group non-Mexican and Mexican Hispanics (new \code{race} variable);}
#' \item{group high-school graduate and less (newd \code{educ} variable)}
#' }
#' NB : no individual is removed from dataset.
#' @return 0 for correct execution
#' @export
clean_nhanes_f=function(data_a, height_a="measured", verbose_a=F){
  
  
  #### change names of repeatedly used variables 
  
  if(verbose_a){ # to monitor new variables and renamed variables
    names_init = copy(names(data_a))  }
  
  # shorten names of important variables (demographic data)
  setnames(data_a, 
           old = c("demo_ridstatr", "demo_sdmvpsu", "demo_sdmvstra", "demo_ridexprg", "demo_riagendr", "demo_ridageyr"), 
           new = c("examined",      "psu",          "stratum",       "pregnant",      "sex",           "age"))
  # shorten names of important variables (body measurements)
  setnames(data_a, 
           old = c("bmx_bmxwt", "bmx_bmxht", "bmx_bmxbmi"), 
           new = c("Wmeas",     "Hmeas",     "BMImeas"))
  # shorten names of important variables (smoking)
  setnames(data_a, 
           old = c("smq_smq020", "smq_smd030",  "smq_smq040", "smq_smq050q",    "smq_smq050u"), 
           new = c("smok100",    "smok_age",    "smok_curr", "smok_time_quit", "smok_time_quit_unit"))
  
  
  if( "smq_smd055" %in% names(data_a)){setnames(data_a, old = "smq_smd055", new = "smok_age_last_reg")}
  

  
  ### some changes in coding 
  
  # 0 : interviewed only; 1 : interviewed and examined
  data_a[, examined:=(examined-1L)]
  
  data_a[, sex:=as.character(sex)]
  data_a[sex=="1", sex:="m"]
  data_a[sex=="2", sex:="f"]
  
  
  data_a[pregnant %in% c(2L, 3L) | is.na(pregnant), pregnant:=0L]
  
  
  # set refused and don't know to NA for education 
  data_a[demo_dmdeduc2 %in% c(7L, 9L), demo_dmdeduc2:= NA_integer_]
  
  
  # those who don't know whether they smoked 100 cigarettes in 
  # life are set to No. Very very few cases
  data_a[smok100 == 9L, smok100:=2L]
  # set refused and don't know to NA for age started smoking cigarettes regularly
  data_a[smok_age %in% c(777L, 999L), smok_age:=NA_integer_]
  # set refused and don't know to NA for current smoking status. 
  # NB : those are people known to have smoked 100 cig. in life
  data_a[smok_curr %in% c(7L, 9L), smok_curr := NA_integer_]
  # those who have never smoked 100 cig. are not currently smoking... 
  # prevents big mistakes in descriptive stats
  data_a[smok100 == 2L, smok_curr := 3L]
  
  # table(data_a[,smok100], useNA = "a")
  # table(data_a[,smok_age], useNA = "a")
  # table(data_a[,smok_curr], useNA = "a")
  # table(data_a[,smok_time_quit], useNA = "a")
  # table(data_a[,smok_age_last_reg], useNA = "a")
  
  
  data_a[, smok_age_last := NA_integer_]
  data_a[smok_time_quit_unit == 4, smok_age_last := (age - smok_time_quit)]
  data_a[smok_time_quit_unit == 3, smok_age_last := round(age - smok_time_quit/12)] # months
  data_a[smok_time_quit_unit == 2, smok_age_last := round(age - smok_time_quit/52)] # weeks
  data_a[smok_time_quit_unit == 1, smok_age_last := round(age - smok_time_quit/365)] # days
  
  
  # data_a[smok_age_last_reg<120, plot(smok_age_last, smok_age_last_reg)]
  # data_a[!is.na(smok_age_last), mean(smok_age_last< min(ages_study) )]
  # data_a[!is.na(smok_age_last), mean(smok_age_last_reg< min(ages_study), na.rm=T )]
  
  
  # minimum age of study = AgeMin
  mas <- min(ages_study) 
  
  # create smoking status at minimum age of study (abbrev. mas)
  # heavy but avoids creating implausible outcomes (e.g. known status for those < mas)
  # + we check we correctly process the available data
  data_a[, smokerAgeMin := NA_integer_]
  
  # Those who started smoking before AgeMin are considered smokers at AgeMin
  data_a[age >= mas & 
           smok100 == 1L &                                    # smoked at least 100 cig.
           smok_age %in% 1:mas &                              # started smoking regularly before mas. NB : never smoked regularly is coded smok_age==0
           (smok_curr %in% c(1L, 2L) | smok_age_last > mas), # currently smoking, or stopped but after ageMin
         smokerAgeMin := 1L ]
  # BUT those who stopped before ageMin are removed from the smokers at ageMin category
  data_a[age >= mas & 
           (smok100==2L |                                         # never smoked 100 cig
              smok_age == 0 |                                     # never smoked regularly
              smok_age > mas |                                    # began smoking regularly after mas
              smok_age_last  <= mas                               # stopped before mas
           ), 
         smokerAgeMin := 0L ]
  
  
  # check. NAs should be few
  # data_a[age >= mas, table(smokerAgeMin, useNA = "a") ]
  
  
  # conversion factors
  kg_to_lbs = 2.2046 # (unit : lbg/kg)
  cm_to_inc = 0.3937 # (unit : inches/cm)
  
  # give meaningfull names to whd columns
  old_names = paste0("whq_whd", c("010", "020", "050", "110", "120", "130", "140", "150"))
  
  # Age when heaviest weight is whd150 in nhanes 1999-2000, whq150 thereafter
  if(address(data_a) != address(nhanes_l$A)){
    old_names = gsub("whq_whd150", "whq_whq150", old_names)  }
  
  bmi_cols =  c("Hcur", "Wcur", "W1y", "W10y", "W25", "H25", "Wmax", "age_Wmax")
  
  setnames(data_a, old = old_names, new = bmi_cols)
  
  # set refused (series of 7s) and don't know (series of 9s) to NA for bmi-related measures
  na_fun = function(x){ ifelse( x %in% c(7777, 77777, 9999,  99999), NA, x )}
  data_a[, c(bmi_cols) := lapply(.SD, na_fun),  .SDcols = bmi_cols]
  
  # convert all self-reported weights in kg
  w_cols = grep(pattern = "^W", bmi_cols, value = T)
  data_a[, c(w_cols) := lapply(.SD, function(x){x/kg_to_lbs}), .SDcols = w_cols]
  
  # convert all self-reported heights in cm
  h_cols = grep(pattern = "^H", bmi_cols, value = T)
  data_a[, c(h_cols) := lapply(.SD, function(x){x/cm_to_inc}), .SDcols = h_cols]
  
  
  # denominator of bmi measure. Temporary variable
  switch(height_a, 
         "measured"={
           data_a[, denom:= (Hmeas/100)^2] },
         "self-reported"={
           data_a[, denom:= (Hcur/100)^2] },
         {
           stop("Incorrect height") }   )
  
  
  # compute BMIs
  data_a[, `:=`(BMIcur = Wcur / denom,  # current BMI
                BMI1y = W1y / denom, # BMI 1 y before study
                BMI10y = W10y / denom, # BMI 10 y before study
                BMI25 = W25 / denom, # BMI at age 25 
                BMImax = Wmax / denom # Max BMI ever attained 
  )]
  
  data_a[, denom:=NULL]
  
  data_a[, bias:= (BMImeas-BMIcur)] # bias in reported bmi
  data_a[pregnant==1, bias:= 0] # if pregnant woman, set bias to 0 : correcting their past bmis would be spurious
  # since measured bmi is bmi at the time of a pregnancy, it overestimates bmi trajectory. In particular, the difference
  # with reported bmi is not a declaration bias, but the fact that reported bmi is that before pregnancy
  
  
  # define de-biased bmis 
  data_a[, `:=`(BMIcur_deb1 = BMIcur + bias,  # of course it is measure bmi. Defined for coherence of notations
                BMI1y_deb1  = BMI1y  + bias, # BMI 1 y before study
                BMI10y_deb1 = BMI10y + bias, # BMI 10 y before study
                BMI25_deb1  = BMI25  + bias, # BMI at age 25 
                BMImax_deb1 = BMImax + bias # Max BMI ever attained 
  )]
  
  
  # group Mexican and non-Mexican Hispanics in variable race
  data_a[, race:=NA_character_] # define race
  data_a[demo_ridreth1 %in% 1:2, race:="h"] # hispanic : Mexican American	+ Other Hispanic
  data_a[demo_ridreth1 == 3, race:="w"] # white
  data_a[demo_ridreth1 == 4, race:="b"] # black
  data_a[demo_ridreth1 == 5, race:="o"] # other
  
  # group high-school graduate and less as educ. attainments in variable educ
  data_a[, educ:=NA_character_] 
  data_a[demo_dmdeduc2 %in% 1:3, educ:="hsl"] # high school graduate or less
  data_a[demo_dmdeduc2 == 4, educ:="scd"] # some college degree
  data_a[demo_dmdeduc2 == 5, educ:="cg"] # college graduate or above
  
  
  # imput year of birth based on age at screening and cycle years
  # ideally replaced by true values obtained from the CDC
  set.seed(2324)
  epsilon = sample(c(-1, 0, 1), size=data_a[,.N], replace=T, prob = c(0.25, 0.5, 0.25))
  data_a[, yob := cycle_miny - age + epsilon]
  
  
  if(verbose_a){
    changed_names = sort(  setdiff(names(data_a), names_init) )
    cat("New and renamed variables are : \n" , paste0(changed_names, "\n" ) )
    cat("Correct execution of function :\n")
  }
  
  return(0)   }



#' Get colours for males and females
#'
#' @param sex_a Either  "f" or "m"
#' @param a_a alpha value 
#' @return rgb value
#' @export
col_f = function(sex_a="f", a_a=50){
  ifelse(sex_a=="m", 
         rgb(255, 0, 0, max = 255, alpha = a_a), 
         rgb(0, 0, 255, max = 255, alpha = a_a))
}



#' NHANES data in long format (all cycles grouped)
#'
#' @param input_data_a NHANES data in wide format
#' @param verbose_a print details to screen
#' @param bmis_a wich BMI series are available and should be melted
#' @return A \code{data.table}
#' @export
melt_nhanes_f=function(input_data_a, verbose_a=T, bmis_a="all"){
  
  if(!bmis_a %in% c("all", "deb1_only")){stop("Incorrect bmi argument")}
  
  data_a=copy(input_data_a)
  
  # to avoid integer / double mix during melting
  data_a[, age := as.integer(age)]
  
  # create ages at which the 3 measurements were performed
  data_a[, `:=`(age_25 = 25L,
                age_10 = age-10L,
                age_1 = age-1L)]
  
  # age (at screening, originally named demo_ridageyr) kept as an id var. in melting 
  data_a[, age_screening := age]
  
  
  ############### Melting
  
  colA = c("age", "age_25", "age_10", "age_1", "age_Wmax")
  
  colB = c("BMIcur",      "BMI25",      "BMI10y",      "BMI1y",      "BMImax")
  
  colC = c("BMIcur_deb1", "BMI25_deb1", "BMI10y_deb1", "BMI1y_deb1", "BMImax_deb1")
  
  # in the second series of measurements no further correction is needed for
  # BMIcur (known to be unbiased)and BMImax (not used)
  colD = c("BMIcur_deb1", "BMI25_deb2", "BMI10y_deb2", "BMI1y_deb2", "BMImax_deb1")
  
  id_vars = c("seqn", "examined", "psu", "stratum", 
              "cycle", "cycle_miny", "cycle_maxy", 
              "weight",                   
              "pregnant",            
              "sex", "educ", "race", 
              "age_screening", "yob", 
              "bias", "smokerAgeMin", "smok_curr")
  
  
  # this also removes negative ages (obtained with those < 10 y at study time 
  # when demo_ridageyr-10L is created) ; only columns of interest are kept
  if(bmis_a=="all"){
    
    Measures = list(colA, colB, colC, colD)
    Value_name=c("age", "bmi", "bmi_deb1", "bmi_deb2")
    test_variables = c(id_vars, colA, colB, colC, colD) # cf. below
    
  }else{  # bmis_a==deb1_only
    
    Measures=list(colA, colB, colC)
    Value_name=c("age", "bmi", "bmi_deb1")
    test_variables = c(id_vars, colA, colB, colC)
    
  }
  
  res = melt( data_a, 
              measure = Measures, 
              id.vars=id_vars,
              variable.name = c("type"),
              value.name = Value_name,
              variable.factor = F )
  
  recode = data.table(old=1:5, new=c("curr", "at25", "min10", "min1", "whichmax"))
  res[, type_recall := recode[match(res[,type], old), new]  ]
  if(verbose_a){
    cat("..... Check of types of recall :\n")
    print(res[,table(type, type_recall)])
  }
  res[,type:=NULL]
  
  ### Remove measurements we don't have by construction
  # BMI10y for those < 36
  res= res[!(age_screening<36 & type_recall == "min10")]
  # BMI25 for those < 27
  res= res[!(age_screening<27 & type_recall == "at25")]
  
  # some checks : compare data 
  if(verbose_a){
    test_sample = sample(res[, unique(seqn)], size = 3)
    cat("\n\n\n.....Check of the melting on a random sample :\n.................Melted sample :\n")
    print(res[.(test_sample),  on = "seqn"], digits=3)
    cat(".................Original sample:\n")
    print(data_a[.(test_sample),  on = "seqn", test_variables, with=F], digits=3)
  }
  
  return(res)
}



#' Remove "labelled" from class attribute
#'
#' @param x data.table or list of data.tables
#' @return A \code{data.table}
#' @export
clear_labels = function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}