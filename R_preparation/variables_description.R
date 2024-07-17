# NB : Missing values coded as . in the original datasets are loaded as NA by sasxport.get
# Those missing values coded . are mostly missing by design. For example, it 
# adult education (DMDEDUC2, for participants 20+) is not available for those <20. As 
# those < 20 must be in the DEMO component (which holds DMDEDUC2), one has to set
# their DMDEDUC2 value as Missing. 
# Missing values coded 7, 77 etc. (refused) and 9, 99, etc. (don't know) are manually recoded


# Variables described below :

#     SEQN, RIDSTATR, RIAGENDR, RIDAGEYR, RIDRETH1, DMDEDUC2, RIDEXPRG,

#     SMQ020, SMD030, SMQ040, SMQ050Q, SMQ050U, SMD055

#     WTINT2YR, WTMEC2YR, WTINT4YR, WTMEC4YR, SDMVPSU, SDMVSTRA,

#     WHD010, WHD020, WHD050, WHD110, WHD120, WHD130, WHD140, WHQ150 [/WHD150],

#     BMXWT, BMXHT, BMXBMI


################################## Demographic Data

#####  File Demographic Variables and Sample Weights (DEMO)

# Eligible Sample :
      # everybody in the nhanes study
      # the target age groups for specific questions vary.


# SEQN : Respondent sequence number 
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 0 YEARS - 150 YEARS (i.e. everybody)
#         - no missing values
# 
# RIDSTATR : Interview/Examination Status 
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 0 YEARS - 150 YEARS (i.e. everybody)
#         - no missing values
#         - possible values are 1 (interviewed only) and 2 (both interviewed and MEC examined)
# 
# RIAGENDR : Gender
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 0 YEARS - 150 YEARS (i.e. everybody)
#         - no missing values 
#         - possible values are 1 (Male) and 2 (Female)
# 
# RIDAGEYR : Age at Screening  
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 0 YEARS - 150 YEARS (i.e. everybody)
#         - some imputation if exact date of birth information was not provided during the interview
#         - SAS label for cycles A-F : Age at Screening Adjudicated - Recode
#         - for cycles A-D (resp. E-I), individuals 85 (resp. 80) and over are topcoded at 85 (resp. 80)
#           years of age to reduce the risk of disclosure.
#         - no missing values
#         
# RIDRETH1 : Race/Ethnicity
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 0 YEARS - 150 YEARS (i.e. everybody)
#         - SAS Label for cycles A-F : Race/Ethnicity - Recode
#         - SAS Label for cycles G-I : Race/Hispanic origin
#         - no missing values
#         - possible values are 1 (Mexican American)
#                               2 (Other Hispanic)
#                               3 (Non-Hispanic White)
#                               4 (Non-Hispanic Black)
#                               5 (Other Race - Including Multi-Racial)
#
# DMDEDUC2 : Education level - Adults 20+
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 20 YEARS - 150 YEARS
#         - Missing values : 7 (Refused)
#                            9 (Don't know)
#                            . (Missing) 
#           For cycles C-I, Missing are exactly those for individuals < 20. For cycles A-B, resp. 2 and 4 other values are Missing
#         - possible values (missing values left aside) are : 1 (Less Than 9th Grade)
#                                                             2 (9-11th Grade (Includes 12th grade with no diploma))
#                                                             3 (High School Grad/GED or Equivalent)
#                                                             4 (Some College or AA degree)
#                                                             5 (College Graduate or above)
# 
# RIDEXPRG : Pregnancy Status at Exam [i.e. MEC exam]
#         - present in all cycles, with same name
#         - at cycles A-D, the target is : Females 8 YEARS - 59 YEARS
#         - at cycles E-I, the target is : Females 20 YEARS - 44 YEARS
#         - Missing values are all coded . (Missing)
#         - in short, possible values are : 1 Yes (pregnant at exam) 
#                                           2 No (not pregnant at exam)
#                                           3 Cannot ascertain if the participant is pregnant at exam
#         - participants interviewed only (RIDSTATR==1) may also be RIDEXPRG==1. It is only at the time of exam, not for those examined
# 
# WTINT2YR : Full Sample 2 Year Interview Weight
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 0 YEARS - 150 YEARS (i.e. everybody)
#         - no missing values
#
# WTMEC2YR : Full Sample 2 Year MEC Exam Weight
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 0 YEARS - 150 YEARS (i.e. everybody)
#         - no missing values
#         - English Text : Both Interviewed and MEC Examined Sample Persons.
#         - this weight is 0 if and only if the participant was only interviewed (no MEC exam)
#
# WTINT4YR : Full Sample 4 Year Interview Weight 
#         - present ONLY FOR CYCLES A and B (years 1999-2002), with same name
#         - at both cycles, the target is : Both males and females 0 YEARS - 150 YEARS (i.e. everybody)
#         - no missing values
#         - The 4-year weights were provided because there were some transition issues related to the use
#           of 1990 Census and 2000 Census information.
#
# WTMEC4YR : Full Sample 4 Year MEC Exam Weight 
#         - same as WTINT4YR : only for cycles A and B (with sm name), sm target, no mis. val., provided for the same reasons
#
# SDMVPSU : Masked Variance Pseudo-PSU
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 0 YEARS - 150 YEARS (i.e. everybody)
#         - no missing values
#         - values : 1, 2, 3 or 1, 2, depending on the cycle
#
# SDMVSTRA : Masked Variance Pseudo-Stratum
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 0 YEARS - 150 YEARS (i.e. everybody)
#         - no missing values
#         - values : 1 to 13 for cycle A, 14-28 for cycle B, ..., 119-133 for cycle I




################################## Questionnaire Data :  Smoking - Cigarette Use

# Eligible Sample : 
#       - All survey participants 20+ for cycles A-C, then 12+ for D-I

# Variables of interest described below : SMQ020, SMD030, SMQ040, SMQ050Q, SMQ050U, SMQ055


# SMQ020 : Smoked at least 100 cigarettes in life
#         - present in all cycles, with same name
#         - at cycles A-G, the target is : Both males and females 20 YEARS - 150 YEARS 
#         - at cycles H and I, the target is : Both males and females 18 YEARS - 150 YEARS 
#         - values : 
#              - 1: Yes
#              - 2: No
#         - missing values : 7 (Refused)
#                            9 (Don't know)
#                            . (Missing)
#
# SMD030 : Age started smoking cigarets regularly (for those Yes at SMQ020)
#         - present in all cycles, with same name
#         - at cycles A-G, the target is : Both males and females 20 YEARS - 150 YEARS 
#         - at cycles H and I, the target is : Both males and females 18 YEARS - 150 YEARS 
#         - lowest value across all waves : 5 (lowest value is either 5, 6 or 7)
#         - missing values : 777 (Refused)
#                            999 (Don't know)
#                            . (Missing)
#         - Never smoked cigarettes regularly is coded 0
#         - NB : values for those who answered No at SMQ020 are coded as Missing (.)
#
#
# SMQ040 : Do you now smoke cigarettes ?
#         - present in all cycles, with same name
#         - at cycles A-G, the target is : Both males and females 20 YEARS - 150 YEARS 
#         - at cycles H and I, the target is : Both males and females 18 YEARS - 150 YEARS 
#         - values : 
#              - 1: every day
#              - 2: some days
#              - 3: not at all
#         - missing values : 7 (Refused)
#                            9 (Don't know)
#                            . (Missing)
# 
# 
# SMQ050Q : How long since quit smoking cigarettes
#         - present in all cycles, with same name
#         - at cycles A-F, the target is : Both males and females 20 YEARS - 150 YEARS 
#         - at cycles G and H, the target is : Both males and females 12 YEARS - 150 YEARS 
#         - at cycle I, the target is : Both males and females 18 YEARS - 150 YEARS 
#         - missing values : 77777 (Refused)
#                            99999 (Don't know)
#                            . (Missing)
#         - for cycles H and I : 50 year or more is coded 66666
# 
# 
# SMQ050U : Unit of measure (day/week/month/year) [for SMQ050Q]
#         - present in all cycles, with same name
#         - at cycles A-F, the target is : Both males and females 20 YEARS - 150 YEARS 
#         - at cycles G and H, the target is : Both males and females 12 YEARS - 150 YEARS 
#         - at cycle I, the target is : Both males and females 18 YEARS - 150 YEARS 
#         - values : 
#              - 1: days
#              - 2: weeks
#              - 3: months
#              - 4: years
#         - missing and don't know are coded 7 and 9, but no case across all cycles
# 
# 
# SMD055 : Age last smoked cigarettes regularly
#         - present in all cycles, with same name
#         - at cycles A-F, the target is : Both males and females 20 YEARS - 150 YEARS 
#         - at cycles G-H, the target is : Both males and females 12 YEARS - 150 YEARS 
#         - at cycle I, the target is : Both males and females 18 YEARS - 150 YEARS 
#         - missing values : 777 (Refused)
#                            999 (Don't know)
#                            . (Missing)
#         - question is answered if smq050q above 1 yr


################################## Questionnaire Data :  Weight History file (WHQ)

# Eligible Sample : 
#       - The target sample for the questions in this section is persons age 16 years and older.
#         However, target samples for some questions (e.g. weight 10y ago) in this section vary. 


# Variables of interest described below : WHD010, WHD020, WHD050, WHD110, WHD120, WHD130, WHD140, WHQ150 [/WHD150]


# WHD010 : Current self-reported height (inches)
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 16 YEARS - 150 YEARS 
#         - range of values across all waves : 39-84
#         - missing values : 7777 (Refused)
#                            9999 (Don't know)
#                            . (Missing)
# 
# WHD020 : Current self-reported weight (pounds)
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 16 YEARS - 150 YEARS 
#         - English Instructions for cycles A-B: RECORD CURRENT WEIGHT. INCLUDE WEIGHT DURING PREGNANCY. ENTER WEIGHT IN POUNDS OR KILOGRAMS.
#         - English Instructions for cycles C-I : RECORD CURRENT WEIGHT. IF PREGNANT, ASK FOR WEIGHT BEFORE PREGNANCY. ENTER WEIGHT IN POUNDS OR KILOGRAMS.
#         - in the "Data Processing and Editing" for cycle B : "In 2001-2002, current weight (WHD020) for pregnant women was recorded as 
#           weight before pregnancy. In 1999-2000, current weight included weight during pregnancy.". So despite what the Instructions say for cycle B,
#           cycle B is like cycles C-I
#         - range of values across all waves : 50-670
#         - missing values : 7777 (Refused) [77777 for cycle A]
#                            9999 (Don't know) [99999 for cycle A]
#                            . (Missing) 
# 
# WHD050 : Self-reported weight-1 yr ago (pounds)
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 16 YEARS - 150 YEARS 
#         - English Text : How much did {you/SP} weigh a year ago?
#         - English Instructions : ENTER WEIGHT IN POUNDS OR KILOGRAMS. IF PREGNANT, ASK FOR WEIGHT BEFORE PREGNANCY.
#         - range of values across all waves : 55-600
#         - missing values : 7777 (Refused) [77777 for cycle A]
#                            9999 (Don't know) [99999 for cycle A]
#                            . (Missing) 
# 
# WHD110 : Self-reported weight-10 yrs ago (pounds)
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 36 YEARS - 150 YEARS 
#         - English Text: How much did {you/SP} weigh 10 years ago? [If you don't know {your/his/her} exact weight, please make your best guess.]
#         - English Instructions: ENTER WEIGHT IN POUNDS OR KILOGRAMS. IF PREGNANT, ASK FOR WEIGHT BEFORE PREGNANCY.
#         - range of values across all waves : 50-600
#         - missing values : 7777 (Refused) [77777 for cycle A]
#                            9999 (Don't know) [99999 for cycle A]
#                            . (Missing) 
# 
# WHD120 : Self-reported weight - age 25 (pounds)
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 27 YEARS - 150 YEARS 
#         - English Text for cycles A-C: How much did {you/SP} weigh at age 25? [If you don't know {your/his/her} exact weight, please make your best guess.]
#         - English Text for cycles D-I: How much did {you/SP} weigh at age 25? [If you don't know {your/his/her} exact weight, please make your best guess.] If ( you were/she was) pregnant, how much did (you?she) weigh before (your/her) pregnancy?
#         - English Instructions: ENTER WEIGHT IN POUNDS OR KILOGRAMS. IF PREGNANT, ASK FOR WEIGHT BEFORE PREGNANCY.
#         - range of values across all waves : 50-600
#         - missing values : 7777 (Refused) [77777 for cycle A]
#                            9999 (Don't know) [99999 for cycle A]
#                            . (Missing) 
# 
# WHD130 : Self-reported height - age 25 (inches)
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 50 YEARS - 150 YEARS 
#         - English Text : How tall {were you/was SP} at age 25? [If you don't know {your/his/her} exact height, please make your best guess.]
#         - English Instructions : ENTER HEIGHT IN FEET AND INCHES OR METERS AND CENTIMETERS
#         - range of values across all waves : 39-84
#         - missing values : 7777 (Refused)
#                            9999 (Don't know) 
#                            . (Missing) 
# 
# WHD140 : Self-reported greatest weight (pounds)
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 18 YEARS - 150 YEARS  
#         - English Instructions: ENTER WEIGHT IN POUNDS OR KILOGRAMS. DO NOT INCLUDE WEIGHT DURING PREGNANCY.
#         - range of values across all waves : 75-717
#         - missing values : 7777 (Refused) [77777 for cycle A]
#                            9999 (Don't know) [99999 for cycle A]
#                            . (Missing) 
# 
# WHQ150 : Age when heaviest weight
#         - present in all cycles, with same name EXCEPT FOR CYCLE A, where name is WHD150 
#         - at all cycles, the target is : Both males and females 18 YEARS - 150 YEARS  
#         - English Text: How old {were you/was SP} then? [If you don't know {your/his/her} exact age, please make your best guess.]
#         - missing values : 77777 (Refused) 
#                            99999 (Don't know) 
#                            . (Missing) 



################################## Examination Data : Body Measures file (BMX)


# Eligible Sample :
#       - evidently, only those MEC examined are present in this component (see the RIDSTATR variable)
#       - Body measurements were recorded for all examinees by a trained examiner in the mobile examination center (MEC). 
#       - This data set includes body measurements for women who were pregnant at the time of the exam. 


# BMXWT : Weight (kg)
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 0 YEARS - 150 YEARS (i.e. everybody)
#         - missing values : . (Missing) 
# 
# BMXHT : Standing Height (cm)
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 2 YEARS - 150 YEARS
#         - missing values : . (Missing) 
# 
# BMXBMI : Body Mass Index (kg/m**2)
#         - present in all cycles, with same name
#         - at all cycles, the target is : Both males and females 2 YEARS - 150 YEARS
#         - missing values : . (Missing) 
# 
# 