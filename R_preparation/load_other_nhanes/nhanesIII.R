
# Examination File


# # Import using read.SAScii (takes time) and save as csv file.
# nhanesIII <-  read.SAScii(fn = "../data/nhanesIII/exam.dat", sas_ri = "../data/nhanesIII/exam.sas")
# nhanesIII = as.data.table(nhanesIII)
# fwrite(nhanesIII, file="../data/nhanesIII/csv/nhanesIII.csv")
# # Reimport nhanesIII (gains time)
nhanesIII = fread("../data/nhanesIII/csv/nhanesIII.csv")


# change names, to be in line with variables' names,  in particular, we name measured bmi bmi_deb1
old_v = c("SEQN", "HSSEX", "DMARETHN", "DMAETHNR",  "HSAGEIR", "WTPFEX6", "SDPPHASE", "SDPSTRA6", "SDPPSU6", "BMPBMI", "MAPF12R")
new_v = c("seqn", "sex",   "race0",    "ethnicity", "age",     "weight",  "phase",    "stratum",  "psu" ,    "bmi_deb1", "pregnant")

setnames(nhanesIII, old = old_v, new = new_v)
nhanesIII=nhanesIII[, new_v, with=FALSE]
rm(list=c("old_v", "new_v"))


# Measured BMI
nhanesIII[bmi_deb1==8888, bmi_deb1:=NA] # 8888 : Blank but applicable 

# we duplicate the bmi column because nhanesIII will be used both together with
# melted (with melt_nhanes_f) and non melted continous nhanes data
nhanesIII[, BMIcur_deb1:=bmi_deb1]

# Pregnancy status
nhanesIII[pregnant %in% c(2, 8, 9, NA), pregnant:=0]  
nhanesIII[,table(pregnant)] # Check with documentation

### Sex
nhanesIII[, sex:=as.character(sex)]
nhanesIII[sex=="1", sex:="m"]
nhanesIII[sex=="2", sex:="f"]

### Race
nhanesIII[, race:=as.character(NA)] # this line should be useless
nhanesIII[race0 == 1, race:="w"] # Non-Hispanic white
nhanesIII[race0 == 2, race:="b"] # Non-Hispanic black
nhanesIII[ethnicity %in% c(1,2), race:="h"] # Mexican-American + Other Hispanic
nhanesIII[ethnicity == 3 & race0==4, race:="o"] # other
nhanesIII[is.na(race), .N] # should be 0


#######################################################################################################
# " NHANES III [...] was conducted from October 18,1988, through October 15, 1994, in two phases [...]
# The first phase was conducted from October 18, 1988, through October 24, 1991, at 44 locations. The 
# second phase was conducted from September 20, 1991, through October 15, 1994, at 45 sites. "
#######################################################################################################
######### we approximate by saying phase 1 is from 1989 to 1991 and phase 2 from 1992 to 1994 #########

nhanesIII[,phase_min:=ifelse(phase==1, 1989, 1992)]

set.seed(2392)
epsilon = sample(c(-1, 0, 1, 2), size=nhanesIII[,.N], replace=T, prob = c(1/6, 2/6, 2/6, 1/6))
nhanesIII[, yob := phase_min - age + epsilon]
rm(epsilon)


### some checks

# nhanesIII[,.(phase, phase_min, age, yob, bmi_deb1)]
# hist(nhanesIII[,(bmi_deb1)], main="measured BMIs, NHANES III", xlab="BMI")
# abline(v = c(18.5, 25, 30, 40), lty = 2, lwd=1.5, col = "darkgrey")

# nhanesIII[,mean(pregnant), by=.(sex)]
# nhanesIII[sex == "f" & age%in%19:45, mean(pregnant)]

