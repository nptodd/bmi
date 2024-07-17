
# Anthropometric Data tape. 
# NB : there is no phase variable in nhanes II (as opposed to
# Nhanes II), but Year of birth is available


# # Import using read.SAScii (takes time) and save as csv file.
# nhanesII <-  read.SAScii(fn = "../data/nhanesII/anthropometry5301.txt",
#                          sas_ri = "../data/nhanesII/anthropometry5301_F.sas")
# nhanesII = as.data.table(nhanesII)
# fwrite(nhanesII, file="../data/nhanesII/csv/nhanesII.csv") 
# # Reimport nhanesII (gains time)
nhanesII = fread("../data/nhanesII/csv/nhanesII.csv")


# create complete yob
nhanesII[, yob:= (1900+N2BM0053)]

### change names, to be in line with variables' names

old_v = c("SEQN", "N2BM0055", "N2BM0056", "N2BM0060", "N2BM0047", "N2BM0282", "N2BM0324", "N2BM0326", "N2BM0412", "N2BM0418")
new_v = c("seqn", "sex",      "race0",    "ancestry", "age",      "weight",   "stratum",  "psu" ,     "Wmeas", "Hmeas")

setnames(nhanesII, old = old_v, new = new_v)
nhanesII=nhanesII[, c(new_v, "yob"), with=FALSE]
rm(list=c("old_v", "new_v"))

### Curation of body measurements
nhanesII[Hmeas==9999, Hmeas:=NA] # 9999 : Not applicable (less than 2yrs old)
nhanesII[,Hmeas:=Hmeas/10] # original format : ABCD, where height ABC.D cm
nhanesII[,Wmeas:=Wmeas/100] # original format : ABCDE, where height ABC.DE kg

### Sex
nhanesII[, sex:=as.character(sex)]
nhanesII[sex=="1", sex:="m"]
nhanesII[sex=="2", sex:="f"]

### Race
nhanesII[, race:="o"] # other 
nhanesII[race0 == 1, race:="w"] # all whites
nhanesII[race0 == 2, race:="b"] # all blacks
nhanesII[ancestry %in% 1:8, race:="h"] # Mexican-American + Other Hispanic

### BMI
# We duplicate the bmi var because nhanesII might be used both together with
# melted (with melt_nhanes_f) and non melted continous nhanes data
nhanesII[,`:=`(bmi_deb1 = Wmeas/((Hmeas/100)^2))] 
nhanesII[,`:=`(BMIcur_deb1=bmi_deb1)] 



###################### Add pregnancy status using the Health History file

# nhanesIIb <-  read.SAScii(fn = "../data/nhanesII/HealthHistory5305.txt",
#                           sas_ri = "../data/nhanesII/HealthHistory5305_F.sas")
# nhanesIIb = as.data.table(nhanesIIb)
# fwrite(nhanesIIb, file="../data/nhanesII/csv/nhanesIIb.csv")
nhanesIIb = fread("../data/nhanesII/csv/nhanesIIb.csv")

setnames(nhanesIIb, "SEQN", "seqn")

nhanesIIb[, pregnant:=ifelse(N2SH0785 %in% c(2, 8, 9, NA), 0, 1)]


nhanesIIb = nhanesIIb[,.(seqn, pregnant)]


# add extra lines for members of nhanesII not in nhanesIIb (those < 12y)
# not in nhanesII : not examined
# not in nhanesIIb : below 12
nhanesII = merge(nhanesII, nhanesIIb, by = "seqn", all.x=T)

if(nhanesII[is.na(pregnant), max(age)]!=11){print("Error pregnant wommen")} # should be 11
nhanesII[is.na(pregnant), pregnant:=0] # NA is known to be 0
 
## some checks
# nhanesII[,mean(pregnant), by=.(sex)]
# nhanesII[sex == "f", mean(pregnant), by=.(age%in%19:45)]

rm(nhanesIIb)
