# NB : in graphical representations below, some squares of D1 and D2 will be missing. 
# This is because the plotting function interpolates with the neighbouring points, 
# which are at NA at the frontier of D1 and D2 

YOB = as.numeric(rownames(D1))
AGE = as.numeric(colnames(D1))

new_data = as.data.table(expand.grid(yob=YOB, age=AGE, 
                                     inD1=F,  inD2=F))

for(i in 1:nrow(new_data)){
  yob_loc = new_data[i, yob]
  age_loc = new_data[i, age]
  inD1loc = D1[rownames(D1)== yob_loc, colnames(D1)==age_loc] 
  inD2loc = D2[rownames(D2)== yob_loc, colnames(D2)==age_loc]   
  new_data[i, inD1:= inD1loc ]
  new_data[i, inD2:= inD2loc ]
} 

# checks
new_data[,sum(inD1)]; sum(D1)
new_data[,sum(inD2)]; sum(D2)


new_data[,`:=`(pred1a = ifelse(inD1, mgcv::predict.gam(Glist$curr, new_data, type = "response"), NA),
               pred1b = ifelse(inD1, mgcv::predict.gam(Glist$min1, new_data, type = "response"), NA),
               pred2a = ifelse(inD2, mgcv::predict.gam(Glist$curr, new_data, type = "response"), NA),
               pred2b = ifelse(inD2, mgcv::predict.gam(Glist$min10, new_data, type = "response"), NA))]

new_data[,`:=`(pred1aM = ifelse(inD1, mgcv::predict.gam(Glist$currM, new_data, type = "response"), NA),
               pred1bM = ifelse(inD1, mgcv::predict.gam(Glist$min1M, new_data, type = "response"), NA),
               pred2aM = ifelse(inD2, mgcv::predict.gam(Glist$currM, new_data, type = "response"), NA),
               pred2bM = ifelse(inD2, mgcv::predict.gam(Glist$min10M, new_data, type = "response"), NA))]

new_data[,`:=`(pred1aF = ifelse(inD1, mgcv::predict.gam(Glist$currF, new_data, type = "response"), NA),
               pred1bF = ifelse(inD1, mgcv::predict.gam(Glist$min1F, new_data, type = "response"), NA),
               pred2aF = ifelse(inD2, mgcv::predict.gam(Glist$currF, new_data, type = "response"), NA),
               pred2bF = ifelse(inD2, mgcv::predict.gam(Glist$min10F, new_data, type = "response"), NA))]


#### Plain method, using means by cohort

nhanes_plain_method = rbindlist(nhanes_l, fill = T)
nhanes_plain_method<- clear_labels(nhanes_plain_method)
nhanes_plain_method = rbindlist(list(nhanes_plain_method, nhanesIII), fill=T )

for(I in 1:nrow(new_data)){
  
  if(new_data[I, inD1]){
    
    val_locCur =  nhanes_plain_method[age==new_data[I, age] & yob==new_data[I, yob],
                                      weighted.mean(x = BMIcur_deb1, w = weight, na.rm = T)]
    val_loc1y  =  nhanes_plain_method[age==new_data[I, (age+1)] & yob==new_data[I, yob],
                                     weighted.mean(x = BMI1y_deb1, w = weight, na.rm = T)]
    new_data[I, pred_mean1a := val_locCur]
    new_data[I, pred_mean1b := val_loc1y]

    val_locCurM =  nhanes_plain_method[age==new_data[I, age] & yob==new_data[I, yob] & sex=="m" ,
                                      weighted.mean(x = BMIcur_deb1, w = weight, na.rm = T)]
    val_loc1yM  =  nhanes_plain_method[age==new_data[I, (age+1)] & yob==new_data[I, yob] & sex=="m",
                                     weighted.mean(x = BMI1y_deb1, w = weight, na.rm = T)]
    new_data[I, pred_mean1aM := val_locCurM]
    new_data[I, pred_mean1bM := val_loc1yM]
    
    
    val_locCurF =  nhanes_plain_method[age==new_data[I, age] & yob==new_data[I, yob] & sex=="f" ,
                                      weighted.mean(x = BMIcur_deb1, w = weight, na.rm = T)]
    val_loc1yF  =  nhanes_plain_method[age==new_data[I, (age+1)] & yob==new_data[I, yob] & sex=="f",
                                     weighted.mean(x = BMI1y_deb1, w = weight, na.rm = T)]
    new_data[I, pred_mean1aF := val_locCurF]
    new_data[I, pred_mean1bF := val_loc1yF]
  }
  
  if(new_data[I, inD2]){
    
    val_locCur =  nhanes_plain_method[age==new_data[I, age] & yob==new_data[I, yob],
                                      weighted.mean(x = BMIcur_deb1, w = weight, na.rm = T)]
    val_loc10y =  nhanes_plain_method[age==new_data[I, (age+10)] & yob==new_data[I, yob],
                                      weighted.mean(x = BMI10y_deb1, w = weight, na.rm = T)]
    new_data[I, pred_mean2a := val_locCur]
    new_data[I, pred_mean2b := val_loc10y]
    
    
    val_locCurM =  nhanes_plain_method[age==new_data[I, age] & yob==new_data[I, yob] & sex=="m",
                                      weighted.mean(x = BMIcur_deb1, w = weight, na.rm = T)]
    val_loc10yM =  nhanes_plain_method[age==new_data[I, (age+10)] & yob==new_data[I, yob]  & sex=="m",
                                      weighted.mean(x = BMI10y_deb1, w = weight, na.rm = T)]
    new_data[I, pred_mean2aM := val_locCurM]
    new_data[I, pred_mean2bM := val_loc10yM]
    
    val_locCurF =  nhanes_plain_method[age==new_data[I, age] & yob==new_data[I, yob] & sex=="f",
                                       weighted.mean(x = BMIcur_deb1, w = weight, na.rm = T)]
    val_loc10yF =  nhanes_plain_method[age==new_data[I, (age+10)] & yob==new_data[I, yob]  & sex=="f",
                                       weighted.mean(x = BMI10y_deb1, w = weight, na.rm = T)]
    new_data[I, pred_mean2aF := val_locCurF]
    new_data[I, pred_mean2bF := val_loc10yF]
  } }

rm(nhanes_plain_method)


####################### Comparison of plain and Gam methods for determining corrections

# with GAMs
new_data[inD1==T,mean(pred1a-pred1b)]
new_data[inD2==T,mean(pred2a-pred2b)]
new_data[inD1==T,mean((pred1a-pred1b)^2, na.rm=T)]
new_data[inD2==T,mean((pred2a-pred2b)^2, na.rm=T)]
# plain method
new_data[inD1==T,mean(pred_mean1a-pred_mean1b)]
new_data[inD2==T,mean(pred_mean2a-pred_mean2b, na.rm=T)]
new_data[inD1==T,mean((pred_mean1a-pred_mean1b)^2, na.rm=T)]
new_data[inD2==T,mean((pred_mean2a-pred_mean2b)^2, na.rm=T)]



pal = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5))

persp_bmi_f=function(pred_a, pal_a="blue", add_a=F, zlab_a="BMI",  gridded_a=T){
  
  rgl::persp3d(YOB, AGE, pred_a, col = pal_a, add=add_a,
          xlab = "", ylab = "", zlab = "")
  if(!add_a){
    rgl::title3d('', '', 'Cohort', 'Age', zlab_a, cex=1.2, font=2)
    rgl::axes3d(edges = "bbox", lwd=2, box=T)  }

  if(gridded_a){ rgl::surface3d(YOB, AGE, pred_a,  front = "lines")}
}

### current vs 1y ago
# persp_bmi_f(new_data[,pred1a], pal_a=pal[1])
# persp_bmi_f(new_data[,pred1b], pal_a=pal[2], add_a = T)
# z0 = new_data[,min(c(pred1a, pred1b), na.rm=T)]
# rgl::persp3d(YOB, AGE, ifelse(new_data[,inD1], z0, NA), col = "darkgrey", add=T)

# snapshot3d(file=paste0("../ms/figs_supApp/recall_comparisons/recall_1y_1.png"))


### current vs 10y ago FOR FEMALES
persp_bmi_f(new_data[,pred2aF], pal_a=pal[1])
persp_bmi_f(new_data[,pred2bF], pal_a=pal[2], add_a = T)
z0 = new_data[,min(c(pred2aF, pred2bF), na.rm=T)]
rgl::persp3d(YOB, AGE, ifelse(new_data[,inD2], z0, NA), col = "darkgrey", add=T)


rgl::snapshot3d(file=paste0("../ms/figs_supApp/recall_10y_1.png"))
# OR SIMPLY PRINT SCREEN 


# hist(new_data[,pred2a-pred2b])
# # BREAKS = c(-0.5,0,0.3, 0.5, 0.7,1)
# BREAKS = c(-0.5,0, 0.5, 0.75,1)
# image(YOB, AGE, matrix(new_data[,pred2a-pred2b], nrow = length(YOB), byrow=F),
#       breaks=BREAKS, col=rev(heat.colors(length(BREAKS)-1)),
#       xlim=c(min(YOB), 1985), ylim=c(ageMin-2, ageMax-10+2),
#       axes=F, xlab="Cohort", ylab="Age")
# abline(v=YOB+0.5, h=AGE+0.5, col="grey")
# axis(1) ; axis(2, las=2)
# rm(BREAKS)


