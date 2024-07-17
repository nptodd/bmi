

plot(nhanes25[(about25), weighted.mean(BMIcur_deb1, weight, na.rm=T), keyby=yob], pch=20, cex=0.6)
plot(nhanes25[(about25) & sex=="m", weighted.mean(BMIcur_deb1, weight, na.rm=T), keyby=yob], pch=20, cex=0.6)
plot(nhanes25[(about25) & sex=="f", weighted.mean(BMIcur_deb1, weight, na.rm=T), keyby=yob], pch=20, cex=0.6)
plot(nhanes25[(about25), .(yob, BMIcur_deb1)], cex=0.3)


plot_pred_f=function(model_a, add_a=F, main_a="", lty_a=2, lwd_a=1, ylim_a=c(22, 30), axes_a=T){
  
  # Prediction is plotted on the domain for which model_a had data
  
  Pred = data.frame(yob= min(model_a$model$yob):max(model_a$model$yob))
  
  if(!add_a){
    plot(Pred$yob, mgcv::predict.gam(model_a, Pred, type = "response"), ylim = ylim_a, axes=axes_a,
         lty=lty_a, lwd=lwd_a, type="l", xlab="Year of birth", ylab="Mean BMI at 25")
    mtext(main_a, line = -2, font = 2)
  }else{
    lines(Pred$yob, mgcv::predict.gam(model_a, Pred, type = "response"),
          lty=lty_a, lwd=lwd_a, col="gray32")}
  return(0)}


## test whether the set of weights (rescaled or not) has influence on the estimate
par(mfrow=c(3, 1))
plot_pred_f(G25list$a, main_a = "All", lty_a = 1, ylim_a = c(22.8,28)) ; plot_pred_f(G25list$a0, add=T)
plot_pred_f(G25list$m, main_a = "Male", lty_a = 1, ylim_a = c(22.8,28)) ; plot_pred_f(G25list$m0, add=T)
plot_pred_f(G25list$f, main_a = "Female", lty_a = 1, ylim_a = c(22.8,28)) ; plot_pred_f(G25list$f0, add=T)

par(mfrow=c(3, 1))
plot(nhanes_dec25[, weighted.mean(BMI25_deb1, weight, na.rm=T), keyby=yob], pch=20, cex=0.6)
plot(nhanes_dec25[ sex=="m", weighted.mean(BMI25_deb1, weight, na.rm=T), keyby=yob], pch=20, cex=0.6)
plot(nhanes_dec25[ sex=="f", weighted.mean(BMI25_deb1, weight, na.rm=T), keyby=yob], pch=20, cex=0.6)


# use yob only as a predictor for graphic
gam25dec_des  = function(sex_a=c("m", "f"), k_a=6, cycle_a = nhanes_dec25[,unique(cycle)]){
  
  GAM=mgcv::gam(BMI25_deb1 ~ s(yob, k = k_a),
          data = nhanes_dec25,
          family=Gamma(link=log),
          subset = (sex%in% sex_a & cycle %in% cycle_a ),
          weights = weight)
  return(GAM)
}

declared_fig=gam25dec_des()

par(mfrow=c(1,1), mar=c(10, 4, 0.5, 0.5))
plot_pred_f(G25list$a, lty_a = 1, lwd_a = 2, ylim_a = c(23,28), axes_a = F)
axis(1, tck=-0.02)
axis(2, las=2)
AT = c(1976:1979, 1989:1994, 1999:2016)-25
axis(1, at = AT, labels = rep("", length(AT)), tck=0.02)
legend("topleft", col=c("black", "gray32"),
       cex=2, lwd = 2, lty=c(1, 2), bty="n",
       legend = c(expression(paste("Based on { ", b["i,cur"], " }")),
                  expression(paste("Based on { ", b["i,25"], " }"))))
mtext("Estimation based on measurements vs. on data reported during continouous NHANES
      \n (all cycles combined). Ticks : Cohorts aged 25 at survey years (NHANES II, III and continuous)",
      side = 1, cex=1, line = 7)

plot_pred_f(declared_fig, add=T, lty_a = 2, lwd_a = 2.5)



