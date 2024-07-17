
is_observedCur  = isObs_f(0)
is_observedmin1  = isObs_f(1)
is_observedmin10 = isObs_f(10)

is_observedCurIII  = isObs_f(0, minYr_a = 1989, maxYr_a = 1994, # observed during NHANES III
                             cohort_span_a = 1940:1995) 



pal1 =c(gray(1, alpha = 0), rgb(0,0,1,0.5)) 
pal2 =c(gray(1, alpha = 0), rgb(1,0,0,0.5)) 


main_plot_f=function(withNhanesIII=FALSE){
  
  if(!withNhanesIII){
    image(as.numeric(rownames(is_observedCur)), 
          as.numeric(colnames(is_observedCur)), 
          is_observedCur,
          xlim=c(min(cohorts_study)-2.5, max(cohorts_study)+2.5),
          ylim=c(min(ages_study)-2.5, max(ages_study)+2.5),
          col=pal1, xlab="", ylab="", axes=F)
    axis(1, tck=-0.02, cex.axis=0.8, padj=-2) 
    axis(2, las=2, tck=-0.02) ; box()
    mtext("Cohort", 1, 1.5)
    sapply(1900:2020-0.5, function(x){abline(v=x, col="black", lty=1, lwd=0.5)})
    sapply(0:100-0.5, function(x){abline(h=x, col="black", lty=1, lwd=0.5)})
  } else{
    main_plot_f() # recursive call
    image(as.numeric(rownames(is_observedCurIII)), 
          as.numeric(colnames(is_observedCurIII)), 
          is_observedCurIII,
          col=pal1, add=T)
  }
}

png( "../ms/figs_supApp/regions_comparison.png",
     width = 3.25, height = 3.25,
     units = "in", res    = 1200, pointsize = 6)
par(mfrow=c(1, 2), mar=c(4, 3, 2, 1), oma=c(0,1,0,0))
# left panel
main_plot_f(F)
image(as.numeric(rownames(is_observedmin1)), 
      as.numeric(colnames(is_observedmin1)), 
      is_observedmin1, 
      col=pal2, add=T)
# right panel
main_plot_f(T)
image(as.numeric(rownames(is_observedmin10)), 
      as.numeric(colnames(is_observedmin10)), 
      is_observedmin10, 
      col=pal2, add=T)
mtext("Age", 2, outer = T)

dev.off()

