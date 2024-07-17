#' Plot trajectories
#'
#' Plot BMI trajectories of specific individuals
#'
#' @param seqn_v_a identifier (seqn) of individuals for which trajectories should be plotted
#' @param result_set_a set of parameters for which results are analyzed
#' @param data_a data.table that was used in BFDA
#' @param Z_a mean of posterior distribution 
#' @param Z_CL_a lower limit of 95\%CI  
#' @param Z_UL_a upper limit of 95\%CI 
#' @param bmi_a \code{Uncorr}, \code{Deb1} or \code{Deb2}
#' @param ylim_a \code{ylim} for plot
#' @param verbose_a also print info to screen
#' @param print_seqn_a if \code{TRUE}, add \code{seqn} to plot 
#' 
#' @return None
#'
#' @examples
#' plot_traj_f("seqn1111", RS, nhanes_bfda, Z, Z_CL, Z_UL)
#'
#' @export
plot_traj_f = function(seqn_v_a, 
                       result_set_a,
                       data_a,
                       Z_a, Z_CL_a, Z_UL_a,
                       bmi_a="Deb2",
                       ylim_a=c(15,48),
                       verbose_a=F, 
                       print_seqn_a=F){ 
  
  if(! bmi_a %in% c("Uncor", "Deb1", "Deb2")){ stop("Incorrect BMI argument") }
  
  for(I_a in seqn_v_a){
    
    plot(0,0, xlim=c(result_set_a$ageMin, result_set_a$ageMax),
         ylim=ylim_a, axes=F, xlab="", ylab="")  
    
    mtext("Age", side = 1, line = 2.7, cex = 1.8)
    mtext("BMI", side = 2, line = 3.0, cex = 1.8)
    
    polygon(c(Z_a[, tau], rev(Z_a[,tau])),
            c(t(Z_CL_a[, I_a, with=FALSE]), 
              rev(t(Z_UL_a[, I_a, with=FALSE]))),
            col    = "gray", 
            border = NA )
    
    lines(Z_a[,tau], t(Z_a[, I_a, with=FALSE]), lwd= 1.6 )
    
    abline(h = c(18.5, 25, 30, 40), lty = 2, col = "dimgray")
    axis(1, cex.axis=2); axis(2, las=2, tck=0.02, cex.axis=2)
    
    
    if(print_seqn_a){mtext(text =I_a, side = 3, line = -3)}  
    
    if(bmi_a == "Deb2"){
      
      # add observed data points
      cex_raw = ifelse(data_a[seqn_long == I_a, abs(bmi-bmi_deb2)<0.5], 2.5, 2.1)
      points(data_a[seqn_long == I_a, .(age, bmi)], pch=20, cex=cex_raw, col="dimgrey")
      # add points
      points(data_a[seqn_long == I_a, .(age, bmi_deb2)], pch=20, cex=2.5, col="black")
      
    } else if (bmi_a == "Deb1"){
      
      # add observed data points
      cex_raw = ifelse(data_a[seqn_long == I_a, abs(bmi-bmi_deb1)<0.2], 2.5, 2.1)
      points(data_a[seqn_long == I_a, .(age, bmi)], pch=20, cex=cex_raw, col="dimgrey")
      # add points
      points(data_a[seqn_long == I_a, .(age, bmi_deb1)], pch=20, cex=2.5, col="black")
      
    } else {
      
      points(data_a[seqn_long == I_a, .(age, bmi)], pch=20, cex=2, col="black")
      
    } 
    
    if(verbose_a){
      cat("Observed data : \n")
      print(data_a[seqn_long == I_a, 
                        .(seqn, cycle, sex, educ, race, age_screening, yob,
                          bias, age, bmi, bmi_deb1, bmi_deb2, type_recall)]) 
      
      cat("\nMean posterior BMI curve :\n",  round(t(Z_a[, I_a, with=FALSE]), 1))
    }
  }
}


