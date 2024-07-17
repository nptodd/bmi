#' Interactive plot of average posterior mean BMI functions
#' 
#' Should only be run if strata are defined based on :
#' Sex, Race, Education and Smoking
#'
#' @param sex_a Either female (\code{"f"}) or male (\code{"m"})
#' @param RS_a set of parameters analyzed
#' @param strata_a strata analyzed
#' 
#' @return plotly visualization.
#'
#' @export 

muSRES_interactive_f=function(sex_a, RS_a, strata_a){
  
  strata_loc <- strata_a[substr(strata_a, 1, 1)==sex_a]
  
  # graphical parameters
  races_dt = data.table(
    col = c("navy", "red","orange", "forestgreen", "pink"),
    race = c("w", "h", "b", "o", "bh"),
    leg = c( "White", "Hispanic", "Black",  "Other", "Bl. & Hisp."),
    add=c(F, T, T, T, T)  )
  
  
  # loading results
  age_mat = lapply(X = strata_loc, 
                   FUN = load_results_f, 
                   RS_a=RS_a,
                   res_type_a = "short",
                   var_a="param_uc",
                   var_type_a = "mat")
  age_mat <- lapply(age_mat, 
                    function(ages){ ages$param.uc[,1,1]$tau }   )
  
  out_grid_l = lapply(X = strata_loc, 
                      FUN = load_results_f, 
                      RS_a=RS_a,
                      res_type_a = "short",
                      var_a="out_ucgrid_reduced",
                      var_type_a = "mat") 
  
  mu_mat = lapply(X = out_grid_l, 
                  FUN = function(outgrid){
                    outgrid$`out.ucgrid.reduced`[,,1][["mu"]]} )
  muCI1_mat = lapply(X = out_grid_l, 
                     FUN = function(outgrid, b){
                       outgrid$`out.ucgrid.reduced`[,,1][["mu.CI"]][,b] }, b=1)
  muCI2_mat = lapply(X = out_grid_l, 
                     FUN = function(outgrid, b){
                       outgrid$`out.ucgrid.reduced`[,,1][["mu.CI"]][,b] }, b=2)
  
  names(mu_mat) <- names(muCI1_mat) <- names(muCI2_mat) <-  names(age_mat) <- strata_loc
  
  
  dtb_all <- data.table(tau=as.vector(age_mat[[1]]), X=as.vector(mu_mat[[1]]))
  
  p <- plotly::plot_ly(data=dtb_all, x=~tau, y=~X, name = names(age_mat)[[1]],
                       type = 'scatter', mode = 'lines')
  
  for(I in 2:length(mu_mat)){
    
    dtb_loc <- data.table(tau=as.vector(age_mat[[I]]), X=as.vector(mu_mat[[I]]))
    
    p <-  plotly::add_trace(p, data=dtb_loc, y = ~X, name = names(age_mat)[[I]], 
                            mode = 'lines') 
  }
  
  p  <- plotly::layout(p, xaxis = list(title="Age"), 
                       yaxis = list(title="Mean BMI"))
  
  return(p)
}
