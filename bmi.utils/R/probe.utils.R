#' probe.utils: Functions used in the PROBE project
#'
#' Functions for preparation of data and analysis of results, including computation and visualization
#' projections of obesity prevalence and times spent obese. 
#' 
#' @section Prepare data:
#' Including functions \code{\link{import_nhanes_f}}, \code{\link{clean_nhanes_f}}, 
#' \code{\link{add_weights_f}}, \code{\link{add_bias_f}}, \code{\link{add_bias25_f}}
#' 
#' 
#' @section Load data and results:
#' Including functions \code{\link{load_results_f}} and \code{\link{complete_Zage_f}}
#' 
#' @section Average posterior of mean BMI functions and individual trajectories:
#' Functions \code{\link{muSRES_all_f}}, 
#' \code{\link{muSRES_race_f}}, \code{\link{muSRES_interactive_f}} and \code{\link{plot_traj_f}}
#' 
#' @section Projection of obesity prevalence:
#' Functions \code{\link{obese_prop_f}}, \code{\link{prevalence_f}}, 
#' \code{\link{graphs_prev_f}} and \code{\link{store_prevalence_f}}
#' 
#' @section Projection of time spent obese:
#' Functions \code{\link{timeSpent_f}}, \code{\link{tso_f}}
#' 
#' @docType package
#' @name probe.utils
NULL
