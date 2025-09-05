#' Clear Experiment Data Cache
#'
#' Removes cached parquet files from local storage for specified experiments and data aggregation levels.
#' This function helps manage disk space usage by clearing cached data that is no longer needed.
#' The cache can be cleared selectively by experiment and/or aggregation level, or entirely.
#'
#' @param experiment_names Character vector or NULL. Names of experiments whose cache should be cleared.
#'   If NULL (default), cache for ALL experiments will be cleared. Can be a single experiment name (string)
#'   or multiple names (character vector).
#' @param levels Character vector or NULL. Data aggregation levels to clear from cache.
#'   Must be subset of: "psms", "precursors", "peptides", "modified_peptides", "protein_groups",
#'   "sample_rollup_precursors", "sample_rollup_peptides", "sample_rollup_modified_peptides", "sample_rollup_protein_groups".
#'   If NULL (default), cache for ALL aggregation levels of the specified experiments will be cleared.
#'   Partial matching is supported.
#'
#' @return NULL (invisible). The function does not return a value but performs cache cleanup operations.
#'
#' @details
#' This function operates on the local cache directory where downloaded parquet files are stored.
#' Cache clearing is performed at the intersection of specified experiments and levels:
#' \itemize{
#'   \item If both \code{experiment_names} and \code{levels} are provided, only those specific combinations are cleared
#'   \item If only \code{experiment_names} is provided, all levels for those experiments are cleared
#'   \item If only \code{levels} is provided, those levels are cleared for all experiments
#'   \item If both are NULL, the entire cache is cleared
#' }
#'
#' The cache is organized by experiment UUID and aggregation level, so clearing cache for one experiment
#' or level does not affect others.
#'
#' @section Cache Management:
#' Regular cache maintenance is recommended because:
#' \itemize{
#'   \item Cached files can consume significant disk space for large experiments
#'   \item Cache clearing does not affect your ability to re-download data when needed
#'   \item Clearing cache ensures you get the most recent data if experiments have been updated
#' }
#'
#' @examples
#' \dontrun{
#' # Clear cache for specific experiments only
#' platform_clear_experiment_cache(experiment_names = c("exp1", "exp2"))
#'
#' # Clear cache for specific aggregation levels only
#' platform_clear_experiment_cache(levels = c("psms", "protein_groups"))
#'
#' # Clear cache for specific experiments and levels
#' platform_clear_experiment_cache(
#'   experiment_names = "my_experiment",
#'   levels = c("peptides", "protein_groups")
#' )
#'
#' # Clear entire cache (use with caution!)
#' platform_clear_experiment_cache()
#'
#' # Clear cache for a single experiment (all levels)
#' platform_clear_experiment_cache(experiment_names = "proteomics_study_2024")
#' }
#'
#' @seealso \code{\link{platform_read_experiment_results}} for reading experiment data,
#'   \code{\link{platform_set_debug}} for enabling debug output during cache operations
#'
#' @export
platform_clear_experiment_cache <- function(experiment_names=NULL, levels=NULL) {
  # Validate levels using match.arg if provided
  if (!is.null(levels)) {
    valid_levels <- c("psms", "precursors", "peptides", "modified_peptides", "protein_groups",
                      "sample_rollup_precursors", "sample_rollup_peptides", 
                      "sample_rollup_modified_peptides", "sample_rollup_protein_groups")
    levels <- sapply(levels, function(level) match.arg(level, valid_levels))
  }
  debug <- get_env_var("debug")
  if (debug){
    region <- get_env_var("region")
    stage <- get_env_var("stage")
    endpoint <- get_env_var("endpoint")
    message("region: ", region)
    message("stage: ", stage)
    message("endpoint: ", endpoint)
  }

  clean <- function(experiment_names,levels){
    for (level in levels) {
      clean_cache(experiment_names = experiment_names, level = level)
    }
   }

  if (is.null(levels)){
    levels <- c("psms", "precursors", "peptides", "modified_peptides", "protein_groups",
                "sample_rollup_precursors", "sample_rollup_peptides", 
                "sample_rollup_modified_peptides", "sample_rollup_protein_groups")
    clean(experiment_names,levels)
  }
  if (is.null(experiment_names)){
    clean_cache(delete_all=T)
  } else {
    clean(experiment_names,levels)
  }
}
