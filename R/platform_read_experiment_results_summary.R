#' Read Experiment Results Summary
#'
#' Retrieves summary statistics for one or more experiments from the MSAID Platform.
#' Returns a data frame containing counts of PSMs, peptides, protein groups, and precursors
#' for each experiment. This provides a quick overview of experiment results without
#' downloading the full datasets.
#'
#' @param experiment_names Character vector or single string. Names of the experiments to retrieve summaries for.
#'   Can be a single experiment name (string) or multiple names (character vector). Default is empty vector c().
#' @param experiment_uuids Character vector. UUIDs of experiments to retrieve summaries for.
#'   Can be used as an alternative to or in combination with experiment_names when you know the specific experiment UUIDs.
#'   Default is empty vector c().
#'
#' @return A data frame containing summary statistics for the requested experiments.
#'   Each row represents one experiment with columns:
#'   \itemize{
#'     \item \code{experiment_uuid} - UUID of the experiment
#'     \item \code{experiment_name} - Name of the experiment (only included when experiment names were provided as input)
#'     \item \code{psms_count} - Number of peptide-spectrum matches
#'     \item \code{peptides_count} - Number of peptides
#'     \item \code{protein_groups_count} - Number of protein groups
#'     \item \code{precursors_count} - Number of precursors
#'   }
#'
#' @details
#' This function provides a lightweight way to get overview statistics for experiments
#' without downloading full result datasets. It's useful for:
#' \itemize{
#'   \item Quickly assessing experiment completion and result sizes
#'   \item Comparing result counts across multiple experiments
#'   \item Validating experiment processing before downloading full data
#'   \item Monitoring experiment progress and data quality
#' }
#'
#' The function accepts both experiment names and UUIDs. If both are provided, all specified
#' experiments will be included in the results. Experiment names are resolved to UUIDs
#' internally using the platform's experiment listing API.
#'
#' \strong{Count Filtering Criteria:}
#' The counts returned by this function are filtered as follows:
#' \itemize{
#'   \item \code{psms_count} - PSMs with file local FDR < 0.01
#'   \item \code{precursors_count} - Precursors with dataset global FDR < 0.01
#'   \item \code{peptides_count} - Peptides with dataset global FDR < 0.01
#'   \item \code{protein_groups_count} - Protein groups with dataset global FDR < 0.01
#' }
#'
#' @examples
#' \dontrun{
#' # Get summary for a single experiment by name
#' summary <- platform_read_experiment_results_summary(
#'   experiment_names = "my_experiment"
#' )
#'
#' # Get summaries for multiple experiments
#' summaries <- platform_read_experiment_results_summary(
#'   experiment_names = c("exp1", "exp2", "exp3")
#' )
#'
#' # Get summary by experiment UUID
#' summary <- platform_read_experiment_results_summary(
#'   experiment_uuids = c("550e8400-e29b-41d4-a716-446655440000")
#' )
#'
#' # Mix of names and UUIDs
#' summaries <- platform_read_experiment_results_summary(
#'   experiment_names = c("exp1", "exp2"),
#'   experiment_uuids = c("550e8400-e29b-41d4-a716-446655440000")
#' )
#' }
#'
#' @seealso \code{\link{platform_read_experiment_results}} for downloading full experiment data,
#'   \code{\link{platform_list_experiments}} for discovering available experiments,
#'   \code{\link{platform_set_debug}} for enabling debug output during API operations
#'
#' @importFrom httr GET content add_headers http_error
#' @export
platform_read_experiment_results_summary <- function(experiment_names = c(), experiment_uuids = c()) {
  # Input validation
  if (length(experiment_names) == 0 && length(experiment_uuids) == 0) {
    stop("Must provide either experiment_names or experiment_uuids (or both)")
  }
  
  # Convert single string to character vector
  if (is.character(experiment_names) && length(experiment_names) == 1) {
    experiment_names <- c(experiment_names)
  }
  if (is.character(experiment_uuids) && length(experiment_uuids) == 1) {
    experiment_uuids <- c(experiment_uuids)
  }
  
  # Get environment variables
  endpoint <- get_env_var("endpoint")
  id_token <- get_id_token()
  debug <- get_env_var("debug")
  
  if (debug) {
    message("endpoint: ", endpoint)
    message("experiment_names: ", paste(experiment_names, collapse = ", "))
    message("experiment_uuids: ", paste(experiment_uuids, collapse = ", "))
  }
  
  # Convert experiment names to UUIDs if provided and create name lookup
  all_uuids <- experiment_uuids
  uuid_to_name <- NULL
  
  if (length(experiment_names) > 0) {
    experiments <- experiment_names_to_objects(experiment_names)
    name_uuids <- sapply(experiments, function(exp) exp$uuid)
    all_uuids <- c(all_uuids, name_uuids)
    
    # Create lookup table from UUID to name for experiments that came from names
    uuid_to_name <- sapply(experiments, function(exp) exp$name)
    names(uuid_to_name) <- sapply(experiments, function(exp) exp$uuid)
  }
  
  # Remove duplicates
  all_uuids <- unique(all_uuids)
  
  if (debug) {
    message("resolved UUIDs: ", paste(all_uuids, collapse = ", "))
  }
  
  # Collect results for all experiments
  results_list <- list()
  
  for (uuid in all_uuids) {
    # Construct API endpoint for this experiment
    summary_endpoint <- sprintf("%s/v1/experiments/experiment/%s/results/summary", endpoint, uuid)
    
    if (debug) {
      message("calling: ", summary_endpoint)
    }
    
    # Make API call
    response <- httr::GET(summary_endpoint,
                          httr::add_headers(Authorization = paste("Bearer", id_token)))
    
    if (httr::http_error(response)) {
      stop(sprintf("Error retrieving summary for experiment (uuid: %s): %s", uuid, 
                   httr::content(response, as = "text")))
    }
    
    # Parse response
    summary_data <- httr::content(response, as = "parsed", type = "application/json")
    
    # Add to results - include experiment_name only if we have it
    result_data <- list(
      experiment_uuid = summary_data$experimentUuid
    )
    
    # Add experiment_name if available
    if (!is.null(uuid_to_name) && uuid %in% names(uuid_to_name)) {
      result_data$experiment_name <- uuid_to_name[[uuid]]
    }
    
    # Add count columns
    result_data$psms_count <- summary_data$psmsCount
    result_data$peptides_count <- summary_data$peptidesCount
    result_data$protein_groups_count <- summary_data$proteinGroupsCount
    result_data$precursors_count <- summary_data$precursorsCount
    
    results_list[[uuid]] <- data.frame(result_data, stringsAsFactors = FALSE)
  }
  
  # Combine all results into a single data frame
  if (length(results_list) == 1) {
    result_df <- results_list[[1]]
  } else {
    result_df <- do.call(rbind, results_list)
    rownames(result_df) <- NULL  # Reset row names
  }
  
  return(result_df)
}