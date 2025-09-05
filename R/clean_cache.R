#' clean_cache
#' cleans the caches of given experiment names and levels
#' @param experiment_names character vector of experiment names
#' @param level psms,precursors,protein_groups
#' @param delete_all boolean that deletes all of the caches
#'
#' @return none
#'
#' @examples  \dontrun{
#'   clean_cache(c("exp1", "exp2"), "psms")
#' }
clean_cache <- function(experiment_names=NULL,level=NULL,delete_all=F) {
  cache_root <- get_env_var("cache_root")
  if (delete_all){
    unlink(cache_root, recursive = TRUE, force = TRUE)
    return()
  }

  level <- tolower(level)
  cache_level_path <- normalizePath(file.path(cache_root, level), winslash = "/", mustWork = FALSE)

  # Check if cache directory exists
  if (!file.exists(cache_level_path)) {
    return()
  }

  # Return early if no experiment names provided (delete nothing)
  if (is.null(experiment_names) || length(experiment_names) == 0) {
    return()
  }

  # Use platform_read_parquets to get the dataset with local_file_path column
  tryCatch({
    ds <- platform_read_parquets(cache_level_path, level)

    # Check if dataset is empty and return early
    if (nrow(ds) == 0) {
      return()
    }

    # Filter by experiments (experiment_names is guaranteed to be non-null here)
    experiments <- experiment_names_to_objects(experiment_names)
    exp_uuids <- sapply(experiments, function(exp) exp$uuid)
    ds <- ds %>% dplyr::filter(experiment_uuid %in% exp_uuids)

    # Get unique file paths to delete
    file_paths <- ds %>%
      dplyr::select(local_file_path) %>%
      dplyr::distinct() %>%
      dplyr::collect() %>%
      dplyr::pull(local_file_path)

    # Delete each file
    for (file_path in file_paths) {
      if (file.exists(file_path) && endsWith(file_path, ".parquet")) {
        print(sprintf("deleting: %s", file_path))
        unlink(file_path, force = TRUE)
      } else if (file.exists(file_path)) {
        print(sprintf("skipping non-parquet file: %s", file_path))
      }
    }
  }, error = function(e) {
    print(sprintf("Error reading cache for cleanup: %s", e$message))
    print("Cache may be empty or corrupted")
  })
}

