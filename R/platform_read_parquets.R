#' platform_read_parquets
#' reads the downloaded experiment parquet file
#' @param dir cache dir
#' @param level result rollup level (one of: psms, precursors, peptides, modified_peptides, protein_groups)
#' @param max_q_value maximum q value
#' @param max_global_q_value maximum global q value
#' @param include_decoys include decoys (TRUE/FALSE)
#' @param include_columns include columns (TRUE/FALSE)
#' @param exclude_columns exclude columns (TRUE/FALSE)
#' @param exclude_array_columns exclude array columns (TRUE/FALSE)
#' @importFrom arrow open_dataset
#' @importFrom dplyr filter select mutate compute
#' @importFrom magrittr %>%
#' @return data
#' @examples \dontrun{
#' platform_read_parquets("")
#' }

platform_read_parquets <- function (dir,level,max_q_value=1,max_global_q_value=1,include_decoys=F, include_columns=NULL, exclude_columns=c(), exclude_array_columns=F) {
  array_columns <- c()

  ds <- arrow::open_dataset(dir, hive_style = T)

  # Check if dataset is empty and return early
  if (nrow(ds) == 0) {
    return(ds)
  }

  all_columns <- unlist(sapply(ds$schema$fields, function(field) {
    field$name
  }))

  # Determine user-requested columns (NULL means all)
  user_include <- if(is.null(include_columns)) all_columns else include_columns

  if(exclude_array_columns) {
    array_columns <- sapply(ds$schema$fields, function(field) {
      if (grepl("list", field$type$ToString(), ignore.case = TRUE))
        field$name
      else
        NULL
    })
    array_columns <- array_columns[!sapply(array_columns, is.null)]
    array_columns <- unlist(array_columns)
  }

  if (!include_decoys){
    ds <- ds %>% dplyr::filter(DECOY == F)
  }

  if(level %in% c("psms", "precursors", "peptides", "modified_peptides")) {
    ds <- ds %>%
      dplyr::filter(Q_VALUE <= max_q_value)
  }

  if(level != "psms") {
    ds <- ds %>%
      dplyr::filter(GLOBAL_Q_VALUE <= max_global_q_value)
  }

  # Build final column set:
  # - Start with user-requested columns
  # - include_columns takes priority over exclude_columns
  # - Always include experiment_uuid (needed by caller for filtering)
  # - Always exclude organization_uuid and account_uuid (internal partition columns)
  columns_to_exclude <- c(exclude_columns, array_columns)
  if (!is.null(include_columns)) {
    # User-provided include_columns take priority over exclude_columns
    columns_to_exclude <- columns_to_exclude[!columns_to_exclude %in% include_columns]
  }
  final_columns <- user_include[!user_include %in% columns_to_exclude]
  if ("experiment_uuid" %in% all_columns) {
    final_columns <- union(final_columns, "experiment_uuid")
  }
  final_columns <- setdiff(final_columns, c("organization_uuid", "account_uuid"))

  ds <- ds %>%
    dplyr::select(dplyr::any_of(final_columns)) %>%
    dplyr::mutate(local_file_path = arrow::add_filename()) %>%
    dplyr::compute()

  return(ds)
}
