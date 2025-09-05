#' Read Experiment Results from MSAID Platform
#'
#' Downloads and reads mass spectrometry experiment results from the MSAID Platform into an R data structure.
#' The function handles data caching automatically - files are downloaded once and stored locally for subsequent access.
#' Results can be filtered by quality metrics and customized by column selection and aggregation level.
#'
#' @param level Character string. Data aggregation level. Must be one of: "psms", "precursors", "peptides", "modified_peptides", "protein_groups", "sample_rollup_precursors", "sample_rollup_peptides", "sample_rollup_modified_peptides", or "sample_rollup_protein_groups". Partial matching is supported.
#' @param experiment_names Character vector or single string. Names of the experiments to retrieve. Can be a single experiment name (string) or multiple names (character vector). Default is empty vector c().
#' @param experiment_uuids Character vector. UUIDs of experiments to retrieve. Can be used as an alternative to or in combination with experiment_names when you know the specific experiment UUIDs. Default is empty vector c().
#' @param max_q_value Numeric. Maximum local q-value threshold for filtering results. Default is 0.01.
#' @param max_global_q_value Numeric. Maximum global q-value threshold for filtering results. Default is 0.01.
#' @param include_decoys Logical. Whether to include decoy hits in the results. Default is FALSE.
#' @param include_columns Character vector or NULL. Specific columns to include in the results. If NULL, all columns are included. Default is NULL. See Details section for available columns by level.
#' @param exclude_columns Character vector. Columns to exclude from the results. Default is empty vector c(). See Details section for available columns by level.
#' @param exclude_array_columns Logical. Whether to exclude array/list columns for simpler data frames. Default is FALSE.
#' @param out_format Character string. Output format for the results. Must be one of: "data_table" (default), "data_frame", or "arrow_dataset". Partial matching is supported.
#'
#' @return A data.table (if out_format="data_table", default), data frame (if out_format="data_frame"), or arrow dataset (if out_format="arrow_dataset") containing the filtered experiment results
#'
#' @details
#' Available Columns by Data Level
#'
#' The following columns are available for each data aggregation level. Use these column names with the
#' include_columns and exclude_columns parameters to customize your results.
#'
#' PSMS (32 columns): PSM_ID, SEQUENCE, MODIFIED_SEQUENCE, PEPTIDE_ID, MODIFIED_PEPTIDE_ID, PRECURSOR_ID,
#' MASS, PRECURSOR_CHARGE, LENGTH, M_Z, MISSED_CLEAVAGES, RETENTION_TIME, RETENTION_TIME_PREDICTION,
#' SPECTRAL_ANGLE, CTP, COEFF, MATCHED_PEAKS, TOP_PEAKS_SHARED, SCAN_NUMBER_IN_FILE, Q_VALUE, SE_SCORE,
#' PEP, IS_AMBIGUOUS, DECOY, LOCALIZATION_SEQUENCE, LOCALIZATION_SCORE, RAW_FILE_NAME, SAMPLE_NAME,
#' QUANTIFICATION, plus array columns: PSM_IDS, POSITION_IN_PROTEIN_IDS, PROTEIN_IDS, PROTEIN_SITES,
#' QUANTIFICATION_TRACE_RETENTION_TIMES, QUANTIFICATION_TRACE_INTENSITIES
#'
#' PRECURSORS (35 columns): PRECURSOR_ID, SEQUENCE, MODIFIED_SEQUENCE, PEPTIDE_ID, MODIFIED_PEPTIDE_ID,
#' MASS, PRECURSOR_CHARGE, LENGTH, M_Z, MISSED_CLEAVAGES, MIN_RETENTION_TIME, MAX_RETENTION_TIME,
#' Q_VALUE, SE_SCORE, PEP, GLOBAL_Q_VALUE, GLOBAL_SE_SCORE, GLOBAL_PEP, MAX_SPECTRAL_ANGLE, MAX_CTP,
#' IS_AMBIGUOUS, DECOY, IS_IDENTIFIED_BY_MBR, LOCALIZATION_SEQUENCE, LOCALIZATION_SCORE, RAW_FILE_NAME,
#' SAMPLE_NAME, QUANTIFICATION, COUNT_PSMS, plus array columns: PSM_IDS, POSITION_IN_PROTEIN_IDS,
#' PROTEIN_IDS, PROTEIN_SITES, QUANTIFICATION_TRACE_RETENTION_TIMES, QUANTIFICATION_TRACE_INTENSITIES
#'
#' PEPTIDES (23 columns): PEPTIDE_ID, SEQUENCE, LENGTH, Q_VALUE, SE_SCORE, PEP, GLOBAL_Q_VALUE,
#' GLOBAL_SE_SCORE, GLOBAL_PEP, IS_AMBIGUOUS, DECOY, IS_IDENTIFIED_BY_MBR, RAW_FILE_NAME, SAMPLE_NAME,
#' QUANTIFICATION, COUNT_PSMS, COUNT_PRECURSORS, COUNT_MODIFIED_PEPTIDES, plus array columns: PSM_IDS,
#' PRECURSOR_IDS, MODIFIED_PEPTIDE_IDS, POSITION_IN_PROTEIN_IDS, PROTEIN_IDS
#'
#' MODIFIED_PEPTIDES (30 columns): MODIFIED_PEPTIDE_ID, SEQUENCE, MODIFIED_SEQUENCE, PEPTIDE_ID, MASS,
#' LENGTH, MISSED_CLEAVAGES, Q_VALUE, SE_SCORE, PEP, GLOBAL_Q_VALUE, GLOBAL_SE_SCORE, GLOBAL_PEP,
#' MAX_SPECTRAL_ANGLE, MAX_CTP, IS_AMBIGUOUS, DECOY, IS_IDENTIFIED_BY_MBR, LOCALIZATION_SEQUENCE,
#' LOCALIZATION_SCORE, RAW_FILE_NAME, SAMPLE_NAME, QUANTIFICATION, COUNT_PSMS, COUNT_PRECURSORS,
#' plus array columns: PSM_IDS, PRECURSOR_IDS, POSITION_IN_PROTEIN_IDS, PROTEIN_IDS, PROTEIN_SITES
#'
#' PROTEIN_GROUPS (21 columns): PROTEIN_GROUP_ID, GLOBAL_Q_VALUE, GLOBAL_SE_SCORE, DECOY,
#' RAW_FILE_NAME, SAMPLE_NAME, QUANTIFICATION, COUNT_PSMS, COUNT_PRECURSORS, COUNT_MODIFIED_PEPTIDES,
#' COUNT_PEPTIDES, plus array columns: PROTEIN_IDS, FASTA_HEADERS, GENE_NAMES, PROTEIN_IDENTIFIERS,
#' TAXONOMY_IDS, ORGANISMS
#'
#' SAMPLE_ROLLUP_PRECURSORS (29 columns): PRECURSOR_ID, SEQUENCE, MODIFIED_SEQUENCE, PEPTIDE_ID,
#' MODIFIED_PEPTIDE_ID, MASS, PRECURSOR_CHARGE, LENGTH, M_Z, MISSED_CLEAVAGES, MIN_RETENTION_TIME,
#' MAX_RETENTION_TIME, SAMPLE_Q_VALUE, SAMPLE_SE_SCORE, SAMPLE_PEP, GLOBAL_Q_VALUE, GLOBAL_SE_SCORE,
#' GLOBAL_PEP, MAX_SPECTRAL_ANGLE, MAX_CTP, IS_AMBIGUOUS, DECOY, IS_IDENTIFIED_BY_MBR, SAMPLE_NAME,
#' QUANTIFICATION, COUNT_PSMS, plus array columns: PSM_IDS, POSITION_IN_PROTEIN_IDS, PROTEIN_IDS
#'
#' SAMPLE_ROLLUP_PEPTIDES (23 columns): PEPTIDE_ID, SEQUENCE, LENGTH, SAMPLE_Q_VALUE, SAMPLE_SE_SCORE,
#' SAMPLE_PEP, GLOBAL_Q_VALUE, GLOBAL_SE_SCORE, GLOBAL_PEP, IS_AMBIGUOUS, DECOY, IS_IDENTIFIED_BY_MBR,
#' MAX_SPECTRAL_ANGLE, MAX_CTP, SAMPLE_NAME, QUANTIFICATION, COUNT_PSMS, COUNT_PRECURSORS,
#' COUNT_MODIFIED_PEPTIDES, plus array columns: PSM_IDS, PRECURSOR_IDS, MODIFIED_PEPTIDE_IDS,
#' POSITION_IN_PROTEIN_IDS, PROTEIN_IDS
#'
#' SAMPLE_ROLLUP_MODIFIED_PEPTIDES (26 columns): MODIFIED_PEPTIDE_ID, SEQUENCE, MODIFIED_SEQUENCE,
#' PEPTIDE_ID, MASS, LENGTH, MISSED_CLEAVAGES, SAMPLE_Q_VALUE, SAMPLE_SE_SCORE, SAMPLE_PEP,
#' GLOBAL_Q_VALUE, GLOBAL_SE_SCORE, GLOBAL_PEP, MAX_SPECTRAL_ANGLE, MAX_CTP, IS_AMBIGUOUS, DECOY,
#' IS_IDENTIFIED_BY_MBR, SAMPLE_NAME, QUANTIFICATION, COUNT_PSMS, COUNT_PRECURSORS, plus array columns:
#' PSM_IDS, PRECURSOR_IDS, POSITION_IN_PROTEIN_IDS, PROTEIN_IDS
#'
#' SAMPLE_ROLLUP_PROTEIN_GROUPS (20 columns): PROTEIN_GROUP_ID, GLOBAL_Q_VALUE, GLOBAL_SE_SCORE, DECOY,
#' SAMPLE_NAME, QUANTIFICATION, COUNT_PSMS, COUNT_PRECURSORS, COUNT_MODIFIED_PEPTIDES, COUNT_PEPTIDES,
#' plus array columns: PROTEIN_IDS, FASTA_HEADERS, GENE_NAMES, PROTEIN_IDENTIFIERS, TAXONOMY_IDS,
#' ORGANISMS
#'
#' Note: Use exclude_array_columns=TRUE to remove complex array columns for simpler data frames.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Read PSM-level data from a single experiment
#' data <- platform_read_experiment_results(
#'   level = "psms",
#'   experiment_names = "my_experiment",
#'   max_global_q_value = 0.01
#' )
#'
#' # Read protein group data from multiple experiments
#' data <- platform_read_experiment_results(
#'   level = "protein_groups",
#'   experiment_names = c("exp1", "exp2", "exp3"),
#'   max_q_value = 0.05,
#'   include_decoys = FALSE,
#'   exclude_array_columns = TRUE
#' )
#'
#' # Read specific columns only
#' data <- platform_read_experiment_results(
#'   level = "peptides",
#'   experiment_names = "proteomics_study",
#'   include_columns = c("SEQUENCE", "CHARGE", "INTENSITY"),
#'   max_global_q_value = 0.01
#' )
#'
#' # Read sample rollup protein group data
#' data <- platform_read_experiment_results(
#'   level = "sample_rollup_protein_groups",
#'   experiment_names = "my_experiment"
#' )
#'
#' # Read data as data.table for high-performance operations (default)
#' data <- platform_read_experiment_results(
#'   level = "protein_groups",
#'   experiment_names = "my_experiment",
#'   out_format = "data_table"
#' )
#'
#' # Read data as data frame
#' data <- platform_read_experiment_results(
#'   level = "peptides",
#'   experiment_names = "my_experiment",
#'   out_format = "data_frame"
#' )
#'
#' # Read data as arrow dataset for memory-efficient processing
#' data <- platform_read_experiment_results(
#'   level = "psms",
#'   experiment_names = "large_experiment",
#'   out_format = "arrow_dataset"
#' )
#'
#' ## Working with Arrow Datasets
#'
#' # When using out_format="arrow_dataset", the function returns an Arrow dataset
#' # object that provides memory-efficient processing for large datasets. Arrow
#' # datasets use lazy evaluation and require dplyr syntax for operations.
#'
#' # Basic Arrow Dataset Operations:
#' # Get precursor data as arrow dataset
#' ds <- platform_read_experiment_results(
#'   level = "precursors",
#'   experiment_names = "large_experiment",
#'   out_format = "arrow_dataset"
#' )
#'
#' # Filter data (lazy evaluation - no computation until collect())
#' filtered_ds <- ds %>%
#'   dplyr::filter(GLOBAL_Q_VALUE <= 0.01, PRECURSOR_CHARGE >= 2) %>%
#'   dplyr::select(SEQUENCE, MODIFIED_SEQUENCE, PRECURSOR_CHARGE, QUANTIFICATION)
#'
#' # Collect results to R data frame (triggers computation)
#' results <- filtered_ds %>% dplyr::collect()
#'
#' # Aggregation and Summarization:
#' # Group by sample and calculate summary statistics
#' summary_ds <- ds %>%
#'   dplyr::filter(GLOBAL_Q_VALUE <= 0.01) %>%
#'   dplyr::group_by(SAMPLE_NAME, PRECURSOR_CHARGE) %>%
#'   dplyr::summarise(
#'     precursor_count = dplyr::n(),
#'     median_intensity = median(QUANTIFICATION, na.rm = TRUE),
#'     mean_retention_time = mean(MIN_RETENTION_TIME, na.rm = TRUE),
#'     max_retention_time = mean(MAX_RETENTION_TIME, na.rm = TRUE),
#'     .groups = "drop"
#'   ) %>%
#'   dplyr::collect()
#'
#' # String Pattern Matching:
#' # Find precursors containing specific amino acid patterns
#' pattern_matches <- ds %>%
#'   dplyr::filter(
#'     stringr::str_detect(SEQUENCE, "PEPTIDE|PROTEIN"),
#'     GLOBAL_Q_VALUE <= 0.01
#'   ) %>%
#'   dplyr::select(SEQUENCE, MODIFIED_SEQUENCE, SAMPLE_NAME, QUANTIFICATION) %>%
#'   dplyr::collect()
#'
#' # Working with Multiple Samples:
#' # Compare quantification across samples
#' sample_comparison <- ds %>%
#'   dplyr::filter(GLOBAL_Q_VALUE <= 0.01, !is.na(QUANTIFICATION)) %>%
#'   dplyr::group_by(SAMPLE_NAME) %>%
#'   dplyr::summarise(
#'     total_precursors = dplyr::n(),
#'     median_intensity = median(QUANTIFICATION),
#'     q75_intensity = quantile(QUANTIFICATION, 0.75),
#'     unique_sequences = dplyr::n_distinct(SEQUENCE),
#'     unique_modified_sequences = dplyr::n_distinct(MODIFIED_SEQUENCE),
#'     .groups = "drop"
#'   ) %>%
#'   dplyr::arrange(desc(median_intensity)) %>%
#'   dplyr::collect()
#'
#' # Memory-Efficient Processing for Large Datasets:
#' # Process data in chunks without loading everything into memory
#' high_confidence_precursors <- ds %>%
#'   dplyr::filter(
#'     GLOBAL_Q_VALUE <= 0.01,
#'     PRECURSOR_CHARGE %in% c(2, 3, 4),
#'     !DECOY,
#'     !IS_IDENTIFIED_BY_MBR  # Only direct identifications
#'   ) %>%
#'   dplyr::select(PRECURSOR_ID, SEQUENCE, MODIFIED_SEQUENCE, SAMPLE_NAME,
#'                 QUANTIFICATION, MIN_RETENTION_TIME, MAX_RETENTION_TIME,
#'                 GLOBAL_Q_VALUE) %>%
#'   dplyr::arrange(GLOBAL_Q_VALUE) %>%
#'   dplyr::collect()
#'
#' # Combining with Other Arrow Operations:
#' # Export filtered results to Parquet for further analysis
#' ds %>%
#'   dplyr::filter(GLOBAL_Q_VALUE <= 0.01, PRECURSOR_CHARGE >= 2) %>%
#'   dplyr::select(SEQUENCE, MODIFIED_SEQUENCE, SAMPLE_NAME, QUANTIFICATION,
#'                 MIN_RETENTION_TIME, MAX_RETENTION_TIME) %>%
#'   arrow::write_parquet("filtered_precursors.parquet")
#'
#' # Join with external metadata (assuming metadata_df exists)
#' enriched_data <- ds %>%
#'   dplyr::filter(GLOBAL_Q_VALUE <= 0.01) %>%
#'   dplyr::left_join(
#'     arrow::arrow_table(metadata_df),
#'     by = "SAMPLE_NAME"
#'   ) %>%
#'   dplyr::collect()
#'
#' # Charge State Analysis:
#' # Analyze precursor charge state distribution
#' charge_analysis <- ds %>%
#'   dplyr::filter(GLOBAL_Q_VALUE <= 0.01, !DECOY) %>%
#'   dplyr::group_by(SAMPLE_NAME, PRECURSOR_CHARGE) %>%
#'   dplyr::summarise(
#'     count = dplyr::n(),
#'     median_mass = median(MASS, na.rm = TRUE),
#'     median_mz = median(M_Z, na.rm = TRUE),
#'     .groups = "drop"
#'   ) %>%
#'   dplyr::collect()
#'
#' # Performance Tips for Arrow Datasets:
#' # - Use dplyr::filter() early to reduce data size before other operations
#' # - Apply dplyr::select() to choose only needed columns before collect()
#' # - Use dplyr::collect() only when you need the data in R memory
#' # - Leverage Arrow's columnar format for efficient aggregations
#' # - Chain multiple dplyr operations before collect() for optimal performance
#' }
#' @importFrom httr GET POST content http_error add_headers http_error
platform_read_experiment_results <- function(level = c("psms", "precursors", "peptides", "modified_peptides", "protein_groups",
                                                                                        "sample_rollup_precursors", "sample_rollup_peptides",
                                                                                        "sample_rollup_modified_peptides", "sample_rollup_protein_groups"),
                                             experiment_names=c(),experiment_uuids=c(), max_q_value=0.01,max_global_q_value=0.01,include_decoys=F,
                                             include_columns=NULL, exclude_columns=c(), exclude_array_columns=F, out_format=c("data_table", "data_frame", "arrow_dataset")) {
  # Validate that level is provided
  if (missing(level) || is.null(level) || length(level) == 0 || level == "") {
    stop("Level parameter must be provided")
  }

  level <- match.arg(level)
  out_format <- match.arg(out_format)

  # Convert single string to character vector
  if (is.character(experiment_names) && length(experiment_names) == 1) {
    experiment_names <- c(experiment_names)
  }

  # Validate that at least one of experiment_names or experiment_uuids is provided
  if (length(experiment_names) == 0 && length(experiment_uuids) == 0) {
    stop("Either experiment_names or experiment_uuids must be provided")
  }

  region <- get_env_var("region")
  stage <- get_env_var("stage")
  endpoint <- get_env_var("endpoint")
  id_token <- get_id_token()
  cache_root <- get_env_var("cache_root")
  debug <- get_env_var("debug")
  if (debug){
    message("region: ", region)
    message("stage: ", stage)
    message("endpoint: ", endpoint)
  }

  experiments <- experiment_names_to_objects(experiment_names)

  for (experiment in experiments) {
    experiment_uuids <- append(experiment_uuids,experiment$uuid)

    # Check if sample rollup is available for sample rollup levels
    if (startsWith(level, "sample_rollup_")) {
      if (is.null(experiment$sampleRollup) || !experiment$sampleRollup) {
        stop(sprintf("Experiment '%s' (uuid: %s) does not have sample rollup data available. Cannot retrieve %s level data.",
                     experiment$name, experiment$uuid, level))
      }
    }
  }

  experiment_uuids <- unique(experiment_uuids)

  for (experiment_uuid in experiment_uuids){
    # Check if this is a sample rollup level
    if (startsWith(level, "sample_rollup_")) {
      presigned_url_endpoint <- sprintf("%s/v1/experiments/experiment/%s/results/sampleRollup/%s/parquets/presignedUrls",
                                        endpoint, experiment_uuid, toupper(level))
    } else {
      # Use original endpoint for regular levels
      presigned_url_endpoint <- sprintf("%s/v1/experiments/experiment/%s/results/%s/parquets/presignedUrls",
                                        endpoint,experiment_uuid, toupper(level))
    }

    response <- httr::GET(presigned_url_endpoint,
                          httr::add_headers(Authorization = paste("Bearer", id_token)))
    if (httr::http_error(response)){
      stop(sprintf("error retrieving data for experiment (uuid: %s): %s", experiment_uuid, response))
    }
    presigned_content <- httr::content(response,as="parsed",type="application/json")
    if (length(presigned_content) < 1){
      stop(sprintf("could not retrieve presigned url for experiment (uuid: %s)",experiment_uuid))
    }

    for (presigned_item in presigned_content) {
      s3_path <- shorten_cache_path(presigned_item$path)
      presigned_url <- presigned_item$presignedUrl
      if (cache_exists(level, s3_path)){
        # Cache exists, skip download
      } else {
        download_parquet_to_cache(level=level,s3_path=s3_path, presigned_url=presigned_url)
      }
    }

  }
  cache_level_path <- normalizePath(file.path(cache_root,level), winslash = "/", mustWork = FALSE)
  entire_cache <- platform_read_parquets(cache_level_path,level,
                                         max_q_value=max_q_value,
                                         max_global_q_value=max_global_q_value,
                                         include_decoys=include_decoys,
                                         include_columns=include_columns,
                                         exclude_columns=exclude_columns,
                                         exclude_array_columns=exclude_array_columns)
  ds <- entire_cache %>% dplyr::filter(experiment_uuid %in% experiment_uuids)

  # Convert to requested output format
  if (out_format == "data_table") {
    print("Converting to data.table")
    ds <- ds %>% dplyr::collect()
    data.table::setDT(ds)
  } else if (out_format == "data_frame") {
    print("Converting to data frame")
    ds <- ds %>% as.data.frame()
  } else if (out_format == "arrow_dataset") {
    print("Returning as arrow dataset")
    # ds is already an arrow dataset, no conversion needed
  } else {
    stop(sprintf("Invalid out_format: '%s'. Must be one of: data_table, data_frame, arrow_dataset", out_format))
  }

  return(ds)
}

