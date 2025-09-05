#' download_parquet_to_cache
#' downloads the parquet file to the cache directory
#' @param level result rollup level (one of: psms, precursors, peptides, modified_peptides, protein_groups)
#' @param s3_path cache path
#' @param presigned_url presigned url for the experiment files
#'
#' @importFrom httr GET content
#' @examples  \dontrun{
#'   create_cache("","","","","")
#' }
download_parquet_to_cache <- function(level, s3_path, presigned_url) {
  cache_root <- get_env_var("cache_root")
  cache_fp <- normalizePath(file.path(cache_root,level,s3_path), winslash = "/", mustWork = FALSE)
  print("Downloading the parquet file to the cache..")
  print(sprintf("Cache file path: %s",cache_fp))
  response <- httr::GET(presigned_url)
  if (httr::http_error(response)) {
    stop("Failed to retrieve the parquet file")
  }
  cache_dp <- dirname(cache_fp)
  dir.create(cache_dp, showWarnings = FALSE, recursive = TRUE)
  writeBin(httr::content(response, "raw"), cache_fp)
  print("Finished downloading the parquet file to the cache")
}
