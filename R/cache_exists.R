#' cache_exists
#' checks if given path exists in the cache directory
#' @param level result rollup level (one of: psms, precursors, peptides, modified_peptides, protein_groups)
#' @param path s3 path to the parquet file
#'
#' @return True or False
#'
#' @examples \dontrun{
#'   cache_exists("")
#' }
cache_exists <- function(level, path){
  cache_root <- get_env_var("cache_root")
  cache_path <- normalizePath(file.path(cache_root,level,path), winslash = "/", mustWork = FALSE)
  if (file.exists(cache_path)){
    return(T)
  }
  return (F)
}
