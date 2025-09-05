#' Validate and Normalize Timestamp
#'
#' Helper function to validate and normalize timestamp inputs for filtering experiments.
#' Handles POSIXct objects, character strings, and performs timezone and format validation.
#'
#' @param timestamp POSIXct object or character string representing a timestamp
#'
#' @return Character string in ISO 8601 format (YYYY-MM-DDTHH:MM:SSZ)
#'
#' @details
#' This function:
#' \itemize{
#'   \item Converts POSIXct objects to ISO 8601 format, preserving timezone information
#'   \item Validates character strings as valid timestamps
#'   \item Warns when timezone information is missing and assumes UTC
#'   \item Handles incomplete timestamps (date only) by assuming 00:00:00 UTC
#'   \item Provides informative error messages for invalid inputs
#' }
#'
#' @examples
#' \dontrun{
#' # POSIXct with timezone
#' validate_and_normalize_timestamp(as.POSIXct("2024-01-01 10:30:00", tz = "UTC"))
#' 
#' # Character string with timezone
#' validate_and_normalize_timestamp("2024-01-01T10:30:00Z")
#' 
#' # Date only - assumes 00:00:00 UTC with warning
#' validate_and_normalize_timestamp("2024-01-01")
#' }
validate_and_normalize_timestamp <- function(timestamp) {
  # Handle POSIXct objects
  if (inherits(timestamp, "POSIXct")) {
    # Get timezone information
    tz <- attr(timestamp, "tzone")
    if (is.null(tz) || tz == "") {
      warning("POSIXct object has no timezone information. Assuming UTC.")
      # Convert to UTC explicitly
      timestamp <- as.POSIXct(format(timestamp), tz = "UTC")
    }
    
    # Format to ISO 8601 in UTC
    return(format(timestamp, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  }
  
  # Handle character strings
  if (is.character(timestamp)) {
    # Check for common invalid formats first
    if (timestamp == "") {
      stop("Empty string is not a valid timestamp.")
    }
    
    # Try to detect different timestamp formats and normalize them
    normalized_timestamp <- normalize_character_timestamp(timestamp)
    
    # Validate the normalized timestamp by trying to parse it
    tryCatch({
      parsed <- as.POSIXct(normalized_timestamp, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      if (is.na(parsed)) {
        stop(sprintf("Could not parse timestamp '%s'. Expected formats: 'YYYY-MM-DD', 'YYYY-MM-DDTHH:MM:SS', or 'YYYY-MM-DDTHH:MM:SSZ'.", timestamp))
      }
    }, error = function(e) {
      stop(sprintf("Invalid timestamp '%s'. Expected formats: 'YYYY-MM-DD', 'YYYY-MM-DDTHH:MM:SS', or 'YYYY-MM-DDTHH:MM:SSZ'. Error: %s", timestamp, e$message))
    })
    
    return(normalized_timestamp)
  }
  
  # Invalid type
  stop(sprintf("Must be a POSIXct object or character string, got %s.", class(timestamp)[1]))
}

#' Normalize Character Timestamp
#'
#' Internal helper function to normalize character timestamp strings
#'
#' @param timestamp Character string timestamp
#'
#' @return Normalized timestamp string in ISO 8601 format
normalize_character_timestamp <- function(timestamp) {
  # Remove any leading/trailing whitespace
  timestamp <- trimws(timestamp)
  
  # Pattern matching for different formats
  
  # Full ISO 8601 with Z suffix (already normalized)
  if (grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", timestamp)) {
    return(timestamp)
  }
  
  # Full ISO 8601 with timezone offset - convert to Z
  if (grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}[+-]\\d{2}:?\\d{2}$", timestamp)) {
    # Parse with timezone and convert to UTC
    tryCatch({
      # Handle both formats: +02:00 and +0200
      if (grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}[+-]\\d{4}$", timestamp)) {
        # Format without colon: +0200
        parsed <- as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
      } else {
        # Format with colon: +02:00 - need to remove colon for %z
        timestamp_no_colon <- gsub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", timestamp)
        parsed <- as.POSIXct(timestamp_no_colon, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
      }
      if (!is.na(parsed)) {
        return(format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
      }
    }, error = function(e) {
      # Continue to other patterns
    })
  }
  
  # ISO 8601 without timezone - assume UTC and warn
  if (grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}$", timestamp)) {
    warning(sprintf("Timestamp '%s' has no timezone information. Assuming UTC.", timestamp))
    return(paste0(timestamp, "Z"))
  }
  
  # Date only format - assume 00:00:00 UTC and warn
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", timestamp)) {
    # Validate that this is actually a valid date before proceeding
    tryCatch({
      test_date <- as.Date(timestamp)
      if (is.na(test_date)) {
        stop("Invalid date")
      }
    }, error = function(e) {
      stop(sprintf("Invalid date '%s'. Please check the year, month, and day values.", timestamp))
    })
    
    warning(sprintf("Date-only timestamp '%s' provided. Assuming 00:00:00 UTC time.", timestamp))
    return(paste0(timestamp, "T00:00:00Z"))
  }
  
  # Try some common alternative formats
  
  # Format: YYYY/MM/DD
  if (grepl("^\\d{4}/\\d{2}/\\d{2}$", timestamp)) {
    warning(sprintf("Date '%s' provided with '/' separators. Assuming 00:00:00 UTC time. Use ISO format 'YYYY-MM-DD' for better compatibility.", timestamp))
    normalized <- gsub("/", "-", timestamp)
    return(paste0(normalized, "T00:00:00Z"))
  }
  
  # Format: YYYY-MM-DD HH:MM:SS (space separator)
  if (grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", timestamp)) {
    warning(sprintf("Timestamp '%s' has no timezone information. Assuming UTC.", timestamp))
    normalized <- gsub(" ", "T", timestamp)
    return(paste0(normalized, "Z"))
  }
  
  # If we get here, the format is not recognized
  stop(sprintf("Unrecognized timestamp format '%s'. Supported formats:
  - Date only: 'YYYY-MM-DD' (assumes 00:00:00 UTC)
  - ISO 8601: 'YYYY-MM-DDTHH:MM:SS' (assumes UTC if no timezone)
  - ISO 8601 with UTC: 'YYYY-MM-DDTHH:MM:SSZ'
  - ISO 8601 with timezone: 'YYYY-MM-DDTHH:MM:SS+HH:MM'", timestamp))
}