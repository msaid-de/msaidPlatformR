test_that("validate_and_normalize_timestamp handles POSIXct objects correctly", {
  # POSIXct with UTC timezone
  utc_time <- as.POSIXct("2024-01-01 10:30:00", tz = "UTC")
  expect_silent(result <- validate_and_normalize_timestamp(utc_time))
  expect_equal(result, "2024-01-01T10:30:00Z")
  
  # POSIXct with other timezone
  est_time <- as.POSIXct("2024-01-01 10:30:00", tz = "EST")
  expect_silent(result <- validate_and_normalize_timestamp(est_time))
  # Should convert to UTC
  expect_equal(result, "2024-01-01T15:30:00Z")
  
  # POSIXct without timezone should warn
  no_tz_time <- as.POSIXct("2024-01-01 10:30:00")
  attr(no_tz_time, "tzone") <- NULL
  expect_warning(
    result <- validate_and_normalize_timestamp(no_tz_time),
    "POSIXct object has no timezone.*Assuming UTC"
  )
  expect_equal(result, "2024-01-01T10:30:00Z")
})

test_that("validate_and_normalize_timestamp handles character strings correctly", {
  # Full ISO 8601 with Z should work silently
  expect_silent(result <- validate_and_normalize_timestamp("2024-01-01T10:30:00Z"))
  expect_equal(result, "2024-01-01T10:30:00Z")
  
  # ISO 8601 without timezone should warn
  expect_warning(
    result <- validate_and_normalize_timestamp("2024-01-01T10:30:00"),
    "has no timezone information.*Assuming UTC"
  )
  expect_equal(result, "2024-01-01T10:30:00Z")
  
  # Date only should warn and add time
  expect_warning(
    result <- validate_and_normalize_timestamp("2024-01-01"),
    "Date-only timestamp.*Assuming 00:00:00 UTC"
  )
  expect_equal(result, "2024-01-01T00:00:00Z")
  
  # Space separator should warn and convert
  expect_warning(
    result <- validate_and_normalize_timestamp("2024-01-01 10:30:00"),
    "has no timezone information.*Assuming UTC"
  )
  expect_equal(result, "2024-01-01T10:30:00Z")
  
  # Date with slashes should warn and convert
  expect_warning(
    result <- validate_and_normalize_timestamp("2024/01/01"),
    "Date.*with '/' separators.*Assuming 00:00:00 UTC"
  )
  expect_equal(result, "2024-01-01T00:00:00Z")
})

test_that("validate_and_normalize_timestamp handles timezone offsets", {
  # Timezone offset with colon
  expect_silent(result <- validate_and_normalize_timestamp("2024-01-01T10:30:00+02:00"))
  expect_equal(result, "2024-01-01T08:30:00Z")
  
  # Timezone offset without colon
  expect_silent(result <- validate_and_normalize_timestamp("2024-01-01T10:30:00+0200"))
  expect_equal(result, "2024-01-01T08:30:00Z")
  
  # Negative offset
  expect_silent(result <- validate_and_normalize_timestamp("2024-01-01T10:30:00-05:00"))
  expect_equal(result, "2024-01-01T15:30:00Z")
})

test_that("validate_and_normalize_timestamp error cases", {
  # Empty string
  expect_error(
    validate_and_normalize_timestamp(""),
    "Empty string is not a valid timestamp"
  )
  
  # Invalid format
  expect_error(
    validate_and_normalize_timestamp("invalid-date"),
    "Unrecognized timestamp format.*invalid-date"
  )
  
  # Wrong type
  expect_error(
    validate_and_normalize_timestamp(123),
    "Must be a POSIXct object or character string, got numeric"
  )
  
  # Invalid date values
  expect_error(
    validate_and_normalize_timestamp("2024-13-01"),
    "Invalid date.*2024-13-01.*Please check the year, month, and day values"
  )
  
  # Invalid time values  
  expect_error(
    validate_and_normalize_timestamp("2024-01-01T25:00:00"),
    "Invalid timestamp.*2024-01-01T25:00:00.*Expected formats"
  )
})

test_that("validate_and_normalize_timestamp generic messages", {
  # Check that warning messages are generic (no parameter names)
  expect_warning(
    validate_and_normalize_timestamp("2024-01-01"),
    "Date-only timestamp.*2024-01-01.*Assuming 00:00:00 UTC"
  )
  
  # Check that error messages are generic (no parameter names)
  expect_error(
    validate_and_normalize_timestamp(""),
    "Empty string is not a valid timestamp"
  )
})

test_that("normalize_character_timestamp internal function", {
  # Test various formats through the main function
  
  # Whitespace handling
  expect_warning(
    result <- validate_and_normalize_timestamp("  2024-01-01  "),
    "Date-only timestamp.*Assuming 00:00:00 UTC"
  )
  expect_equal(result, "2024-01-01T00:00:00Z")
  
  # Different separators and formats should provide helpful errors
  expect_error(
    validate_and_normalize_timestamp("2024.01.01"),
    "Unrecognized timestamp format.*2024.01.01.*Supported formats"
  )
})