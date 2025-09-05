test_that("shorten_cache_path removes organization_uuid and account_uuid portions", {
  # Test standard path with organization_uuid and account_uuid
  input_path <- "result-db/v1/psms.parquet/organization_uuid=abc123/account_uuid=def456/experiment_uuid=ghi789"
  expected_path <- "result-db/v1/psms.parquet/experiment_uuid=ghi789"
  
  result <- shorten_cache_path(input_path)
  expect_equal(result, expected_path)
})

test_that("shorten_cache_path handles different file extensions", {
  # Test with different file types
  input_path <- "result-db/v1/protein_groups.parquet/organization_uuid=org123/account_uuid=acc456/experiment_uuid=exp789"
  expected_path <- "result-db/v1/protein_groups.parquet/experiment_uuid=exp789"
  
  result <- shorten_cache_path(input_path)
  expect_equal(result, expected_path)
})

test_that("shorten_cache_path handles paths without organization_uuid", {
  # Test path that doesn't contain organization_uuid pattern
  input_path <- "result-db/v1/psms.parquet/experiment_uuid=ghi789"
  expected_path <- "result-db/v1/psms.parquet/experiment_uuid=ghi789"
  
  result <- shorten_cache_path(input_path)
  expect_equal(result, expected_path)
})

test_that("shorten_cache_path handles paths without experiment_uuid", {
  # Test path that doesn't contain experiment_uuid
  input_path <- "result-db/v1/psms.parquet/organization_uuid=abc123/account_uuid=def456"
  expected_path <- "result-db/v1/psms.parquet/organization_uuid=abc123/account_uuid=def456"
  
  result <- shorten_cache_path(input_path)
  expect_equal(result, expected_path)
})

test_that("shorten_cache_path handles complex UUID values", {
  # Test with realistic UUID values
  input_path <- "result-db/v1/peptides.parquet/organization_uuid=550e8400-e29b-41d4-a716-446655440000/account_uuid=6ba7b810-9dad-11d1-80b4-00c04fd430c8/experiment_uuid=6ba7b811-9dad-11d1-80b4-00c04fd430c8"
  expected_path <- "result-db/v1/peptides.parquet/experiment_uuid=6ba7b811-9dad-11d1-80b4-00c04fd430c8"
  
  result <- shorten_cache_path(input_path)
  expect_equal(result, expected_path)
})

test_that("shorten_cache_path handles paths with additional parameters after experiment_uuid", {
  # Test path with additional parameters after experiment_uuid
  input_path <- "result-db/v1/precursors.parquet/organization_uuid=abc123/account_uuid=def456/experiment_uuid=ghi789/some_other_param=value"
  expected_path <- "result-db/v1/precursors.parquet/experiment_uuid=ghi789/some_other_param=value"
  
  result <- shorten_cache_path(input_path)
  expect_equal(result, expected_path)
})

test_that("shorten_cache_path handles empty or invalid inputs", {
  # Test empty string - should throw error
  expect_error(shorten_cache_path(""), "Invalid cache path: path must start with 'result-db/', got: ")
  
  # Test simple path without expected structure - should throw error
  simple_path <- "/simple/path/file.parquet"
  expect_error(shorten_cache_path(simple_path), "Invalid cache path: path must start with 'result-db/', got: /simple/path/file.parquet")
})

test_that("shorten_cache_path validates input path format", {
  # Test various invalid path formats
  expect_error(shorten_cache_path("/result-db/v1/file.parquet"), "Invalid cache path: path must start with 'result-db/', got: /result-db/v1/file.parquet")
  expect_error(shorten_cache_path("/invalid/path"), "Invalid cache path: path must start with 'result-db/', got: /invalid/path")
  expect_error(shorten_cache_path("relative/path"), "Invalid cache path: path must start with 'result-db/', got: relative/path")
})

test_that("shorten_cache_path handles multiple organization_uuid patterns", {
  # Test edge case with multiple organization_uuid patterns (should match first occurrence only)
  input_path <- "result-db/v1/psms.parquet/organization_uuid=first/account_uuid=acc/experiment_uuid=exp/organization_uuid=second/experiment_uuid=exp2"
  expected_path <- "result-db/v1/psms.parquet/experiment_uuid=exp2"
  
  result <- shorten_cache_path(input_path)
  expect_equal(result, expected_path)
})