test_that("download_parquet_to_cache writes file to cache", {
  temp_cache_dir <- withr::local_tempdir()
  s3_path <- "test/data.parquet"
  presigned_url <- "https://fake-s3.com/fake.parquet"
  expected_fp <- file.path(temp_cache_dir, "psms", s3_path)

  # Mock get_env_var to return our temp directory
  mock_get_env_var <- mockery::mock(temp_cache_dir)

  # Mock httr::GET to return a fake response object
  fake_response <- structure(list(status_code = 200L), class = "response")
  mock_httr_get <- mockery::mock(fake_response)

  # Mock httr::content to return some raw binary content
  fake_binary_data <- charToRaw("parquetdata")
  mock_httr_content <- mockery::mock(fake_binary_data)

  # Replace the functions with mocks
  mockery::stub(download_parquet_to_cache, "get_env_var", mock_get_env_var)
  mockery::stub(download_parquet_to_cache, "httr::GET", mock_httr_get)
  mockery::stub(download_parquet_to_cache, "httr::content", mock_httr_content)

  # Run
  download_parquet_to_cache("psms", s3_path, presigned_url)

  # Assert file was written
  expect_true(file.exists(expected_fp))

  # Read and confirm contents
  written_content <- readBin(expected_fp, what = "raw", n = 100000)
  expect_equal(written_content, fake_binary_data)
})
