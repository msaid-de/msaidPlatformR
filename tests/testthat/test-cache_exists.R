test_that("cache_exists returns TRUE if file is present in fake cache", {
  # Create a temporary directory and file
  temp_cache_dir <- withr::local_tempdir()
  fake_file <- file.path(temp_cache_dir, "psms/data/file.parquet")
  dir.create(dirname(fake_file), recursive = TRUE)
  file.create(fake_file)

  # Mock get_env_var to return the temp directory as "cache_root"
  mock_get_env_var <- mockery::mock(temp_cache_dir)
  mockery::stub(cache_exists, "get_env_var", mock_get_env_var)

  result <- cache_exists("psms", "data/file.parquet")
  expect_true(result)
})
