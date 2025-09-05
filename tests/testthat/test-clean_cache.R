test_that("clean_cache deletes entire cache when delete_all is TRUE", {
  temp_cache_dir <- withr::local_tempdir()
  dummy_file <- file.path(temp_cache_dir, "dummy.parquet")
  file.create(dummy_file)

  # Sanity check
  expect_true(file.exists(dummy_file))

  mock_get_env_var <- mockery::mock(temp_cache_dir)
  mockery::stub(clean_cache, "get_env_var", mock_get_env_var)

  clean_cache(delete_all = TRUE)

  expect_false(dir.exists(temp_cache_dir))
})

test_that("clean_cache deletes correct experiment file when delete_all is FALSE", {
  temp_cache_dir <- withr::local_tempdir()
  psms_dir <- file.path(temp_cache_dir, "psms")
  dir.create(psms_dir, recursive = TRUE)

  # Create a test parquet file in the expected Hive structure
  test_parquet_path <- file.path(psms_dir, "result-db/v1/psms.parquet/experiment_uuid=exp123/test.parquet")
  dir.create(dirname(test_parquet_path), recursive = TRUE)
  file.create(test_parquet_path)

  expect_true(file.exists(test_parquet_path))

  mock_experiment <- list(
    list(
      uuid = "exp123",
      organizationUuid = "org123",
      accountUuid = "acc123"
    )
  )

  # Mock functions
  mock_get_env_var <- mockery::mock(temp_cache_dir)
  mock_experiment_names_to_objects <- mockery::mock(mock_experiment)
  
  # Mock platform_read_parquets to return a dataset with the file path
  mock_dataset <- data.frame(
    experiment_uuid = "exp123",
    local_file_path = test_parquet_path,
    stringsAsFactors = FALSE
  )
  mock_platform_read_parquets <- mockery::mock(mock_dataset)

  mockery::stub(clean_cache, "get_env_var", mock_get_env_var)
  mockery::stub(clean_cache, "experiment_names_to_objects", mock_experiment_names_to_objects)
  mockery::stub(clean_cache, "platform_read_parquets", mock_platform_read_parquets)

  clean_cache(experiment_names = c("some_exp"), level = "psms", delete_all = FALSE)

  expect_false(file.exists(test_parquet_path))
})

test_that("clean_cache does nothing when experiment_names is empty", {
  temp_cache_dir <- withr::local_tempdir()
  psms_dir <- file.path(temp_cache_dir, "psms")
  dir.create(psms_dir, recursive = TRUE)

  # Create a test parquet file
  test_parquet_path <- file.path(psms_dir, "result-db/v1/psms.parquet/experiment_uuid=exp123/test.parquet")
  dir.create(dirname(test_parquet_path), recursive = TRUE)
  file.create(test_parquet_path)

  expect_true(file.exists(test_parquet_path))

  mock_get_env_var <- mockery::mock(temp_cache_dir, temp_cache_dir, cycle = TRUE)
  mockery::stub(clean_cache, "get_env_var", mock_get_env_var)

  # Test with NULL experiment_names
  clean_cache(experiment_names = NULL, level = "psms", delete_all = FALSE)
  expect_true(file.exists(test_parquet_path))

  # Test with empty character vector
  clean_cache(experiment_names = character(0), level = "psms", delete_all = FALSE)
  expect_true(file.exists(test_parquet_path))
})
