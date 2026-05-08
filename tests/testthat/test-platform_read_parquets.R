# Helper to create a test parquet dataset with hive-style partitioning
create_test_parquet_dataset <- function(temp_dir, n_rows = 10) {
  # Create a data frame with typical columns including partition columns
  df <- data.frame(
    PRECURSOR_ID = paste0("PREC_", seq_len(n_rows)),
    SEQUENCE = paste0("SEQ_", seq_len(n_rows)),
    RAW_FILE_NAME = paste0("raw_", seq_len(n_rows)),
    Q_VALUE = runif(n_rows, 0, 0.05),
    SE_SCORE = runif(n_rows, 0, 1),
    GLOBAL_Q_VALUE = runif(n_rows, 0, 0.05),
    DECOY = rep(FALSE, n_rows),
    QUANTIFICATION = runif(n_rows, 100, 10000),
    stringsAsFactors = FALSE
  )

  # Create hive-style partition path
  partition_path <- file.path(temp_dir, "organization_uuid=org1", "account_uuid=acc1", "experiment_uuid=exp1")
  dir.create(partition_path, recursive = TRUE)

  # Write parquet file
  arrow::write_parquet(arrow::arrow_table(df), file.path(partition_path, "data.parquet"))

  return(temp_dir)
}

test_that("platform_read_parquets returns all columns when include_columns is NULL", {
  temp_dir <- withr::local_tempdir()
  create_test_parquet_dataset(temp_dir)

  result <- platform_read_parquets(temp_dir, level = "precursors")

  col_names <- names(result)
  expect_true("PRECURSOR_ID" %in% col_names)
  expect_true("SEQUENCE" %in% col_names)
  expect_true("RAW_FILE_NAME" %in% col_names)
  expect_true("Q_VALUE" %in% col_names)
  expect_true("experiment_uuid" %in% col_names)
  # Internal partition columns should be removed
  expect_false("organization_uuid" %in% col_names)
  expect_false("account_uuid" %in% col_names)
  # local_file_path should be added
  expect_true("local_file_path" %in% col_names)
})

test_that("platform_read_parquets with include_columns keeps experiment_uuid", {
  temp_dir <- withr::local_tempdir()
  create_test_parquet_dataset(temp_dir)

  result <- platform_read_parquets(
    temp_dir,
    level = "precursors",
    include_columns = c("RAW_FILE_NAME", "PRECURSOR_ID", "Q_VALUE", "SE_SCORE")
  )

  col_names <- names(result)
  # User-requested columns should be present
  expect_true("RAW_FILE_NAME" %in% col_names)
  expect_true("PRECURSOR_ID" %in% col_names)
  expect_true("Q_VALUE" %in% col_names)
  expect_true("SE_SCORE" %in% col_names)
  # experiment_uuid must be preserved for downstream filtering
  expect_true("experiment_uuid" %in% col_names)
  # Non-requested data columns should be excluded
  expect_false("SEQUENCE" %in% col_names)
  expect_false("QUANTIFICATION" %in% col_names)
  # Internal partition columns should be removed
  expect_false("organization_uuid" %in% col_names)
  expect_false("account_uuid" %in% col_names)
})

test_that("platform_read_parquets with include_columns does not crash on filter", {
  temp_dir <- withr::local_tempdir()
  create_test_parquet_dataset(temp_dir)

  # This is the exact scenario from issue #1698 that caused a hard crash
  result <- platform_read_parquets(
    temp_dir,
    level = "precursors",
    include_columns = c("RAW_FILE_NAME", "PRECURSOR_ID", "Q_VALUE", "SE_SCORE")
  )

  # The caller needs to filter by experiment_uuid - this must not crash
  filtered <- result %>% dplyr::filter(experiment_uuid %in% c("exp1"))
  expect_true(nrow(filtered) > 0)
})

test_that("platform_read_parquets respects exclude_columns", {
  temp_dir <- withr::local_tempdir()
  create_test_parquet_dataset(temp_dir)

  result <- platform_read_parquets(
    temp_dir,
    level = "precursors",
    exclude_columns = c("QUANTIFICATION", "SE_SCORE")
  )

  col_names <- names(result)
  expect_false("QUANTIFICATION" %in% col_names)
  expect_false("SE_SCORE" %in% col_names)
  expect_true("PRECURSOR_ID" %in% col_names)
  expect_true("Q_VALUE" %in% col_names)
})

test_that("platform_read_parquets include_columns takes priority over exclude_columns", {
  temp_dir <- withr::local_tempdir()
  create_test_parquet_dataset(temp_dir)

  # If a column is in both include and exclude, include wins
  result <- platform_read_parquets(
    temp_dir,
    level = "precursors",
    include_columns = c("RAW_FILE_NAME", "Q_VALUE"),
    exclude_columns = c("Q_VALUE")
  )

  col_names <- names(result)
  expect_true("Q_VALUE" %in% col_names)
  expect_true("RAW_FILE_NAME" %in% col_names)
})

test_that("platform_read_parquets filters by max_q_value", {
  temp_dir <- withr::local_tempdir()

  # Create data with known Q_VALUE distribution
  df <- data.frame(
    PRECURSOR_ID = paste0("PREC_", 1:10),
    SEQUENCE = paste0("SEQ_", 1:10),
    RAW_FILE_NAME = paste0("raw_", 1:10),
    Q_VALUE = c(0.001, 0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.5, 1.0),
    SE_SCORE = runif(10),
    GLOBAL_Q_VALUE = rep(0.001, 10),
    DECOY = rep(FALSE, 10),
    QUANTIFICATION = runif(10, 100, 10000),
    stringsAsFactors = FALSE
  )

  partition_path <- file.path(temp_dir, "organization_uuid=org1", "account_uuid=acc1", "experiment_uuid=exp1")
  dir.create(partition_path, recursive = TRUE)
  arrow::write_parquet(arrow::arrow_table(df), file.path(partition_path, "data.parquet"))

  result <- platform_read_parquets(temp_dir, level = "precursors", max_q_value = 0.01)

  # Should only include rows with Q_VALUE <= 0.01
  result_df <- as.data.frame(result)
  expect_true(all(result_df$Q_VALUE <= 0.01))
  expect_equal(nrow(result_df), 3)
})

test_that("platform_read_parquets filters decoys", {
  temp_dir <- withr::local_tempdir()

  df <- data.frame(
    PRECURSOR_ID = paste0("PREC_", 1:6),
    SEQUENCE = paste0("SEQ_", 1:6),
    RAW_FILE_NAME = paste0("raw_", 1:6),
    Q_VALUE = rep(0.001, 6),
    SE_SCORE = runif(6),
    GLOBAL_Q_VALUE = rep(0.001, 6),
    DECOY = c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE),
    QUANTIFICATION = runif(6, 100, 10000),
    stringsAsFactors = FALSE
  )

  partition_path <- file.path(temp_dir, "organization_uuid=org1", "account_uuid=acc1", "experiment_uuid=exp1")
  dir.create(partition_path, recursive = TRUE)
  arrow::write_parquet(arrow::arrow_table(df), file.path(partition_path, "data.parquet"))

  # Exclude decoys (default)
  result <- platform_read_parquets(temp_dir, level = "precursors")
  expect_equal(nrow(result), 4)

  # Include decoys
  result_with_decoys <- platform_read_parquets(temp_dir, level = "precursors", include_decoys = TRUE)
  expect_equal(nrow(result_with_decoys), 6)
})

test_that("platform_read_parquets handles empty dataset", {
  temp_dir <- withr::local_tempdir()

  df <- data.frame(
    PRECURSOR_ID = character(0),
    Q_VALUE = numeric(0),
    GLOBAL_Q_VALUE = numeric(0),
    DECOY = logical(0),
    stringsAsFactors = FALSE
  )

  partition_path <- file.path(temp_dir, "organization_uuid=org1", "account_uuid=acc1", "experiment_uuid=exp1")
  dir.create(partition_path, recursive = TRUE)
  arrow::write_parquet(arrow::arrow_table(df), file.path(partition_path, "data.parquet"))

  result <- platform_read_parquets(temp_dir, level = "precursors")
  expect_equal(nrow(result), 0)
})

test_that("platform_read_parquets exclude_array_columns removes list columns", {
  temp_dir <- withr::local_tempdir()

  # Create schema with a list column
  schema <- arrow::schema(
    PRECURSOR_ID = arrow::utf8(),
    Q_VALUE = arrow::float64(),
    GLOBAL_Q_VALUE = arrow::float64(),
    DECOY = arrow::boolean(),
    PROTEIN_IDS = arrow::list_of(arrow::utf8())
  )

  tbl <- arrow::arrow_table(
    PRECURSOR_ID = c("PREC_1", "PREC_2"),
    Q_VALUE = c(0.001, 0.002),
    GLOBAL_Q_VALUE = c(0.001, 0.002),
    DECOY = c(FALSE, FALSE),
    PROTEIN_IDS = arrow::Array$create(list(c("P1", "P2"), c("P3")), type = arrow::list_of(arrow::utf8()))
  )

  partition_path <- file.path(temp_dir, "organization_uuid=org1", "account_uuid=acc1", "experiment_uuid=exp1")
  dir.create(partition_path, recursive = TRUE)
  arrow::write_parquet(tbl, file.path(partition_path, "data.parquet"))

  # With exclude_array_columns = TRUE
  result <- platform_read_parquets(temp_dir, level = "precursors", exclude_array_columns = TRUE)
  col_names <- names(result)
  expect_false("PROTEIN_IDS" %in% col_names)
  expect_true("PRECURSOR_ID" %in% col_names)

  # With exclude_array_columns = FALSE (default)
  result2 <- platform_read_parquets(temp_dir, level = "precursors", exclude_array_columns = FALSE)
  col_names2 <- names(result2)
  expect_true("PROTEIN_IDS" %in% col_names2)
})
