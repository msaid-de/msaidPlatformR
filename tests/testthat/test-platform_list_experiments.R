test_that("platform_list_experiments returns data frame with all columns", {
  # Mock environment variables
  withr::with_envvar(c(
    "endpoint" = "https://api.test.msaid.io",
    "id_token" = "fake_token",
    "debug" = "FALSE"
  ), {
    # Mock the helper functions
    mockery::stub(platform_list_experiments, "get_id_token", "fake_token")
    mockery::stub(platform_list_experiments, "get_env_var", function(name) {
      switch(name,
        "endpoint" = "https://api.test.msaid.io",
        "debug" = FALSE,
        "region" = "eu-central-1",
        "stage" = "prod"
      )
    })
    mockery::stub(platform_list_experiments, "create_filter_experiment_list", "name,includes,test")
    
    # Create mock response data
    mock_response_content <- '{
      "totalItemCount": 2,
      "pageItems": [
        {
          "uuid": "123e4567-e89b-12d3-a456-426614174000",
          "name": "test_experiment_1",
          "description": "Test experiment 1",
          "username": "user1@example.com",
          "tags": ["phospho", "human"],
          "status": "completed",
          "createdAt": "2024-01-01T10:00:00Z"
        },
        {
          "uuid": "223e4567-e89b-12d3-a456-426614174001", 
          "name": "test_experiment_2",
          "description": "Test experiment 2",
          "username": "user2@example.com",
          "tags": ["label-free"],
          "status": "running",
          "createdAt": "2024-01-02T11:00:00Z"
        }
      ]
    }'
    
    # Mock HTTP response
    mock_response <- list(
      status_code = 200
    )
    class(mock_response) <- "response"
    
    mockery::stub(platform_list_experiments, "httr::GET", mock_response)
    mockery::stub(platform_list_experiments, "httr::http_error", FALSE)
    mockery::stub(platform_list_experiments, "httr::content", mock_response_content)
    
    # Call the function
    result <- platform_list_experiments(name_includes = "test")
    
    # Assertions
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
    expect_true("uuid" %in% names(result))
    expect_true("name" %in% names(result))
    expect_true("description" %in% names(result))
    expect_true("username" %in% names(result))
    expect_true("tags" %in% names(result))
    expect_true("status" %in% names(result))
    expect_true("createdAt" %in% names(result))
    
    expect_equal(result$name[1], "test_experiment_1")
    expect_equal(result$name[2], "test_experiment_2")
    expect_equal(result$uuid[1], "123e4567-e89b-12d3-a456-426614174000")
    expect_equal(result$status[1], "completed")
    expect_equal(result$status[2], "running")
  })
})

test_that("platform_list_experiments returns empty data frame when no experiments found", {
  withr::with_envvar(c(
    "endpoint" = "https://api.test.msaid.io",
    "id_token" = "fake_token",
    "debug" = "FALSE"
  ), {
    # Mock the helper functions
    mockery::stub(platform_list_experiments, "get_id_token", "fake_token")
    mockery::stub(platform_list_experiments, "get_env_var", function(name) {
      switch(name,
        "endpoint" = "https://api.test.msaid.io",
        "debug" = FALSE
      )
    })
    mockery::stub(platform_list_experiments, "create_filter_experiment_list", "name,includes,nonexistent")
    
    # Create empty response
    mock_response_content <- '{
      "totalItemCount": 0,
      "pageItems": []
    }'
    
    mock_response <- list(status_code = 200)
    class(mock_response) <- "response"
    
    mockery::stub(platform_list_experiments, "httr::GET", mock_response)
    mockery::stub(platform_list_experiments, "httr::http_error", FALSE)
    mockery::stub(platform_list_experiments, "httr::content", mock_response_content)
    
    # Call the function
    result <- platform_list_experiments(name_includes = "nonexistent")
    
    # Assertions
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 0)
    expect_equal(ncol(result), 0)
  })
})

test_that("platform_list_experiments handles experiments with different column structures", {
  withr::with_envvar(c(
    "endpoint" = "https://api.test.msaid.io",
    "id_token" = "fake_token",
    "debug" = "FALSE"
  ), {
    # Mock the helper functions
    mockery::stub(platform_list_experiments, "get_id_token", "fake_token")
    mockery::stub(platform_list_experiments, "get_env_var", function(name) {
      switch(name,
        "endpoint" = "https://api.test.msaid.io",
        "debug" = FALSE
      )
    })
    mockery::stub(platform_list_experiments, "create_filter_experiment_list", "")
    
    # Create response with experiments having different fields
    mock_response_content <- '{
      "totalItemCount": 2,
      "pageItems": [
        {
          "uuid": "123e4567-e89b-12d3-a456-426614174000",
          "name": "experiment_1",
          "status": "completed",
          "extra_field": "value1"
        },
        {
          "uuid": "223e4567-e89b-12d3-a456-426614174001",
          "name": "experiment_2",
          "status": "running",
          "different_field": "value2"
        }
      ]
    }'
    
    mock_response <- list(status_code = 200)
    class(mock_response) <- "response"
    
    mockery::stub(platform_list_experiments, "httr::GET", mock_response)
    mockery::stub(platform_list_experiments, "httr::http_error", FALSE)
    mockery::stub(platform_list_experiments, "httr::content", mock_response_content)
    
    # Call the function
    result <- platform_list_experiments()
    
    # Assertions - should handle different column structures gracefully
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
    expect_true("uuid" %in% names(result))
    expect_true("name" %in% names(result))
    expect_true("status" %in% names(result))
    expect_true("extra_field" %in% names(result))
    expect_true("different_field" %in% names(result))
    
    # Check that missing fields are filled appropriately
    expect_equal(result$name[1], "experiment_1")
    expect_equal(result$name[2], "experiment_2")
    expect_equal(result$extra_field[1], "value1")
    expect_equal(result$different_field[2], "value2")
  })
})

test_that("platform_list_experiments handles HTTP errors", {
  withr::with_envvar(c(
    "endpoint" = "https://api.test.msaid.io",
    "id_token" = "fake_token",
    "debug" = "FALSE"
  ), {
    # Mock the helper functions
    mockery::stub(platform_list_experiments, "get_id_token", "fake_token")
    mockery::stub(platform_list_experiments, "get_env_var", function(name) {
      switch(name,
        "endpoint" = "https://api.test.msaid.io",
        "debug" = FALSE
      )
    })
    mockery::stub(platform_list_experiments, "create_filter_experiment_list", "")
    
    # Mock HTTP error response
    mock_response <- list(status_code = 401)
    class(mock_response) <- "response"
    
    mockery::stub(platform_list_experiments, "httr::GET", mock_response)
    mockery::stub(platform_list_experiments, "httr::http_error", TRUE)
    
    # Call the function and expect an error
    expect_error(platform_list_experiments(), class = "simpleError")
  })
})

test_that("platform_list_experiments stops when too many experiments returned", {
  withr::with_envvar(c(
    "endpoint" = "https://api.test.msaid.io",
    "id_token" = "fake_token",
    "debug" = "FALSE"
  ), {
    # Mock the helper functions
    mockery::stub(platform_list_experiments, "get_id_token", "fake_token")
    mockery::stub(platform_list_experiments, "get_env_var", function(name) {
      switch(name,
        "endpoint" = "https://api.test.msaid.io",
        "debug" = FALSE
      )
    })
    mockery::stub(platform_list_experiments, "create_filter_experiment_list", "")
    
    # Create response with totalItemCount > pageSize (100)
    mock_response_content <- '{
      "totalItemCount": 150,
      "pageItems": []
    }'
    
    mock_response <- list(status_code = 200)
    class(mock_response) <- "response"
    
    mockery::stub(platform_list_experiments, "httr::GET", mock_response)
    mockery::stub(platform_list_experiments, "httr::http_error", FALSE)
    mockery::stub(platform_list_experiments, "httr::content", mock_response_content)
    
    # Call the function and expect an error
    expect_error(
      platform_list_experiments(),
      "Too many experiments. Please narrow your selection using filters.",
      fixed = TRUE
    )
  })
})

test_that("platform_list_experiments handles list columns correctly", {
  withr::with_envvar(c(
    "endpoint" = "https://api.test.msaid.io",
    "id_token" = "fake_token", 
    "debug" = "FALSE"
  ), {
    # Mock the helper functions
    mockery::stub(platform_list_experiments, "get_id_token", "fake_token")
    mockery::stub(platform_list_experiments, "get_env_var", function(name) {
      switch(name,
        "endpoint" = "https://api.test.msaid.io",
        "debug" = FALSE
      )
    })
    mockery::stub(platform_list_experiments, "create_filter_experiment_list", "")
    
    # Create response with list columns (tags)
    mock_response_content <- '{
      "totalItemCount": 1,
      "pageItems": [
        {
          "uuid": "123e4567-e89b-12d3-a456-426614174000",
          "name": "test_experiment",
          "tags": ["phospho", "human", "label-free"],
          "metadata": {
            "instrument": "orbitrap",
            "species": "human"
          }
        }
      ]
    }'
    
    mock_response <- list(status_code = 200)
    class(mock_response) <- "response"
    
    mockery::stub(platform_list_experiments, "httr::GET", mock_response)
    mockery::stub(platform_list_experiments, "httr::http_error", FALSE)
    mockery::stub(platform_list_experiments, "httr::content", mock_response_content)
    
    # Call the function
    result <- platform_list_experiments()
    
    # Assertions
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_true("tags" %in% names(result))
    expect_true("metadata" %in% names(result))
    
    # Check that list columns are preserved
    expect_type(result$tags, "list")
    expect_equal(length(result$tags[[1]]), 3)
    expect_equal(result$tags[[1]], c("phospho", "human", "label-free"))
  })
})