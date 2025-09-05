test_that("experiment_names_to_objects returns parsed experiments", {
  # Dummy response to return - matches API format
  fake_response <- list(
    pageItems = list(
      list(name = "exp1", id = 123),
      list(name = "exp2", id = 456)
    )
  )

  # Create a mock GET function
  mock_get <- mockery::mock(
    structure(
      list(
        status_code = 200L,
        content = NULL
      ),
      class = "response"
    )
  )

  # Mock content() to return our fake_response
  mock_content <- mockery::mock(fake_response)

  # Mock other functions if needed
  mock_get_id_token <- mockery::mock("fake_token")
  mock_get_env_var <- mockery::mock("http://msaid-fake-endpoint.com")

  # Patch the functions using mockery::stub
  mockery::stub(experiment_names_to_objects, "httr::GET", mock_get)
  mockery::stub(experiment_names_to_objects, "httr::content", mock_content)
  mockery::stub(experiment_names_to_objects, "get_id_token", mock_get_id_token)
  mockery::stub(experiment_names_to_objects, "get_env_var", mock_get_env_var)

  result <- experiment_names_to_objects(c("exp1", "exp2"))

  # Now testing the pageItems directly
  expect_equal(result[[1]]$name, "exp1")
  expect_equal(result[[2]]$id, 456)
})
