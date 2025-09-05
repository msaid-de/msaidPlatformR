test_that("check if it creates correct filters", {
  expect_equal(create_filter_experiment_list(name_includes="test1"), "name,includes,test1")
  expect_equal(create_filter_experiment_list(name="test1"), "name,==,test1")
  expect_equal(create_filter_experiment_list(username="user1"), "username,==,user1")
  expect_equal(create_filter_experiment_list(tags=c("test1","test2")), "tags,includesAll,test1;tags,includesAll,test2")
  expect_equal(create_filter_experiment_list(name="test",username="it@msaid.de",tags=c("test1","test2")), "name,==,test;username,==,it@msaid.de;tags,includesAll,test1;tags,includesAll,test2")
  expect_equal(create_filter_experiment_list(name="test",name_includes="test",username="it@msaid.de",tags=c("test1","test2")), "name,includes,test;name,==,test;username,==,it@msaid.de;tags,includesAll,test1;tags,includesAll,test2")
  expect_equal(create_filter_experiment_list(name="test",name_includes="test",username="it@msaid.de",tags=c("test1","test2","test3")), "name,includes,test;name,==,test;username,==,it@msaid.de;tags,includesAll,test1;tags,includesAll,test2;tags,includesAll,test3")
})

test_that("check time filtering", {
  expect_equal(create_filter_experiment_list(from="2024-01-01T00:00:00Z"), "createdAt,>=,2024-01-01T00:00:00Z")
  expect_equal(create_filter_experiment_list(to="2024-12-31T23:59:59Z"), "createdAt,<=,2024-12-31T23:59:59Z")
  expect_equal(create_filter_experiment_list(from="2024-01-01T00:00:00Z", to="2024-12-31T23:59:59Z"), "createdAt,>=,2024-01-01T00:00:00Z;createdAt,<=,2024-12-31T23:59:59Z")
  expect_equal(create_filter_experiment_list(name="test", from="2024-01-01T00:00:00Z", to="2024-12-31T23:59:59Z"), "name,==,test;createdAt,>=,2024-01-01T00:00:00Z;createdAt,<=,2024-12-31T23:59:59Z")
})

test_that("check POSIXct time filtering", {
  from_time <- as.POSIXct("2024-01-01 10:30:00", tz = "UTC")
  to_time <- as.POSIXct("2024-12-31 15:45:30", tz = "UTC")
  
  expect_equal(create_filter_experiment_list(from=from_time), "createdAt,>=,2024-01-01T10:30:00Z")
  expect_equal(create_filter_experiment_list(to=to_time), "createdAt,<=,2024-12-31T15:45:30Z")
  expect_equal(create_filter_experiment_list(from=from_time, to=to_time), "createdAt,>=,2024-01-01T10:30:00Z;createdAt,<=,2024-12-31T15:45:30Z")
})

test_that("timestamp validation - date only formats", {
  # Date only should work with warning
  expect_warning(
    result <- create_filter_experiment_list(from="2024-01-01"),
    "Date-only timestamp.*Assuming 00:00:00 UTC"
  )
  expect_equal(result, "createdAt,>=,2024-01-01T00:00:00Z")
  
  # Date with slashes should work with warning
  expect_warning(
    result <- create_filter_experiment_list(from="2024/01/01"),
    "Date.*with '/' separators.*Assuming 00:00:00 UTC"
  )
  expect_equal(result, "createdAt,>=,2024-01-01T00:00:00Z")
})

test_that("timestamp validation - incomplete time formats", {
  # ISO format without timezone should warn
  expect_warning(
    result <- create_filter_experiment_list(from="2024-01-01T10:30:00"),
    "has no timezone information.*Assuming UTC"
  )
  expect_equal(result, "createdAt,>=,2024-01-01T10:30:00Z")
  
  # Space separator should warn and work
  expect_warning(
    result <- create_filter_experiment_list(from="2024-01-01 10:30:00"),
    "has no timezone information.*Assuming UTC"
  )
  expect_equal(result, "createdAt,>=,2024-01-01T10:30:00Z")
})

test_that("timestamp validation - timezone handling", {
  # UTC timezone should work without warning
  expect_silent(result <- create_filter_experiment_list(from="2024-01-01T10:30:00Z"))
  expect_equal(result, "createdAt,>=,2024-01-01T10:30:00Z")
  
  # Timezone offset should work and convert to UTC
  expect_silent(result <- create_filter_experiment_list(from="2024-01-01T10:30:00+02:00"))
  expect_equal(result, "createdAt,>=,2024-01-01T08:30:00Z")
  
  # Timezone offset without colon should work
  expect_silent(result <- create_filter_experiment_list(from="2024-01-01T10:30:00+0200"))
  expect_equal(result, "createdAt,>=,2024-01-01T08:30:00Z")
})

test_that("timestamp validation - error cases", {
  # Empty string should error
  expect_error(
    create_filter_experiment_list(from=""),
    "Parameter 'from': Empty string is not a valid timestamp"
  )
  
  # Invalid format should error
  expect_error(
    create_filter_experiment_list(from="invalid-date"),
    "Parameter 'from': Unrecognized timestamp format"
  )
  
  # Wrong type should error  
  expect_error(
    create_filter_experiment_list(from=123),
    "Parameter 'from': Must be a POSIXct object or character string"
  )
  
  # Invalid date values should error
  expect_error(
    create_filter_experiment_list(from="2024-13-01"),
    "Parameter 'from': Invalid date.*2024-13-01.*Please check the year, month, and day values"
  )
})

test_that("POSIXct without timezone should warn", {
  # Create POSIXct without timezone (should warn)
  time_no_tz <- as.POSIXct("2024-01-01 10:30:00")
  attr(time_no_tz, "tzone") <- NULL
  
  expect_warning(
    result <- create_filter_experiment_list(from=time_no_tz),
    "POSIXct object has no timezone.*Assuming UTC"
  )
  expect_equal(result, "createdAt,>=,2024-01-01T10:30:00Z")
})
