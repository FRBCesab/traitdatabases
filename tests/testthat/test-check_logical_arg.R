## check_logical_arg() ----

# Note: This function is not designed to be used directly by user (low level
# function).
# We create a generic function 'checker_function()' that calls this function
# and we implement unit tests on this new function.

checker_function <- function(x) {
  check_logical_arg(x)
  invisible(NULL)
}


test_that("check_logical_arg() fails", {
  expect_error(
    checker_function(),
    "Argument 'x' is required",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = NULL),
    "Argument 'x' is required",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = "string"),
    "Argument 'x' must be a logical",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = 10L),
    "Argument 'x' must be a logical",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = c(TRUE, TRUE)),
    "Argument 'x' must be of length 1",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = NA),
    "Argument 'x' cannot be NA",
    fixed = TRUE
  )
})


test_that("check_logical_arg() succeeds", {
  expect_invisible(
    checker_function(TRUE)
  )

  x <- checker_function(TRUE)
  expect_null(x)
})
