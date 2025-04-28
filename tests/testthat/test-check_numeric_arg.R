## check_numeric_arg() ----

# Note: This function is not designed to be used directly by user (low level
# function).
# We create a generic function 'checker_function()' that calls this function
# and we implement unit tests on this new function.

checker_function <- function(x) {
  check_numeric_arg(x)
  invisible(NULL)
}


test_that("check_numeric_arg() fails", {
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
    "Argument 'x' must be a numeric",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = 1:5),
    "Argument 'x' must be of length 1",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = NA_real_),
    "Argument 'x' cannot be NA",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = NA_integer_),
    "Argument 'x' cannot be NA",
    fixed = TRUE
  )
})


test_that("check_numeric_arg() succeeds", {
  expect_invisible(
    checker_function(125L)
  )

  x <- checker_function(125L)
  expect_null(x)
})
