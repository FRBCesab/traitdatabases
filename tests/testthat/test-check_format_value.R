## check_format_value() ----

# Note: This function is not designed to be used directly by user (low level
# function).
# We create a generic function 'checker_function()' that calls this function
# and we implement unit tests on this new function.

checker_function <- function(x) {
  check_format_value(x)
  invisible(NULL)
}


test_that("check_format_value() fails", {
  expect_error(
    checker_function(x = "txt"),
    "The argument 'x' should be one of 'xlsx' and 'yaml'",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = "TXT"),
    "The argument 'x' should be one of 'xlsx' and 'yaml'",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = "rds"),
    "The argument 'x' should be one of 'xlsx' and 'yaml'",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = "xls"),
    "The argument 'x' should be one of 'xlsx' and 'yaml'",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = "XLSX"),
    "The argument 'x' should be one of 'xlsx' and 'yaml'",
    fixed = TRUE
  )
})

test_that("check_format_value() succeeds", {
  expect_invisible(
    checker_function("yaml")
  )

  x <- checker_function("xlsx")
  expect_null(x)
})
