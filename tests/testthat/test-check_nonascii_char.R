# check_nonascii_char() ----
#
# Note: This function is not designed to be used directly by user (low level
# function).
# We create a generic function 'checker_function()' that calls this function
# and we implement unit tests on this new function.

checker_function <- function(x) {
  check_nonascii_char(x)
  invisible(NULL)
}


test_that("check_nonascii_char() fails", {
  expect_error(
    checker_function(x = "ha大ah"),
    "The argument 'x' should not contain non ASCII characters",
    fixed = TRUE
  )

  expect_error(
    checker_function(x = "haअah"),
    "The argument 'x' should not contain non ASCII characters",
    fixed = TRUE
  )
})


test_that("check_nonascii_char() succeeds", {
  expect_invisible(
    checker_function("string")
  )

  x <- checker_function("string")
  expect_null(x)
})
