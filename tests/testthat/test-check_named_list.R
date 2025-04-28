## check_named_list() ----

# Note: This function is not designed to be used directly by user (low level
# function).
# We create a generic function 'checker_function()' that calls this function
# and we implement unit tests on this new function.

checker_function <- function(x) {
  check_named_list(x)
  invisible(NULL)
}


test_that("check_named_list() fails", {
  expect_error(
    checker_function(letters),
    "The argument 'x' must be a list",
    fixed = TRUE
  )

  expect_error(
    checker_function(list(1, "a")),
    "The argument 'x' must be a named list",
    fixed = TRUE
  )
})


test_that("check_named_list() succeeds", {
  liste <- list("toto" = 1, "titi" = letters)
  expect_invisible(
    checker_function(liste)
  )

  x <- checker_function(liste)
  expect_null(x)
})
