## check_path_exists() ----

# Note: This function is not designed to be used directly by user (low level
# function).
# We create a generic function 'checker_function()' that calls this function
# and we implement unit tests on this new function.

checker_function <- function(x) {
  check_path_exists(x)
  invisible(NULL)
}


test_that("check_path_exists() fails", {
  expect_error(
    checker_function(x = file.path("missing_dir")),
    "The path 'missing_dir' does not exist",
    fixed = TRUE
  )
})


test_that("check_path_exists() succeeds", {
  path <- create_tempdir()
  dir.create(file.path(path, "good_dir"))
  expect_invisible(
    checker_function("good_dir")
  )

  x <- checker_function("good_dir")
  expect_null(x)
})
