## check_key_in_yaml() ----

# Note: This function is not designed to be used directly by user (low level
# function).
# We create a generic function 'checker_function()' that calls this function
# and we implement unit tests on this new function.

checker_function <- function(x, y) {
  check_key_in_yaml(x, y)
  invisible(NULL)
}

metadata <- list(
  "status" = "draft",
  "dataset" = list(
    "id" = ".dataset_id",
    "title" = ".dataset_title"
  )
)

test_that("check_key_in_yaml() fails", {
  expect_error(
    checker_function(letters),
    "The argument 'metadata' must be a list",
    fixed = TRUE
  )

  expect_error(
    checker_function(list(1, "a")),
    "The argument 'metadata' must be a named list",
    fixed = TRUE
  )

  expect_error(
    checker_function(metadata),
    "Argument 'key' is required",
    fixed = TRUE
  )

  expect_error(
    checker_function(metadata, y = NULL),
    "Argument 'key' is required",
    fixed = TRUE
  )

  expect_error(
    checker_function(metadata, y = 1L),
    "Argument 'key' must be a character",
    fixed = TRUE
  )

  expect_error(
    checker_function(metadata, y = letters[1:5]),
    "Argument 'key' must be of length 1",
    fixed = TRUE
  )

  expect_error(
    checker_function(metadata, y = NA_character_),
    "Argument 'key' cannot be NA",
    fixed = TRUE
  )

  expect_error(
    checker_function(metadata, y = "toto"),
    "No key 'toto' found in the YAML file",
    fixed = TRUE
  )
})


test_that("check_key_in_yaml() succeeds", {
  expect_invisible(
    checker_function(metadata, "status")
  )

  x <- checker_function(metadata, "dataset")
  expect_null(x)
})
