## td_create_metadata_file() ----

test_that("td_create_metadata_file() fails - Argument 'name'", {
  expect_error(
    td_create_metadata_file(),
    "Argument 'name' is required",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = NULL),
    "Argument 'name' is required",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = 1L),
    "Argument 'name' must be a character",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = letters[1:5]),
    "Argument 'name' must be of length 1",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = NA_character_),
    "Argument 'name' cannot be NA",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "ha大ah"),
    "The argument 'name' should not contain non ASCII characters",
    fixed = TRUE
  )
})


test_that("td_create_metadata_file() fails - Argument 'path'", {
  expect_error(
    td_create_metadata_file(name = "toto", path = NULL),
    "Argument 'path' is required",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", path = 1L),
    "Argument 'path' must be a character",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", path = letters[1:5]),
    "Argument 'path' must be of length 1",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", path = NA_character_),
    "Argument 'path' cannot be NA",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", path = file.path("missing_dir")),
    "The path 'missing_dir' does not exist",
    fixed = TRUE
  )
})


test_that("td_create_metadata_file() fails - Argument 'format'", {
  expect_error(
    td_create_metadata_file(name = "toto", format = NULL),
    "Argument 'format' is required",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", format = 1L),
    "Argument 'format' must be a character",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", format = letters[1:5]),
    "Argument 'format' must be of length 1",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", format = NA_character_),
    "Argument 'format' cannot be NA",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", format = "txt"),
    "The argument 'format' should be one of 'xlsx' and 'yaml'",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", format = "YAML"),
    "The argument 'format' should be one of 'xlsx' and 'yaml'",
    fixed = TRUE
  )
})


test_that("td_create_metadata_file() fails - Argument 'overwrite'", {
  expect_error(
    td_create_metadata_file(name = "toto", overwrite = NULL),
    "Argument 'overwrite' is required",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", overwrite = "string"),
    "Argument 'overwrite' must be a logical",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", overwrite = 10L),
    "Argument 'overwrite' must be a logical",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", overwrite = c(TRUE, TRUE)),
    "Argument 'overwrite' must be of length 1",
    fixed = TRUE
  )

  expect_error(
    td_create_metadata_file(name = "toto", overwrite = NA),
    "Argument 'overwrite' cannot be NA",
    fixed = TRUE
  )
})


test_that("td_create_metadata_file() succeeds - No file exists (yaml)", {
  name <- "MiDDolo 2023."
  slug <- "middolo_2023"

  path <- create_tempdir()
  dir_path <- file.path(path, slug)
  file_path <- file.path(dir_path, paste0(slug, "_metadata.yml"))

  expect_invisible(
    td_create_metadata_file(name, path)
  )

  expect_true(dir.exists(dir_path))
  expect_true(file.exists(file_path))

  metadata <- yaml::read_yaml(file_path)

  expect_true(metadata[["dataset"]][["id"]] == slug)
})


test_that("td_create_metadata_file() succeeds - No file exists (xlsx)", {
  name <- "MiDDolo 2023."
  slug <- "middolo_2023"

  path <- create_tempdir()
  dir_path <- file.path(path, slug)
  file_path <- file.path(dir_path, paste0(slug, "_metadata.xlsx"))

  expect_invisible(
    td_create_metadata_file(name, path, format = "xlsx")
  )

  expect_true(dir.exists(dir_path))
  expect_true(file.exists(file_path))

  metadata <- readxl::read_xlsx(file_path, sheet = "dataset")

  expect_true(metadata[1, "value"] == slug)
})


test_that("td_create_metadata_file() succeeds - File exists (yaml)", {
  name <- "MiDDolo 2023."
  slug <- "middolo_2023"

  path <- create_tempdir()
  dir_path <- file.path(path, slug)
  file_path <- file.path(dir_path, paste0(slug, "_metadata.yml"))

  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
  file.create(file_path)

  expect_message(
    td_create_metadata_file(name, path)
  )

  expect_true(file.size(file_path) == 0L)
})


test_that("td_create_metadata_file() succeeds - File exists (xlsx)", {
  name <- "MiDDolo 2023."
  slug <- "middolo_2023"

  path <- create_tempdir()
  dir_path <- file.path(path, slug)
  file_path <- file.path(dir_path, paste0(slug, "_metadata.xlsx"))

  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
  file.create(file_path)

  expect_message(
    td_create_metadata_file(name, path, format = "xlsx")
  )

  expect_true(file.size(file_path) == 0L)
})


test_that("td_create_metadata_file() succeeds - Overwrite (yaml)", {
  name <- "MiDDolo 2023."
  slug <- "middolo_2023"

  path <- create_tempdir()
  dir_path <- file.path(path, slug)
  file_path <- file.path(dir_path, paste0(slug, "_metadata.yml"))

  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
  file.create(file_path)

  expect_silent(
    td_create_metadata_file(name, path, overwrite = TRUE)
  )

  expect_true(file.size(file_path) > 0L)

  metadata <- yaml::read_yaml(file_path)

  expect_true(metadata[["dataset"]][["id"]] == slug)
})


test_that("td_create_metadata_file() succeeds - Overwrite (xlsx)", {
  name <- "MiDDolo 2023."
  slug <- "middolo_2023"

  path <- create_tempdir()
  dir_path <- file.path(path, slug)
  file_path <- file.path(dir_path, paste0(slug, "_metadata.xlsx"))

  dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
  file.create(file_path)

  expect_silent(
    td_create_metadata_file(name, path, format = "xlsx", overwrite = TRUE)
  )

  expect_true(file.size(file_path) > 0L)

  metadata <- readxl::read_xlsx(file_path, sheet = "dataset")

  expect_true(metadata[1, "value"] == slug)
})
