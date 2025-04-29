## yaml_to_df() ----

test_that("yaml_to_df() fails", {
  metadata <- read_yaml_template()
  metadata <- metadata[-1]

  expect_error(
    yaml_to_df(metadata),
    "No key 'status' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata <- metadata[-2]

  expect_error(
    yaml_to_df(metadata),
    "No key 'dataset' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata <- metadata[-3]

  expect_error(
    yaml_to_df(metadata),
    "No key 'traits' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata[[3]][[1]] <- metadata[[3]][[1]][-1]

  expect_error(
    yaml_to_df(metadata),
    "No key 'variable' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata[[3]][[1]] <- metadata[[3]][[1]][-2]

  expect_error(
    yaml_to_df(metadata),
    "No key 'name' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata[[3]][[1]] <- metadata[[3]][[1]][-3]

  expect_error(
    yaml_to_df(metadata),
    "No key 'category' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata[[3]][[1]] <- metadata[[3]][[1]][-4]

  expect_error(
    yaml_to_df(metadata),
    "No key 'type' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata[[3]][[2]] <- metadata[[3]][[2]][-1]

  expect_error(
    yaml_to_df(metadata),
    "No key 'variable' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata[[3]][[2]] <- metadata[[3]][[2]][-2]

  expect_error(
    yaml_to_df(metadata),
    "No key 'name' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata[[3]][[2]] <- metadata[[3]][[2]][-3]

  expect_error(
    yaml_to_df(metadata),
    "No key 'category' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata[[3]][[2]] <- metadata[[3]][[2]][-4]

  expect_error(
    yaml_to_df(metadata),
    "No key 'type' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata[[3]][[2]]$"levels"[[1]] <- metadata[[3]][[2]]$"levels"[[1]][-1]

  expect_error(
    yaml_to_df(metadata),
    "No key 'value' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata[[3]][[2]]$"levels"[[1]] <- metadata[[3]][[2]]$"levels"[[1]][-2]

  expect_error(
    yaml_to_df(metadata),
    "No key 'description' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata[[3]][[2]]$"levels"[[2]] <- metadata[[3]][[2]]$"levels"[[2]][-1]

  expect_error(
    yaml_to_df(metadata),
    "No key 'value' found in the YAML file",
    fixed = TRUE
  )

  metadata <- read_yaml_template()
  metadata[[3]][[2]]$"levels"[[2]] <- metadata[[3]][[2]]$"levels"[[2]][-2]

  expect_error(
    yaml_to_df(metadata),
    "No key 'description' found in the YAML file",
    fixed = TRUE
  )
})


test_that("yaml_to_df() succeeds - Mixed traits", {
  metadata <- read_yaml_template()
  x <- yaml_to_df(metadata)

  expect_true(is.list(x))
  expect_true(!is.null(names(x)))
  expect_true("status" %in% names(x))
  expect_true("dataset" %in% names(x))
  expect_true("traits" %in% names(x))

  expect_true(is.data.frame(x[["status"]]))
  expect_true(ncol(x[["status"]]) == 1L)
  expect_true(nrow(x[["status"]]) == 1L)
  expect_true(x[["status"]][1, "status"] == "draft")

  expect_true(is.data.frame(x[["dataset"]]))
  expect_true(ncol(x[["dataset"]]) == 2L)
  expect_true(nrow(x[["dataset"]]) == 23L)
  expect_true(x[["dataset"]][20, "key"] == "taxonomy.genus")

  expect_true(is.data.frame(x[["traits"]]))
  expect_true(ncol(x[["traits"]]) == 7L)
  expect_true(nrow(x[["traits"]]) == 3L)
  expect_true(x[["traits"]][2, "variable"] == x[["traits"]][3, "variable"])
})


test_that("yaml_to_df() succeeds - One quantitative trait", {
  metadata <- read_yaml_template()
  metadata[["traits"]] <- metadata[["traits"]][-2]
  x <- yaml_to_df(metadata)

  expect_true(ncol(x[["traits"]]) == 7L)
  expect_true(nrow(x[["traits"]]) == 1L)
  expect_true("levels.value" %in% colnames(x[["traits"]]))
  expect_true("levels.description" %in% colnames(x[["traits"]]))
  expect_true(is.na(x[["traits"]][1, "levels.value"]))
  expect_true(is.na(x[["traits"]][1, "levels.description"]))
})


test_that("yaml_to_df() succeeds - One categorical trait", {
  metadata <- read_yaml_template()
  metadata[["traits"]] <- metadata[["traits"]][-1]
  metadata[["traits"]][[1]] <- metadata[["traits"]][[1]][-5]
  x <- yaml_to_df(metadata)

  expect_true(ncol(x[["traits"]]) == 7L)
  expect_true(nrow(x[["traits"]]) == 2L)
  expect_true("units" %in% colnames(x[["traits"]]))
  expect_true(is.na(x[["traits"]][1, "units"]))
  expect_true(is.na(x[["traits"]][2, "units"]))
})
