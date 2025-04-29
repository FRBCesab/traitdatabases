## read_yaml_template() ----

test_that("read_yaml_template() succeeds", {
  metadata <- read_yaml_template()

  expect_true(is.list(metadata))

  expect_true("status" %in% names(metadata))
  expect_true("traits" %in% names(metadata))
  expect_true("dataset" %in% names(metadata))
  expect_true("taxonomy" %in% names(metadata$"dataset"))
  expect_true("binomial" %in% names(metadata$"dataset"$"taxonomy"))

  expect_true(metadata$"dataset"$"id" == ".dataset_id")
})
