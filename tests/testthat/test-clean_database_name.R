## clean_database_name() ----

test_that("clean_database_name() succeeds", {
  x <- clean_database_name("Middolo 2023")

  expect_true(class(x) == "character")
  expect_equal(length(x), 1L)
  expect_true(x == "middolo_2023")

  x <- clean_database_name("  Middolo - 2023..")
  expect_true(x == "middolo_2023")
})
