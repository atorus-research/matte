test_that("use_batch_jobs throws proper errors", {
  expect_error(use_batch_jobs("asdf"), "The folder")
})
