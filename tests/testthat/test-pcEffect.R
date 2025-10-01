test_that("pcEffect works correctly", {
  data("climate_indices", package = "patterncausality")
  dataset <- climate_indices[, -1][1:100, ]

  # First create a pcMatrix object
  pc_matrix_obj <- pcMatrix(
    dataset,
    E = 3,
    tau = 2,
    metric = "euclidean",
    h = 1,
    weighted = TRUE,
    verbose = FALSE
  )

  # Test pcEffect
  result <- pcEffect(pc_matrix_obj)

  # Check result structure
  expect_s3_class(result, "pc_effect")
  expect_type(result, "list")

  # Check required components
  expected_components <- c("positive", "negative", "dark")
  expect_true(all(expected_components %in% names(result)))

  # Check data types (pcEffect returns data.frames, not matrices)
  expect_true(is.data.frame(result$positive))
  expect_true(is.data.frame(result$negative))
  expect_true(is.data.frame(result$dark))

  # Verify data.frame structure
  expect_true(all(c("received", "exerted", "Diff") %in% colnames(result$positive)))
  expect_true(all(c("received", "exerted", "Diff") %in% colnames(result$negative)))
  expect_true(all(c("received", "exerted", "Diff") %in% colnames(result$dark)))
})

test_that("pcEffect print method works", {
  data("climate_indices", package = "patterncausality")
  dataset <- climate_indices[, -1][1:50, ]

  pc_matrix_obj <- pcMatrix(dataset, E = 2, tau = 1, metric = "euclidean", h = 1, weighted = TRUE, verbose = FALSE)
  result <- pcEffect(pc_matrix_obj)

  # Test print method
  expect_output(print(result), "Pattern Causality Effect Analysis")
})

test_that("pcEffect summary method works", {
  data("climate_indices", package = "patterncausality")
  dataset <- climate_indices[, -1][1:50, ]

  pc_matrix_obj <- pcMatrix(dataset, E = 2, tau = 1, metric = "euclidean", h = 1, weighted = TRUE, verbose = FALSE)
  result <- pcEffect(pc_matrix_obj)

  # Test summary method
  summary_result <- summary(result)
  expect_s3_class(summary_result, "summary.pc_effect")
})

test_that("pcEffect error handling", {
  # Test with invalid input
  expect_error(pcEffect(NULL))
  expect_error(pcEffect("not_a_matrix"))
  expect_error(pcEffect(list(invalid = "object")))
})
