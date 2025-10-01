test_that("pcMatrix works correctly", {
  # Load data within test environment
  data("climate_indices", package = "patterncausality")
  dataset <- climate_indices[, -1][1:100, ]  # Use smaller dataset for speed

  result <- pcMatrix(
    dataset,
    E = 3,
    tau = 2,
    metric = "euclidean",
    h = 1,
    weighted = TRUE,
    verbose = FALSE
  )

  # Check result structure
  expect_s3_class(result, "pc_matrix")
  expect_type(result, "list")

  # Check required components
  expected_components <- c("positive", "negative", "dark", "parameters")
  expect_true(all(expected_components %in% names(result)))

  # Check matrix dimensions
  expect_true(is.matrix(result$positive))
  expect_true(is.matrix(result$negative))
  expect_true(is.matrix(result$dark))

  # Check parameter storage
  expect_equal(result$parameters$E, 3)
  expect_equal(result$parameters$tau, 2)
  expect_equal(result$parameters$metric, "euclidean")
})

test_that("pcMatrix handles different parameters", {
  data("climate_indices", package = "patterncausality")
  dataset <- climate_indices[, -1][1:50, ]

  # Test different metrics
  expect_no_error(pcMatrix(dataset, E = 2, tau = 1, metric = "manhattan", h = 1, weighted = FALSE, verbose = FALSE))
  expect_no_error(pcMatrix(dataset, E = 2, tau = 1, metric = "maximum", h = 1, weighted = FALSE, verbose = FALSE))

  # Test different E values
  expect_no_error(pcMatrix(dataset, E = 2, tau = 1, metric = "euclidean", h = 1, weighted = TRUE, verbose = FALSE))
  expect_no_error(pcMatrix(dataset, E = 3, tau = 1, metric = "euclidean", h = 1, weighted = TRUE, verbose = FALSE))
})

test_that("pcMatrix error handling", {
  data("climate_indices", package = "patterncausality")
  dataset <- climate_indices[, -1][1:50, ]

  # Test invalid inputs
  expect_error(pcMatrix(NULL, E = 3, tau = 2, metric = "euclidean", h = 1, weighted = TRUE))
  expect_error(pcMatrix(dataset, E = 1, tau = 2, metric = "euclidean", h = 1, weighted = TRUE))  # E must be > 1
  expect_error(pcMatrix(dataset, E = 3, tau = 0, metric = "euclidean", h = 1, weighted = TRUE))  # tau must be > 0
  expect_error(pcMatrix(dataset, E = 3, tau = 2, metric = "invalid", h = 1, weighted = TRUE))
})
