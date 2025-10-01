test_that("print.pc_nature works correctly", {
  # Create a pc_nature object
  nature <- structure(
    list(
      no_causality = c(rep(1, 100), rep(0, 200)),
      positive = c(rep(0, 200), runif(100)),
      negative = c(rep(0, 250), runif(50)),
      dark = c(rep(0, 150), runif(150))
    ),
    class = "pc_nature"
  )

  # Test print output
  expect_output(print(nature), "Pattern Causality Nature Analysis")
  expect_output(print(nature), "No Causality: 100")
  expect_output(print(nature), "Positive Causality: 100")
  expect_output(print(nature), "Negative Causality: 50")
  expect_output(print(nature), "Dark Causality: 150")
})

test_that("print.pc_params works correctly", {
  accuracy_summary <- data.frame(
    E = c(2, 3),
    tau = c(1, 2),
    Total = c(0.5, 0.6),
    Positive = c(0.3, 0.4),
    Negative = c(0.1, 0.1),
    Dark = c(0.1, 0.1)
  )
  computation_time <- as.difftime(5, units = "secs")
  parameters <- list(Emax = 5, tauMax = 3, metric = "euclidean")

  pc_result <- pc_params(accuracy_summary, computation_time, parameters)

  # Test print output
  expect_output(print(pc_result), "Pattern Causality Parameter Optimization Results")
  expect_output(print(pc_result), "Parameters tested:")
})

test_that("summary.pc_params works correctly", {
  accuracy_summary <- data.frame(
    E = c(2, 3),
    tau = c(1, 2),
    Total = c(0.5, 0.6),
    Positive = c(0.3, 0.4),
    Negative = c(0.1, 0.1),
    Dark = c(0.1, 0.1)
  )
  computation_time <- as.difftime(5, units = "secs")
  parameters <- list(Emax = 5, tauMax = 3, metric = "euclidean")

  pc_result <- pc_params(accuracy_summary, computation_time, parameters)
  summary_result <- summary(pc_result)

  expect_s3_class(summary_result, "summary.pc_params")
  expect_output(print(summary_result), "Pattern Causality Parameter Optimization Summary")
})

test_that("pcLightweight print methods work", {
  data("climate_indices", package = "patterncausality")
  X <- climate_indices$AO[1:50]
  Y <- climate_indices$AAO[1:50]

  result <- pcLightweight(X, Y, E = 2, tau = 1, metric = "euclidean", h = 1, weighted = TRUE)

  # Test print method
  expect_output(print(result), "Pattern Causality Analysis")

  # Test summary method (note: currently returns pc_fit, not summary.pc_fit)
  summary_result <- summary(result)
  expect_s3_class(summary_result, "pc_fit")
})

test_that("print methods handle edge cases", {
  # Test pc_nature with NA values
  nature_with_na <- structure(
    list(
      no_causality = c(rep(NA, 10), rep(1, 50), rep(0, 40)),
      positive = c(rep(NA, 10), rep(0, 70), runif(20)),
      negative = c(rep(NA, 10), rep(0, 80), runif(10)),
      dark = c(rep(NA, 10), rep(0, 60), runif(30))
    ),
    class = "pc_nature"
  )

  expect_output(print(nature_with_na), "Pattern Causality Nature Analysis")
  expect_output(print(nature_with_na), "No Causality: 50")  # Should handle NA values correctly
})
