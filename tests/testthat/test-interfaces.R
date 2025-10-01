test_that("distanceMetric works with built-in methods", {
  x <- matrix(rnorm(100), ncol = 2)

  # Test euclidean
  d1 <- distanceMetric(x, "euclidean")
  expect_s3_class(d1, "dist")

  # Test manhattan
  d2 <- distanceMetric(x, "manhattan")
  expect_s3_class(d2, "dist")

  # Test maximum
  d3 <- distanceMetric(x, "maximum")
  expect_s3_class(d3, "dist")
})

test_that("distanceMetric works with custom methods", {
  x <- matrix(rnorm(50), ncol = 2)

  # Custom distance function
  custom_dist <- function(x) {
    as.matrix(dist(x, method = "manhattan"))
  }

  result <- distanceMetric(x, method = custom_dist)
  expect_true(is.matrix(result))
  expect_equal(nrow(result), nrow(x))
})

test_that("distanceMetric error handling", {
  x <- matrix(rnorm(50), ncol = 2)

  # Invalid method string
  expect_error(distanceMetric(x, "invalid_method"))

  # Non-function custom method
  expect_error(distanceMetric(x, method = "not_a_function"))

  # Custom function returning invalid type
  bad_custom <- function(x) "invalid_return"
  expect_error(distanceMetric(x, method = bad_custom))
})

test_that("stateSpaceMethod works with default method", {
  x <- rnorm(100)

  result <- stateSpaceMethod(x, E = 3, tau = 2)

  expect_type(result, "list")
  expect_true("matrix" %in% names(result))
  expect_true(is.matrix(result$matrix))
  expect_equal(ncol(result$matrix), 3)  # E = 3
})

test_that("stateSpaceMethod works with custom method", {
  x <- rnorm(50)

  custom_space <- function(x, E, tau) {
    n <- length(x) - (E-1)*tau
    mat <- matrix(NA, nrow = n, ncol = E)
    for(i in 1:E) {
      mat[,i] <- x[1:n + (i-1)*tau]
    }
    list(matrix = mat)
  }

  result <- stateSpaceMethod(x, E = 3, tau = 2, method = custom_space)

  expect_type(result, "list")
  expect_true("matrix" %in% names(result))
  expect_true(is.matrix(result$matrix))
  expect_equal(ncol(result$matrix), 3)
})

test_that("stateSpaceMethod error handling", {
  x <- rnorm(50)

  # Invalid E (must be > 1)
  expect_error(stateSpaceMethod(x, E = 1, tau = 2))
  expect_error(stateSpaceMethod(x, E = 0, tau = 2))
  expect_error(stateSpaceMethod(x, E = -1, tau = 2))

  # Invalid tau (must be > 0)
  expect_error(stateSpaceMethod(x, E = 3, tau = 0))
  expect_error(stateSpaceMethod(x, E = 3, tau = -1))

  # Non-function custom method
  expect_error(stateSpaceMethod(x, E = 3, tau = 2, method = "not_a_function"))

  # Custom function returning invalid structure
  bad_custom <- function(x, E, tau) list(invalid = "structure")
  expect_error(stateSpaceMethod(x, E = 3, tau = 2, method = bad_custom))

  # Custom function returning non-list
  bad_custom2 <- function(x, E, tau) "not_a_list"
  expect_error(stateSpaceMethod(x, E = 3, tau = 2, method = bad_custom2))
})
