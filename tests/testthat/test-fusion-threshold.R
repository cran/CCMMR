test_that("fusion threshold matches median of distances (even count)", {
    # n = 4 observations -> 6 pairwise distances (even), exercising the
    # average-of-two-middle-values branch of the C++ median.
    X <- matrix(c(0, 0,
                  1, 0,
                  0, 3,
                  4, 4), nrow = 4, byrow = TRUE)

    got  <- CCMMR:::.fusion_threshold(t(X), 1.0)
    want <- median(dist(X))

    expect_equal(got, want, tolerance = 1e-8)
})

test_that("fusion threshold matches median of distances (odd count)", {
    # n = 3 observations -> 3 pairwise distances (odd), exercising the
    # single-middle-value branch of the C++ median.
    X <- matrix(c(0, 0,
                  2, 0,
                  0, 5), nrow = 3, byrow = TRUE)

    got  <- CCMMR:::.fusion_threshold(t(X), 0.5)
    want <- 0.5 * median(dist(X))

    expect_equal(got, want, tolerance = 1e-8)
})
