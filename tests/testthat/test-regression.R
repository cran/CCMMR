# Regression tests pinning the numeric output of the main user-facing functions
# to known-good reference values. These guard against unintended changes to the
# results when the internals are modified. The reference values were generated
# with the bundled `two_half_moons` data set, which is fully deterministic
# (RANN::nn2 and the optimizer introduce no randomness).
tol <- 1e-5
tol_lambda <- 1e-4

get_X <- function() {
    data(two_half_moons, package = "CCMMR", envir = environment())
    as.matrix(two_half_moons)[, -3]
}

test_that("sparse_weights (SC) output is stable", {
    X <- get_X()
    W <- sparse_weights(X, 5, 8.0)

    expect_s3_class(W, "sparseweights")
    expect_equal(nrow(W$keys), 1204L)
    expect_equal(sum(W$values), 862.06142430432635, tolerance = tol)
    # With n = 150 the scaling denominator does not overflow, so all Gaussian
    # weights must stay within (0, 1].
    expect_true(all(W$values > 0 & W$values <= 1))
    expect_equal(head(W$values, 4),
                 c(8.8720292688782368e-05, 0.85733295288816891,
                   0.93169786827839174, 0.90642893685425141),
                 tolerance = tol)
})

test_that("sparse_weights (MST) output is stable", {
    X <- get_X()
    W <- sparse_weights(X, 5, 8.0, connection_type = "MST")

    expect_equal(nrow(W$keys), 914L)
    expect_equal(sum(W$values), 839.82276458875947, tolerance = tol)
    expect_true(all(W$values > 0 & W$values <= 1))
})

test_that("convex_clusterpath output is stable", {
    X <- get_X()
    W <- sparse_weights(X, 5, 8.0)
    res <- convex_clusterpath(X, W, seq(0, 50, 10))

    expect_s3_class(res, "cvxclust")
    expect_equal(res$info$clusters, c(150, 27, 15, 11, 9, 8))
    expect_equal(res$info$loss,
                 c(0, 0.046961942259711051, 0.075587177658330923,
                   0.098547234068701681, 0.11808814825028721,
                   0.13604511781387246),
                 tolerance = tol)
    expect_equal(res$eps_fusions, 0.0011715742205747801, tolerance = tol)
    expect_equal(nrow(res$merge), 142L)
    expect_equal(dim(res$coordinates), c(900L, 2L))
})

test_that("convex_clustering output is stable", {
    X <- get_X()
    W <- sparse_weights(X, 5, 8.0)
    res <- convex_clustering(X, W, target_low = 2, target_high = 5)

    expect_s3_class(res, "cvxclust")
    expect_equal(res$info$clusters, c(5, 4, 3, 2))
    expect_equal(res$info$loss,
                 c(0.43836641086492378, 0.44636998233975644,
                   0.45759817321919571, 0.49604066148526821),
                 tolerance = tol)
    expect_equal(res$lambdas,
                 c(365.62663322573178, 386.69676619810804,
                   421.63549877541982, 633.74427116468769),
                 tolerance = tol_lambda)
    # Two balanced clusters of 75 observations each.
    expect_equal(as.integer(table(clusters(res, 2))), c(75L, 75L))
})
