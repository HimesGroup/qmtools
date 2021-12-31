pp <- empty
nm <- prod(dim(pp))
poplin_data(pp, "d1") <- matrix(rnorm(nm), nrow = nrow(pp), ncol = ncol(pp))
poplin_data(pp, "d2") <- matrix(rnorm(nm), nrow = nrow(pp), ncol = ncol(pp))
poplin_reduced(pp, "r1") <- poplin.matrix(matrix(rnorm(nm), nrow = ncol(pp), ncol = 3))
poplin_reduced(pp, "r2") <- poplin.matrix(matrix(rnorm(nm), nrow = ncol(pp), ncol = 3))

test_that("rbind work correctly.", {
  pp.alt <- pp[sample(nrow(pp)), ]
  pp2 <- rbind(pp, pp.alt)
  expect_identical(poplin_raw(pp2), rbind(poplin_raw(pp), poplin_raw(pp.alt)))
  expect_identical(poplin_data(pp2), rbind(poplin_data(pp), poplin_data(pp.alt)))
  expect_identical(poplin_reduced(pp2), rbind(poplin_reduced(pp)))

  ## cannot combine when mismatched custom attributes are involved
  r2 <- poplin_reduced(pp.alt, "r2")
  attr(r2, "test") <- dim(pp.alt)
  poplin_reduced(pp.alt, "r2") <- r2
  expect_error(pp2 <- rbind(pp, pp.alt), "do not match")
})

test_that("cbind work correctly.", {
  pp.alt <- pp[, sample(ncol(pp))]
  pp2 <- cbind(pp, pp.alt)
  expect_identical(poplin_raw(pp2), cbind(poplin_raw(pp), poplin_raw(pp.alt)))
  expect_identical(poplin_data(pp2), cbind(poplin_data(pp), poplin_data(pp.alt)))
  expect_identical(poplin_reduced(pp2), rbind(poplin_reduced(pp), poplin_reduced(pp.alt)))

  ## raise warning when mismatched custom attributes are involved
  r2 <- poplin_reduced(pp.alt, "r2")
  attr(r2, "test") <- "a"
  poplin_reduced(pp.alt, "r2") <- r2
  expect_warning(pp2 <- cbind(pp, pp.alt))
})
