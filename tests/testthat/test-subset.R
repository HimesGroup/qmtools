pp <- faahko_poplin
nm <- prod(dim(pp))
poplin_data(pp, "d1") <- matrix(rnorm(nm), nrow = nrow(pp), ncol = ncol(pp))
poplin_data(pp, "d2") <- matrix(rnorm(nm), nrow = nrow(pp), ncol = ncol(pp))
poplin_reduced(pp, "r1") <- poplin.matrix(matrix(rnorm(nm), nrow = ncol(pp), ncol = 3))
poplin_reduced(pp, "r2") <- poplin.matrix(matrix(rnorm(nm), nrow = ncol(pp), ncol = 3))

test_that("subsetting by row works correctly.", {
  idx <- sample(nrow(pp), 5)
  pp_sub <- pp[idx, ]
  expect_identical(rowData(pp)[idx, ], rowData(pp_sub))
  expect_identical(poplin_raw(pp)[idx, ], poplin_raw(pp_sub))
  expect_identical(poplin_data(pp)[idx, ], poplin_data(pp_sub))
  expect_identical(poplin_data(pp, "d2")[idx, ], poplin_data(pp_sub, "d2"))

  ## remain unchanged
  expect_identical(colData(pp), colData(pp_sub))
  expect_identical(poplin_reduced(pp), poplin_reduced(pp_sub))
  expect_identical(poplin_reduced(pp, "r2"), poplin_reduced(pp_sub, "r2"))
})

test_that("subsetting by column works correctly.", {
  idx <- sample(ncol(pp), 5)
  pp_sub <- pp[, idx]
  expect_identical(colData(pp)[idx, ], colData(pp_sub))
  expect_identical(poplin_raw(pp)[, idx], poplin_raw(pp_sub))
  expect_identical(poplin_data(pp)[, idx], poplin_data(pp_sub))
  expect_identical(poplin_data(pp, 2)[, idx], poplin_data(pp_sub, 2))
  expect_identical(poplin_reduced(pp)[idx, ], poplin_reduced(pp_sub))
  expect_identical(poplin_reduced(pp, "r2")[idx, ], poplin_reduced(pp_sub, "r2"))

  ## remain unchanged
  expect_identical(rowData(pp), rowData(pp_sub))
})

test_that("subset replacement by row works correctly.", {
  to <- 1:3
  from <- 5:7
  pp2 <- pp
  pp2[to, ] <- pp[from, ]
  expect_identical(rownames(poplin_raw(pp2)), rownames(poplin_data(pp2)))
  expect_identical(rownames(pp2), rownames(poplin_data(pp2)))
  expect_identical(poplin_raw(pp)[from, ], poplin_raw(pp2)[to, ])
  expect_identical(poplin_data(pp)[from, ], poplin_data(pp2)[to, ])
  expect_identical(poplin_data(pp, 2)[from, ], poplin_data(pp2, 2)[to, ])
  expect_identical(poplin_raw(pp)[-to, ], poplin_raw(pp2)[-to, ],)
  expect_identical(poplin_data(pp)[-to, ], poplin_data(pp2)[-to, ])
  expect_identical(poplin_data(pp, 2)[-to, ], poplin_data(pp2, 2)[-to, ])

  ## remain unchanged
  expect_identical(colData(pp), colData(pp2))
  expect_identical(poplin_reduced(pp), poplin_reduced(pp2))
  expect_identical(poplin_reduced(pp, 2), poplin_reduced(pp2, 2))

})

test_that("subset replacement by column works correctly.", {
  to <- 1:3
  from <- 5:7
  pp2 <- pp
  pp2[, to] <- pp[, from]
  expect_identical(colnames(poplin_raw(pp2)), colnames(poplin_data(pp2)))
  expect_identical(colnames(pp2), colnames(poplin_data(pp2)))
  expect_identical(colnames(pp2), rownames(poplin_reduced(pp2)))
  expect_identical(poplin_raw(pp)[, from], poplin_raw(pp2)[, to])
  expect_identical(poplin_data(pp)[, from], poplin_data(pp2)[, to])
  expect_identical(poplin_data(pp, 2)[, from], poplin_data(pp2, 2)[, to])
  expect_identical(poplin_reduced(pp)[from, ], poplin_reduced(pp2)[to, ])
  expect_identical(poplin_reduced(pp, 2)[from, ], poplin_reduced(pp2, 2)[to, ])
  expect_identical(poplin_raw(pp)[, -to], poplin_raw(pp2)[, -to],)
  expect_identical(poplin_data(pp)[, -to], poplin_data(pp2)[, -to])
  expect_identical(poplin_data(pp, 2)[, -to], poplin_data(pp2, 2)[, -to])
  expect_identical(poplin_reduced(pp)[-to, ], poplin_reduced(pp2)[-to, ])
  expect_identical(poplin_reduced(pp, 2)[-to, ], poplin_reduced(pp2, 2)[-to, ])

  ## remain unchanged
  expect_identical(rowData(pp), rowData(pp2))
})

test_that("subset replacement by both row and column work correctly.", {
  ## whole replacement
  pp2 <- pp
  poplin_raw_list(pp2) <- list()
  pp2[] <- pp
  expect_identical(pp, pp2)

  ## partial replacement
  pp2 <- pp
  to <- 1:3
  from <- 5:7
  pp2[to, to] <- pp[from, from]
  expect_identical(rownames(poplin_raw(pp2)), rownames(poplin_data(pp2)))
  expect_identical(rownames(pp2), rownames(poplin_data(pp2)))
  expect_identical(colnames(poplin_raw(pp2)), colnames(poplin_data(pp2)))
  expect_identical(colnames(pp2), colnames(poplin_data(pp2)))
  expect_identical(colnames(pp2), rownames(poplin_reduced(pp2)))
  
  expect_identical(poplin_raw(pp)[from, from], poplin_raw(pp2)[to, to])
  expect_identical(poplin_data(pp)[from, from], poplin_data(pp2)[to, to])
  expect_identical(poplin_data(pp, 2)[from, from], poplin_data(pp2, 2)[to, to])
  expect_identical(poplin_reduced(pp)[from, ], poplin_reduced(pp2)[to, ])
  expect_identical(poplin_reduced(pp, 2)[from, ], poplin_reduced(pp2, 2)[to, ])
  expect_identical(poplin_raw(pp)[-to, -to], poplin_raw(pp2)[-to, -to],)
  expect_identical(poplin_data(pp)[-to, -to], poplin_data(pp2)[-to, -to])
  expect_identical(poplin_data(pp, 2)[-to, -to], poplin_data(pp2, 2)[-to, -to])
  expect_identical(poplin_reduced(pp)[-to, ], poplin_reduced(pp2)[-to, ])
  expect_identical(poplin_reduced(pp, 2)[-to, ], poplin_reduced(pp2, 2)[-to, ])
})
