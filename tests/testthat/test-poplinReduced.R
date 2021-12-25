nm <- prod(dim(faahko_poplin))
d1 <- matrix(rnorm(nm), nrow = ncol(faahko_poplin), 2)
rownames(d1) <- colnames(faahko_poplin)
d2 <- matrix(rnorm(nm), nrow = ncol(faahko_poplin), 2)
rownames(d2) <- colnames(faahko_poplin)

test_that("poplinReducedData setters/getters work with character 'type'.", {
  pp <- faahko_poplin
  poplin_reduced(pp, "d1") <- d1
  expect_identical(poplin_reduced(pp, "d1"), d1)
  expect_identical(poplin_reduced_list(pp), SimpleList(d1 = d1))
  expect_identical(poplin_reduced_names(pp), "d1")

  poplin_reduced(pp, "d2") <- d2
  expect_identical(poplin_reduced(pp, "d2"), d2)
  expect_identical(poplin_reduced_list(pp), SimpleList(d1 = d1, d2 = d2))
  expect_identical(poplin_reduced_names(pp), c("d1", "d2"))

  ## Clearing data with NULL.
  poplin_reduced(pp, "d1") <- NULL
  expect_identical(poplin_reduced(pp, "d2"), d2)
  expect_identical(poplin_reduced_list(pp), SimpleList(d2 = d2))
  expect_identical(poplin_reduced_names(pp), "d2")

  ## Check different errors.
  d3 <- d1
  rownames(d3)[1] <- "xxx"
  expect_error(poplin_reduced(pp, "d1"), "invalid subscript")
  expect_error(poplin_reduced(pp, 2), "invalid subscript")
  expect_error(poplin_reduced(pp, "d1") <- d1[1:2, ], "number of rows")
  expect_error(poplin_reduced(pp, "d2") <- d1[1:2, ], "number of rows")
  expect_error(poplin_reduced(pp, "d3") <- d3, "rownames")
  expect_error(poplin_reduced(pp, 1) <- "hello", "number of rows")
})

test_that("poplinReducedData setters/getters work with numeric 'type'.", {
  pp <- faahko_poplin
  expect_error(poplin_reduced(pp), "no available entries")
  expect_error(poplin_reduced(pp, 2), "invalid subscript")
  expect_error(poplin_reduced(pp, "d1"), "invalid subscript")

  expect_error(poplin_reduced(pp, 1) <- d1, "out of bounds")
  expect_error(poplin_reduced(pp, 2) <- d2, "out of bounds")

  ## Warning: assigning an unnamed list.
  expect_warning(poplin_reduced_list(pp) <- list(d1, d2))
  expect_identical(poplin_reduced(pp), d1)
  expect_identical(poplin_reduced(pp, 2), d2)
  expect_identical(poplin_reduced_names(pp), c("reduced1", "reduced2"))
  d11 <- d1 * 10
  poplin_reduced(pp, "d11") <- d11
  expect_identical(poplin_reduced(pp, 1), d1)
  expect_identical(poplin_reduced(pp, 2), d2)
  expect_identical(poplin_reduced(pp, 3), d11)
  expect_identical(poplin_reduced_names(pp), c("reduced1", "reduced2", "d11"))

  ## Clearing data with NULL.
  poplin_reduced(pp, "reduced1") <- NULL
  expect_identical(poplin_reduced(pp), d2) # now d2 is the first entry
  expect_identical(poplin_reduced(pp, 1), d2)
  expect_identical(poplin_reduced(pp, 2), poplin_reduced(pp, "d11"))
  expect_identical(poplin_reduced_names(pp), c("reduced2", "d11"))

  poplin_reduced(pp) <- NULL # now d11 is the first entry
  expect_equal(poplin_reduced(pp), d11)
  expect_equal(poplin_reduced_names(pp), c("d11"))
  poplin_reduced(pp) <- d2 # overwrite
  expect_equal(poplin_reduced(pp, 1), d2)
  expect_equal(poplin_reduced_names(pp), "d11") # maintain the same label

  expect_error(poplin_reduced(pp, 5) <- d1, "out of bounds")
})


test_that("poplinData setters/getters work with List'.", {
  pp <- faahko_poplin
  poplin_reduced_list(pp) <- list(d1 = d1, d2 = d2)
  expect_identical(poplin_reduced_names(pp), c("d1", "d2"))
  expect_identical(poplin_reduced(pp, "d1"), d1)
  expect_identical(poplin_reduced(pp, 1), d1)
  expect_identical(poplin_reduced(pp, "d2"), d2)
  expect_identical(poplin_reduced(pp, 2), d2)

  ## Clearing data with empty List.
  pp2 <- pp
  poplin_reduced_list(pp2) <- SimpleList()
  expect_identical(poplin_reduced_list(pp2), setNames(SimpleList(), character(0)))

  ## Clearing data with NULL.
  poplin_reduced_list(pp) <- SimpleList(d1 = d1)
  expect_identical(poplin_reduced_list(pp), SimpleList(d1 = d1))
  expect_identical(poplin_reduced(pp), d1)
  pp2 <- pp
  poplin_reduced_list(pp2) <- NULL
  expect_identical(poplin_reduced_list(pp2), setNames(SimpleList(), character(0)))

  ## Warning: assigning an unnamed list.
  expect_warning(poplin_reduced_list(pp) <- list(d1, d2), "NULL")
  expect_identical(poplin_reduced_names(pp), c("reduced1", "reduced2"))

  expect_warning(poplin_reduced_list(pp) <- list(d1 = d1, d2), "empty")
  expect_identical(poplin_reduced_names(pp), c("d1", "reduced1"))
  
  ## Check different errors.
  expect_error(poplin_reduced_list(pp) <- list(d1, d2[1:10 ,]), "number of rows")
  expect_error(poplin_reduced_list(pp) <- list(d1[1:10, ], d2), "number of rows")
  expect_error(poplin_reduced_list(pp) <- list(d1[1:10, ], d2[1:10, ]), "number of rows")
})

test_that("poplinReducedData setters/getters respond to dimnames.", {
  pp <- faahko_poplin
  expect_warning(poplin_reduced(pp, "d1") <- d1, NA) # no warning
  expect_warning(poplin_reduced(pp, "d2") <- d2, NA) # no warning
  expect_identical(rownames(poplin_reduced(pp)), colnames(pp))
  expect_identical(rownames(poplin_reduced(pp, 2)), colnames(pp))

  out <- poplin_reduced_list(pp)
  expect_identical(rownames(out[[1]]), colnames(pp))
  expect_identical(rownames(out[[2]]), colnames(pp))

  d11 <- d1
  rownames(d11) <- tolower(rownames(d1))
  expect_error(poplin_reduced(pp, "d1") <- d11, "should be the same")
  expect_error(poplin_reduced_list(pp) <- list(d1 = d11), "should be the same")
  expect_error(poplin_reduced(pp, "d1") <- d2, NA)
})

test_that("poplinReducedData setters/getters preserve mcols and metadata.", {
  pp <- faahko_poplin
  stuff <- List(d1=d1, d2=d2)
  mcols(stuff)$A <- c("one", "two")
  metadata(stuff)$B <- "three"

  poplin_reduced_list(pp) <- stuff
  out <- poplin_reduced_list(pp)
  expect_identical(mcols(out), mcols(stuff))
  expect_identical(metadata(out), metadata(stuff))
})

test_that("poplin_reduced setter assigns 'reduced1' for an unnamed object.", {
  pp <- faahko_poplin
  poplin_reduced(pp) <- d1
  expect_identical(poplin_reduced_names(pp), "reduced1")
})

test_that("poplin_reduced setter assigns colnames(x) when rownames(value) =  NULL.", {
  pp <- faahko_poplin
  d1_null <- d1
  rownames(d1_null) <- NULL

  expect_error(poplin_reduced(pp, "null") <- d1_null, NA)
  expect_identical(rownames(poplin_reduced(pp, "null")), colnames(pp))
  expect_error(poplin_reduced_list(pp) <- list(null = d1_null), NA)
  expect_identical(rownames(poplin_reduced(pp, "null")), colnames(pp))

  rownames(d1_null) <- 1:nrow(d1_null)
  expect_error(poplin_reduced(pp, "null") <- d1_null, "non-NULL")
  expect_error(poplin_reduced_list(pp) <- list(null = d1_null), "non-NULL")

})


test_that("poplin_reduced_names setter/getters work correctly.", {
  pp <- faahko_poplin
  expect_warning(poplin_reduced_list(pp) <- list(d1, d2), "NULL")
  expect_identical(poplin_reduced_names(pp), c("reduced1", "reduced2"))

  poplin_reduced_list(pp) <- list(d1=d1, d2=d2)
  expect_identical(poplin_reduced_names(pp), c("d1", "d2"))

  ## Direct assignment.
  poplin_reduced_names(pp) <- c("A", "B")
  expect_identical(poplin_reduced_names(pp), c("A", "B"))

  ## Respond to empty names.
  expect_warning(poplin_reduced_names(pp) <- c("X", ""), "empty")
  expect_identical(poplin_reduced_names(pp), c("X", "reduced1"))

  ## Names wiped.
  poplin_reduced_list(pp) <- NULL
  expect_identical(poplin_reduced_names(pp), character(0))
})




