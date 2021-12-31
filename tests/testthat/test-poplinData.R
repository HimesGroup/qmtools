nm <- prod(dim(faahko_poplin))
d1 <- matrix(rnorm(nm), nrow = nrow(faahko_poplin), ncol = ncol(faahko_poplin))
rownames(d1) <- rownames(faahko_poplin)
colnames(d1) <- colnames(faahko_poplin)
d2 <- matrix(rnorm(nm), nrow = nrow(faahko_poplin), ncol = ncol(faahko_poplin))
rownames(d2) <- rownames(faahko_poplin)
colnames(d2) <- colnames(faahko_poplin)

test_that("poplinData setters/getters work with character 'type'.", {
  pp <- empty
  poplin_data(pp, "d1") <- d1
  expect_identical(poplin_data(pp, "d1"), d1)
  expect_identical(poplin_data_list(pp), SimpleList(d1 = d1))
  expect_identical(poplin_data_names(pp), "d1")
  
  poplin_data(pp, "d2") <- d2
  expect_identical(poplin_data(pp, "d2"), d2)
  expect_identical(poplin_data_list(pp), SimpleList(d1 = d1, d2 = d2))
  expect_identical(poplin_data_names(pp), c("d1", "d2"))
  
  ## Clearing data with NULL.
  poplin_data(pp, "d1") <- NULL
  expect_identical(poplin_data(pp, "d2"), d2)
  expect_identical(poplin_data_list(pp), SimpleList(d2 = d2))
  expect_identical(poplin_data_names(pp), "d2")
  
  ## Check different errors.
  d4 <- d3 <- d1
  rownames(d3)[1] <- "xxx"
  colnames(d4)[1] <- "xxx"
  expect_error(poplin_data(pp, "d1"), "invalid subscript")
  expect_error(poplin_data(pp, 2), "invalid subscript")
  expect_error(poplin_data(pp, "d1") <- d1[1:2, ], "dimension")
  expect_error(poplin_data(pp, "d2") <- d1[1:2, ], "dimension")
  expect_error(poplin_data(pp, "d2") <- d1[, 1:2], "dimension")
  expect_error(poplin_data(pp, "d3") <- d3, "rownames")
  expect_error(poplin_data(pp, "d4") <- d4, "colnames")
  expect_error(poplin_data(pp, 1) <- "hello", "dimension")
})

test_that("poplinData setters/getters work with numeric 'type'.", {
  pp <- empty
  expect_error(poplin_data(pp), "no available entries")
  expect_error(poplin_data(pp, 2), "invalid subscript")
  expect_error(poplin_data(pp, "d1"), "invalid subscript")

  expect_error(poplin_data(pp, 1) <- d1, "out of bounds")
  expect_error(poplin_data(pp, 2) <- d2, "out of bounds")

  ## Warning: assigning an unnamed list.
  expect_warning(poplin_data_list(pp) <- list(d1, d2))
  expect_identical(poplin_data(pp), d1)
  expect_identical(poplin_data(pp, 2), d2)
  expect_identical(poplin_data_names(pp), c("poplin1", "poplin2"))
  d11 <- d1 * 10
  poplin_data(pp, "d11") <- d11
  expect_identical(poplin_data(pp, 1), d1)
  expect_identical(poplin_data(pp, 2), d2)
  expect_identical(poplin_data(pp, 3), d11)
  expect_identical(poplin_data_names(pp), c("poplin1", "poplin2", "d11"))

  ## Clearing data with NULL.
  poplin_data(pp, "poplin1") <- NULL
  expect_identical(poplin_data(pp), d2) # now d2 is the first entry
  expect_identical(poplin_data(pp, 1), d2)
  expect_identical(poplin_data(pp, 2), poplin_data(pp, "d11"))
  expect_identical(poplin_data_names(pp), c("poplin2", "d11"))

  poplin_data(pp) <- NULL # now d11 is the first entry
  expect_equal(poplin_data(pp), d11)
  expect_equal(poplin_data_names(pp), c("d11"))
  poplin_data(pp) <- d2 # overwrite
  expect_equal(poplin_data(pp, 1), d2)
  expect_equal(poplin_data_names(pp), "d11") # maintain the same label

  expect_error(poplin_data(pp, 5) <- d1, "out of bounds")
})

test_that("poplinData setters/getters work with List'.", {
  pp <- empty
  poplin_data_list(pp) <- list(d1 = d1, d2 = d2)
  expect_identical(poplin_data_names(pp), c("d1", "d2"))
  expect_identical(poplin_data(pp, "d1"), d1)
  expect_identical(poplin_data(pp, 1), d1)
  expect_identical(poplin_data(pp, "d2"), d2)
  expect_identical(poplin_data(pp, 2), d2)

  ## Clearing data with empty List.
  pp2 <- pp
  poplin_data_list(pp2) <- SimpleList()
  expect_identical(poplin_data_list(pp2), setNames(SimpleList(), character(0)))

  ## Clearing data with NULL.
  poplin_data_list(pp) <- SimpleList(d1 = d1)
  expect_identical(poplin_data_list(pp), SimpleList(d1 = d1))
  expect_identical(poplin_data(pp), d1)
  pp2 <- pp
  poplin_data_list(pp2) <- NULL
  expect_identical(poplin_data_list(pp2), setNames(SimpleList(), character(0)))

  ## Warning: assigning an unnamed list.
  expect_warning(poplin_data_list(pp) <- list(d1, d2), "NULL")
  expect_identical(poplin_data_names(pp), c("poplin1", "poplin2"))

  expect_warning(poplin_data_list(pp) <- list(d1 = d1, d2), "empty")
  expect_identical(poplin_data_names(pp), c("d1", "poplin1"))
  
  ## Check different errors.
  expect_error(poplin_data_list(pp) <- list(d1, d2[1:10,]), "dimension")
  expect_error(poplin_data_list(pp) <- list(d1[, 1:10], d2), "dimension")
  expect_error(poplin_data_list(pp) <- list(d1[1:10,], d2[, 1:10]), "dimension")
})

test_that("poplinData setters/getters respond to dimnames.", {
  pp <- empty
  expect_warning(poplin_data(pp, "d1") <- d1, NA) # no warning
  expect_warning(poplin_data(pp, "d2") <- d2, NA) # no warning
  expect_identical(rownames(poplin_data(pp)), rownames(pp))
  expect_identical(rownames(poplin_data(pp, 2)), rownames(pp))
  expect_identical(colnames(poplin_data(pp)), colnames(pp))
  expect_identical(colnames(poplin_data(pp, 2)), colnames(pp))

  out <- poplin_data_list(pp)
  expect_identical(rownames(out[[1]]), rownames(pp))
  expect_identical(rownames(out[[2]]), rownames(pp))
  expect_identical(colnames(out[[1]]), colnames(pp))
  expect_identical(colnames(out[[2]]), colnames(pp))

  d11 <- d1
  rownames(d11) <- tolower(rownames(d1))
  colnames(d11) <- tolower(colnames(d1))
  expect_error(poplin_data(pp, "d1") <- d11, "should be the same")
  expect_error(poplin_data_list(pp) <- list(d1 = d11), "should be the same")
  expect_error(poplin_data(pp, "d1") <- d2, NA)
})

test_that("poplinData setters/getters preserve mcols and metadata.", {
  pp <- empty
  stuff <- List(d1=d1, d2=d2)
  mcols(stuff)$A <- c("one", "two")
  metadata(stuff)$B <- "three"

  poplin_data_list(pp) <- stuff
  out <- poplin_data_list(pp)
  expect_identical(mcols(out), mcols(stuff))
  expect_identical(metadata(out), metadata(stuff))
})


test_that("poplin_data setter assigns 'poplin1' for an unnamed object.", {
  pp <- empty
  poplin_data(pp) <- d1
  expect_identical(poplin_data_names(pp), "poplin1")
})

test_that("poplin_data setter assigns dimnames(x) when dimnames(value) =  NULL.", {
  pp <- empty
  d1_null <- d1
  rownames(d1_null) <- NULL
  colnames(d1_null) <- NULL

  expect_error(poplin_data(pp, "null") <- d1_null, NA)
  expect_identical(dimnames(poplin_data(pp, "null")), dimnames(pp))
  expect_error(poplin_data_list(pp) <- list(null = d1_null), NA)
  expect_identical(dimnames(poplin_data(pp, "null")), dimnames(pp))

  rownames(d1_null) <- 1:nrow(d1_null)
  expect_error(poplin_data(pp, "null") <- d1_null, "non-NULL")
  expect_error(poplin_data_list(pp) <- list(null = d1_null), "non-NULL")

})

test_that("poplin_data_names setter/getters work correctly.", {
  pp <- empty
  expect_warning(poplin_data_list(pp) <- list(d1, d2), "NULL")
  expect_identical(poplin_data_names(pp), c("poplin1", "poplin2"))

  poplin_data_list(pp) <- list(d1=d1, d2=d2)
  expect_identical(poplin_data_names(pp), c("d1", "d2"))

  ## Direct assignment.
  poplin_data_names(pp) <- c("A", "B")
  expect_identical(poplin_data_names(pp), c("A", "B"))

  ## Respond to empty names.
  expect_warning(poplin_data_names(pp) <- c("X", ""), "empty")
  expect_identical(poplin_data_names(pp), c("X", "poplin1"))

  ## Names wiped.
  poplin_data_list(pp) <- NULL
  expect_identical(poplin_data_names(pp), character(0))
})
