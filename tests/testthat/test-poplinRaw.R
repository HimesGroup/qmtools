test_that("poplin_raw is the alias of assay.", {
  expect_equal(poplin_raw_names(d), assayNames(d))
  expect_equal(poplin_raw_list(d), assays(d))
  expect_equal(poplin_raw(d, "raw"), assay(d, "raw"))
  expect_equal(poplin_raw(d, "raw_filled"), assay(d, "raw_filled"))
})

nm <- prod(dim(faahko_poplin))
d1 <- matrix(rnorm(nm), nrow = nrow(faahko_poplin), ncol = ncol(faahko_poplin))
rownames(d1) <- rownames(faahko_poplin)
colnames(d1) <- colnames(faahko_poplin)
d2 <- matrix(rnorm(nm), nrow = nrow(faahko_poplin), ncol = ncol(faahko_poplin))
rownames(d2) <- rownames(faahko_poplin)
colnames(d2) <- colnames(faahko_poplin)

test_that("poplinRaw setters/getters work with character 'type'.", {
  pp <- faahko_poplin
  poplin_raw_list(pp) <- SimpleList()
  poplin_raw(pp, "d1") <- d1
  expect_identical(poplin_raw(pp, "d1"), d1)
  expect_identical(poplin_raw_list(pp), SimpleList(d1 = d1))
  expect_identical(poplin_raw_names(pp), "d1")
  
  poplin_raw(pp, "d2") <- d2
  expect_identical(poplin_raw(pp, "d2"), d2)
  expect_identical(poplin_raw_list(pp), SimpleList(d1 = d1, d2 = d2))
  expect_identical(poplin_raw_names(pp), c("d1", "d2"))
  
  ## Clearing data with NULL.
  poplin_raw(pp, "d1") <- NULL
  expect_identical(poplin_raw(pp, "d2"), d2)
  expect_identical(poplin_raw_list(pp), SimpleList(d2 = d2))
  expect_identical(poplin_raw_names(pp), "d2")
  
  ## Check different errors.
  d4 <- d3 <- d1
  rownames(d3)[1] <- "xxx"
  colnames(d4)[1] <- "xxx"
  expect_error(poplin_raw(pp, "d1"), "invalid subscript")
  expect_error(poplin_raw(pp, 2), "invalid subscript")
  expect_error(poplin_raw(pp, "d1") <- d1[1:2, ], "dimension")
  expect_error(poplin_raw(pp, "d2") <- d1[1:2, ], "dimension")
  expect_error(poplin_raw(pp, "d2") <- d1[, 1:2], "dimension")
  expect_error(poplin_raw(pp, "d3") <- d3, "rownames")
  expect_error(poplin_raw(pp, "d4") <- d4, "colnames")
  expect_error(poplin_raw(pp, 1) <- "hello", "dimension")
})

test_that("poplinRaw setters/getters work with numeric 'type'.", {
  pp <- faahko_poplin
  poplin_raw_list(pp) <- SimpleList()
  expect_error(poplin_raw(pp), "invalid subscript")
  expect_error(poplin_raw(pp, 2), "invalid subscript")
  expect_error(poplin_raw(pp, "d1"), "invalid subscript")

  ## Different behavior from poplin_data
  ## expect_error(poplin_raw(pp, 1) <- d1, "out of bounds")
  ## expect_error(poplin_raw(pp, 2) <- d2, "out of bounds")

  ## Warning: assigning an unnamed list.
  expect_warning(poplin_raw_list(pp) <- list(d1, d2))
  expect_identical(poplin_raw(pp), d1)
  expect_identical(poplin_raw(pp, 2), d2)
  expect_identical(poplin_raw_names(pp), c("raw1", "raw2"))
  d11 <- d1 * 10
  poplin_raw(pp, "d11") <- d11
  expect_identical(poplin_raw(pp, 1), d1)
  expect_identical(poplin_raw(pp, 2), d2)
  expect_identical(poplin_raw(pp, 3), d11)
  expect_identical(poplin_raw_names(pp), c("raw1", "raw2", "d11"))

  ## Clearing data with NULL.
  poplin_raw(pp, "raw1") <- NULL
  expect_identical(poplin_raw(pp), d2) # now d2 is the first entry
  expect_identical(poplin_raw(pp, 1), d2)
  expect_identical(poplin_raw(pp, 2), poplin_raw(pp, "d11"))
  expect_identical(poplin_raw_names(pp), c("raw2", "d11"))

  poplin_raw(pp) <- NULL # now d11 is the first entry
  expect_equal(poplin_raw(pp), d11)
  expect_equal(poplin_raw_names(pp), c("d11"))
  poplin_raw(pp) <- d2 # overwrite
  expect_equal(poplin_raw(pp, 1), d2)
  expect_equal(poplin_raw_names(pp), "d11") # maintain the same label

  expect_error(poplin_raw(pp, 5) <- d1, "subscript must be")
})

test_that("poplinRaw setters/getters work with List'.", {
  pp <- faahko_poplin
  poplin_raw_list(pp) <- SimpleList()
  poplin_raw_list(pp) <- list(d1 = d1, d2 = d2)
  expect_identical(poplin_raw_names(pp), c("d1", "d2"))
  expect_identical(poplin_raw(pp, "d1"), d1)
  expect_identical(poplin_raw(pp, 1), d1)
  expect_identical(poplin_raw(pp, "d2"), d2)
  expect_identical(poplin_raw(pp, 2), d2)

  ## Clearing data with empty List.
  pp2 <- pp
  poplin_raw_list(pp2) <- SimpleList()
  expect_identical(poplin_raw_list(pp2), SimpleList()) # different from poplin_data_list

  ## Warning: assigning an unnamed list.
  expect_warning(poplin_raw_list(pp) <- list(d1, d2), "NULL")
  expect_identical(poplin_raw_names(pp), c("raw1", "raw2"))

  expect_warning(poplin_raw_list(pp) <- list(d1 = d1, d2), "empty")
  expect_identical(poplin_raw_names(pp), c("d1", "raw1"))
  
  ## Check different errors.
  expect_error(poplin_raw_list(pp) <- list(d1, d2[1:10,]), "dimension")
  expect_error(poplin_raw_list(pp) <- list(d1[, 1:10], d2), "dimension")
  expect_error(poplin_raw_list(pp) <- list(d1[1:10,], d2[, 1:10]), "dimension")
})

test_that("poplinRaw setters/getters respond to dimnames.", {
  pp <- faahko_poplin
  poplin_raw_list(pp) <- SimpleList()
  expect_warning(poplin_raw(pp, "d1") <- d1, NA) # no warning
  expect_warning(poplin_raw(pp, "d2") <- d2, NA) # no warning
  expect_identical(rownames(poplin_raw(pp)), rownames(pp))
  expect_identical(rownames(poplin_raw(pp, 2)), rownames(pp))
  expect_identical(colnames(poplin_raw(pp)), colnames(pp))
  expect_identical(colnames(poplin_raw(pp, 2)), colnames(pp))

  out <- poplin_raw_list(pp)
  expect_identical(rownames(out[[1]]), rownames(pp))
  expect_identical(rownames(out[[2]]), rownames(pp))
  expect_identical(colnames(out[[1]]), colnames(pp))
  expect_identical(colnames(out[[2]]), colnames(pp))

  d11 <- d1
  rownames(d11) <- tolower(rownames(d1))
  colnames(d11) <- tolower(colnames(d1))
  expect_error(poplin_raw(pp, "d1") <- d11, "should be the same")
  expect_error(poplin_raw_list(pp) <- list(d1 = d11), "should be the same")
  expect_error(poplin_raw(pp, "d1") <- d2, NA)
})

test_that("poplinRaw setters/getters preserve mcols and metadata.", {
  pp <- faahko_poplin
  poplin_raw_list(pp) <- SimpleList()
  stuff <- List(d1=d1, d2=d2)
  mcols(stuff)$A <- c("one", "two")
  metadata(stuff)$B <- "three"

  poplin_raw_list(pp) <- stuff
  out <- poplin_raw_list(pp)
  expect_identical(mcols(out), mcols(stuff))
  expect_identical(metadata(out), metadata(stuff))
})


test_that("poplin_raw setter assigns 'raw1' for an unnamed object.", {
  pp <- faahko_poplin
  poplin_raw_list(pp) <- SimpleList()
  poplin_raw(pp) <- d1
  expect_identical(poplin_raw_names(pp), "raw1")
})

test_that("poplin_raw setter assigns dimnames(x) when dimnames(value) =  NULL.", {
  pp <- faahko_poplin
  poplin_raw_list(pp) <- SimpleList()
  d1_null <- d1
  rownames(d1_null) <- NULL
  colnames(d1_null) <- NULL

  expect_error(poplin_raw(pp, "null") <- d1_null, NA)
  expect_identical(dimnames(poplin_raw(pp, "null")), dimnames(pp))
  expect_error(poplin_raw_list(pp) <- list(null = d1_null), NA)
  expect_identical(dimnames(poplin_raw(pp, "null")), dimnames(pp))

  rownames(d1_null) <- 1:nrow(d1_null)
  expect_error(poplin_raw(pp, "null") <- d1_null, "non-NULL")
  expect_error(poplin_raw_list(pp) <- list(null = d1_null), "non-NULL")

})

test_that("poplin_raw_names setter/getters work correctly.", {
  pp <- faahko_poplin
  poplin_raw_list(pp) <- SimpleList()
  expect_warning(poplin_raw_list(pp) <- list(d1, d2), "NULL")
  expect_identical(poplin_raw_names(pp), c("raw1", "raw2"))

  poplin_raw_list(pp) <- list(d1=d1, d2=d2)
  expect_identical(poplin_raw_names(pp), c("d1", "d2"))

  ## Direct assignment.
  poplin_raw_names(pp) <- c("A", "B")
  expect_identical(poplin_raw_names(pp), c("A", "B"))

  ## Respond to empty names.
  expect_warning(poplin_raw_names(pp) <- c("X", ""), "empty")
  expect_identical(poplin_raw_names(pp), c("X", "raw1"))

})
