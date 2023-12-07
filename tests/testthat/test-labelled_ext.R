test_that("labelled_ext constructs regular haven_labelled types", {
  test_character <- c("a", "b", "c")
  test_integer <- as.integer(c(1, 2, 3))
  test_double <- c(1, 2, 3)

  expect_s3_class(labelled_ext(test_character), "haven_labelled")
  expect_s3_class(labelled_ext(test_integer), "haven_labelled")
  expect_s3_class(labelled_ext(test_double), "haven_labelled")
})

test_that("haven_labelled works with logical types", {
  labelled_lgl <- labelled_ext(c(TRUE, FALSE, TRUE), label = "Variable label")

  expect_s3_class(labelled_lgl, "haven_labelled")

  # Test type promotion
  expect_equal(labelled_lgl == FALSE, c(FALSE, TRUE, FALSE))
  expect_equal(FALSE == labelled_lgl, c(FALSE, TRUE, FALSE))

  expect_equal(labelled_lgl == 0, c(FALSE, TRUE, FALSE))
  expect_equal(0 == labelled_lgl, c(FALSE, TRUE, FALSE))

  # Logical types should not be constructed with value labels or levels
  expect_error(labelled_ext(TRUE, labels=c(FOO=1)))
  expect_error(labelled_ext(TRUE, levels="FOO"))
})

test_that("labelled_ext constructs haven_labelled_enum types", {
  x <- c(1, 1, 2, 1, 1, 2)
  labels <- c(FOO = 1)
  label <- "Variable label"
  levels <- c(1, 2)

  labelled_enum <- labelled_ext(x, labels, label, levels)

  expect_s3_class(labelled_enum, c("haven_labelled_enum", "haven_labelled"))

  # Value label type must match value
  expect_error(labelled_ext(x, labels=c(FOO = "1")))

  # Missing levels should error
  expect_error(labelled_ext(x, levels=c(1)))
  expect_error(labelled_ext(x, levels=c("1", "2")))

  # Variable label must be a character
  expect_error(labelled_ext(x, labels, 2, levels))
})
