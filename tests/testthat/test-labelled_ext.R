#' @import vctrs

test_that("haven_labelled_lgl works", {
  vec <- c(TRUE, FALSE, TRUE)
  var_label <- "Variable label"
  var_label2 <- "Variable label2"

  lbl_lgl <- labelled_lgl(vec, label = var_label)
  lbl_lgl2 <- labelled_lgl(vec, label = var_label2)

  expect_s3_class(lbl_lgl, "haven_labelled")

  # Test vec_ptype2
  expect_equal(
    vec_ptype2(lbl_lgl, logical()),
    labelled_lgl(logical(), label=var_label)
  )
  expect_error(vec_ptype2(lbl_lgl, integer()))
  expect_error(vec_ptype2(lbl_lgl, double()))
  expect_error(vec_ptype2(lbl_lgl, character()))

   expect_equal(
    vec_ptype2(logical(), lbl_lgl),
    labelled_lgl(logical(), label=var_label)
  )
  expect_error(vec_ptype2(integer(), lbl_lgl))
  expect_error(vec_ptype2(double(), lbl_lgl))
  expect_error(vec_ptype2(character(), lbl_lgl))

  # Test vec_cast
  expect_equal(vec_cast(lbl_lgl, logical()), as.logical(vec))
  expect_equal(vec_cast(lbl_lgl, integer()), as.integer(vec))
  expect_equal(vec_cast(lbl_lgl, double()), as.double(vec))
  expect_error(vec_cast(lbl_lgl, character()))

  expect_equal(vec_cast(vec, lbl_lgl), lbl_lgl)
  expect_error(vec_cast(integer(), lbl_lgl))
  expect_error(vec_cast(double(), lbl_lgl))
  expect_error(vec_cast(character(), lbl_lgl))

  # Misc test casts & promotion
  expect_equal(lbl_lgl == FALSE, !vec)
  expect_equal(FALSE == lbl_lgl, !vec)
  expect_equal(
    c(lbl_lgl, lbl_lgl2),
    labelled_lgl(c(vec, vec), label=var_label)
  )
  expect_equal(
    c(lbl_lgl, vec),
    labelled_lgl(c(vec, vec), label=var_label)
  )
  expect_equal(
    c(vec, lbl_lgl),
    c(vec, vec)
  )

  # Test type assertion
  expect_true(is.logical(lbl_lgl))

  # Test type casts
  expect_equal(as.logical(lbl_lgl), vec)
  expect_equal(as.integer(lbl_lgl), as.integer(vec))
  expect_equal(as.double(lbl_lgl), as.double(vec))
  expect_equal(as.character(lbl_lgl), as.character(vec))
  # TODO: as_factor

  # Labels should be string
  expect_error(lbl_lgl(TRUE, label=1))

  # Data should be logical
  expect_error(labelled_lgl("foo"))
})

test_that("labelled_enum works", {
  x <- c(1, 1, 2, 1, 1, 2)
  labels <- c(FOO = 1)
  label <- "Variable label"
  levels <- c(1, 2)

  lbl_enum <- labelled_enum(x, labels, label, levels, FALSE)
  lbl_enum_ordered <- labelled_enum(x, labels, label, levels, TRUE)

  expect_s3_class(lbl_enum, c("haven_lbl_enum", "haven_labelled"))
  expect_s3_class(lbl_enum_ordered, c("haven_lbl_enum", "haven_labelled"))

  # Test is.* methods
  expect_true(is.enum(lbl_enum))
  expect_true(is.enum(lbl_enum_ordered))
  expect_false(is.ordered_enum(lbl_enum))
  expect_true(is.ordered_enum(lbl_enum_ordered))

  # Test levels
  expect_equal(levels(lbl_enum), levels)

  ### Test construction errors

  # Value label type must match value
  expect_error(labelled_ext(x, labels=c(FOO = "1")))

  # Missing levels should error
  expect_error(labelled_ext(x, levels=c(1)))
  expect_error(labelled_ext(x, levels=c("1", "2")))

  # Variable label must be a character
  expect_error(labelled_ext(x, labels, 2, levels))
})
