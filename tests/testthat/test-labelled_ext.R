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

test_that("labelled_enum validation", {
  # Value label & level type must be coercible to value
  expect_error(labelled_enum(c(1, 2), labels=c(FOO = "1")))
  expect_error(labelled_enum(c(1, 2), levels=c("1", "2")))

  # Missing levels should error
  expect_error(labelled_enum(c(1, 2), levels=c(1)))

  # Variable label must be a character
  expect_error(labelled_ext(c(1, 2), label=2))
})

test_that("labelled_enum (integer)", {
  x <- as.integer(c(1, 1, 2, 1, 1, 2))
  labels <- vec_cast_named(c(FOO = 1), integer())
  label <- "Variable label"
  lvls <- as.integer(c(1, 2))
  ordered <- FALSE

  lbl_enum <- labelled_enum(x, labels, label, lvls, ordered)
  expect_s3_class(lbl_enum, c("haven_lbl_enum", "haven_labelled"))

  # Test is.* methods
  expect_true(is.enum(lbl_enum))
  expect_false(is.ordered_enum(lbl_enum))

  # Test levels
  expect_equal(levels(lbl_enum), lvls)

  # Test level & label type conversion
  expect_identical(
    levels(labelled_enum(x, levels=as.double(lvls))),
    lvls
  )
  expect_identical(
    attr(labelled_enum(x, labels=vec_cast_named(labels, double())), "labels"),
    labels
  )

  # Integer enums shouldn't automatically combine with or cast to logical
  expect_error(
    vec_ptype2(lbl_enum, logical()),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_ptype2(logical(), lbl_enum),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(lbl_enum, logical()),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(logical(), lbl_enum),
    class = "vctrs_error_incompatible_type"
  )

  # Integer enums shouldn't automatically combine with or cast to character
  expect_error(
    vec_ptype2(lbl_enum, character()),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_ptype2(character(), lbl_enum),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(lbl_enum, character()),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(character(), lbl_enum),
    class = "vctrs_error_incompatible_type"
  )

  # Character enums should not be combined or cast with integer enums
  expect_error(
    vec_ptype2(lbl_enum, labelled_enum(character())),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_ptype2(labelled_enum(character()), lbl_enum),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(lbl_enum, labelled_enum(character())),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(labelled_enum(character()), lbl_enum),
    class = "vctrs_error_incompatible_type"
  )

  # Integer enums should promote the underlying type when combined with numeric
  expect_identical(vec_ptype2(lbl_enum, integer()), vec_ptype(lbl_enum))
  expect_identical(
    vec_ptype2(lbl_enum, double()),
    labelled_enum(double(), labels, label, lvls, ordered)
  )
  expect_identical(vec_ptype2(integer(), lbl_enum), vec_ptype(lbl_enum))
  expect_identical(
    vec_ptype2(double(), lbl_enum),
    labelled_enum(double(), labels, label, lvls, ordered)
  )

  # Integer enums should cast to numeric types by zapping
  expect_identical(vec_cast(lbl_enum, integer()), x)
  expect_identical(vec_cast(lbl_enum, double()), as.double(x))

  # Casting numeric types to integer enums should only work when they
  # have a subset of the levels
  expect_identical(
    vec_cast(as.integer(1), lbl_enum),
    labelled_enum(as.integer(1), labels, label, lvls, ordered)
  )
  expect_identical(
    vec_cast(as.integer(c(1, 2)), lbl_enum),
    labelled_enum(as.integer(c(1,2)), labels, label, lvls, ordered)
  )
  expect_identical(
    vec_cast(1, lbl_enum),
    labelled_enum(as.integer(1), labels, label, lvls, ordered)
  )
  expect_identical(
    vec_cast(c(1, 2), lbl_enum),
    labelled_enum(as.integer(c(1,2)), labels, label, lvls, ordered)
  )
  expect_error(
    vec_cast(as.integer(c(1, 2, 3)), lbl_enum),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    vec_cast(c(1, 2, 3), lbl_enum),
    class = "vctrs_error_incompatible_type"
  )

  # Combining two enums should combine their levels (and promote underlying
  # type, if necessary).
  expect_identical(
    vec_ptype2(lbl_enum, labelled_enum(integer(), levels=c(1,3))),
    labelled_enum(integer(), labels, label, levels=c(1,2,3))
  )
  expect_identical(
    vec_ptype2(lbl_enum, labelled_enum(double(), levels=c(1,3))),
    labelled_enum(double(), labels, label, levels=c(1,2,3))
  )
  expect_identical(
    vec_ptype2(labelled_enum(double(), levels=c(1,3)), lbl_enum),
    labelled_enum(double(), labels, label, levels=c(1,3,2))
  )
  expect_identical(
    vec_ptype2(labelled_enum(integer(), levels=c(1,3)), lbl_enum),
    labelled_enum(integer(), labels, label, levels=c(1,3,2))
  )

  # Combining two enums should combine their labels (and warn when there's
  # clashes
  expect_identical(
    vec_ptype2(lbl_enum, labelled_enum(integer(), labels=c(BAR=2))),
    labelled_enum(integer(), labels=c(labels, BAR=2), label, lvls, ordered)
  )

  expect_warning(
    vec_ptype2(lbl_enum, labelled_enum(integer(), labels=c(BAR=1)))
  )

  # Casting should only work when the levels of from are a subset of "to"
  expect_identical(
    vec_cast(labelled_enum(as.integer(1)), lbl_enum),
    labelled_enum(as.integer(1), labels, label, lvls, ordered)
  )
  expect_error(
    vec_cast(labelled_enum(as.integer(c(1, 3))), lbl_enum),
    class = "vctrs_error_incompatible_type"
  )

  # Casting should coalesce labels when one is unlabelled
  expect_identical(
    vec_cast(lbl_enum, labelled_enum(integer(), levels=lvls)),
    lbl_enum
  )
  expect_identical(
    vec_cast(labelled_enum(as.integer(c(1,2))), lbl_enum),
    labelled_enum(as.integer(c(1,2)), labels, label, lvls, ordered)
  )

  # Casting should add labels, but fail when losing labels
  expect_identical(
    vec_cast(lbl_enum, labelled_enum(integer(), labels=c(BAR=1, BAZ=2), levels=lvls)),
    labelled_enum(x, labels=c(BAR=1, BAZ=2), label, lvls)
  )
  expect_error(
    vec_cast(lbl_enum, labelled_enum(integer(), labels=c(BAZ=2), levels=lvls)),
    class = "vctrs_error_incompatible_type"
  )
})
