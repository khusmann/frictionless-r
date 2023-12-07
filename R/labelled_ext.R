#' @import vctrs

labelled_ext <- function(x, labels=NULL, label=NULL, levels=NULL, ...) {
  if (is.character(x) || is.numeric(x)) {
    if (is.null(levels)) {
      haven::labelled(x, labels, label)
    } else {
      new_labelled_enum(x, labels, label, levels)
    }
  } else if (is.logical(x)) {
    assertthat::assert_that(
      is.null(labels),
      is.null(levels),
      msg = "Logical values cannot have labels or levels"
    )
    new_labelled_lgl(x, label)
  } else {
    stop("Error: cannot make labelled vctr from type")
  }
}

new_labelled_lgl <- function(x, label) {
  x <- vec_data(x)

  assertthat::assert_that(
    is.logical(x), msg = glue::glue("Expected logical vector, got {x}")
  )

  vctrs::new_vctr(
    x,
    label = label,
    class = c("haven_labelled"),
    inherit_base_type = TRUE
  )
}

new_labelled_enum <- function(x, labels, label, levels) {
  x <- vec_data(x)

  assertthat::assert_that(
    is.numeric(x) || is.character(x),
    msg = "Labeled enums must be numeric or character"
  )

  assertthat::assert_that(
    is.null(labels) || vec_is(labels, x),
    msg = "Labels must be same type as input vector"
  )

  if (is.null(levels)) {
    levels <- unique(x)
  }

  assertthat::assert_that(
    vec_is(levels, x),
    msg = "Levels must be the same type as input vector"
  )

  missing_levels <- paste0(setdiff(x, levels), collapse = ", ")

  assertthat::assert_that(
    nchar(missing_levels) == 0,
    msg = glue::glue("Missing levels: {missing_levels}")
  )

  assertthat::assert_that(
    is.null(label) || (is.character(label) && length(label) == 1),
    msg = "Label must be a character vector of length one."
  )

  new_vctr(
    x,
    labels = labels,
    label = label,
    levels = levels,
    class = c("haven_labelled_enum", "haven_labelled"),
    inherit_base_type = TRUE
  )
}

#' @export
vec_ptype2.logical.haven_labelled <- function(x, y, ...) {
  data_type <- vec_ptype2(x, vec_data(y), ...)
  labelled_ext(
    data_type,
    labels = vec_cast_named(attr(y, "labels"), data_type),
    label = attr(y, "label", exact = TRUE),
  )
}

#' @export
vec_ptype2.haven_labelled.logical <- function(x, y, ...) vec_ptype2(y, x, ...)

# TODO: Remove once vec_cast() preserves names.
# https://github.com/r-lib/vctrs/issues/623
vec_cast_named <- function(x, to, ...) {
  stats::setNames(vec_cast(x, to, ...), names(x))
}
