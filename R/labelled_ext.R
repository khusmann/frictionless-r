#' @import vctrs

#' @export
labelled_enum <- function(x, labels=NULL, label=NULL, levels=NULL,
                          ordered=FALSE, ...) {
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
    ordered = ordered,
    class = c("haven_labelled_enum", "haven_labelled"),
    inherit_base_type = TRUE
  )
}

#####################################

#' @export
labelled_lgl <- function(x, label=NULL) {
  x <- vec_data(x)

  assertthat::assert_that(
    is.logical(x), msg = glue::glue("Expected logical vector")
  )

  assertthat::assert_that(
    is.null(label) || (is.character(label) && length(label) == 1),
    msg = "Label must be a character vector of length one."
  )

  vctrs::new_vctr(
    x,
    label = label,
    class = c("haven_labelled_lgl", "haven_labelled"),
    inherit_base_type = TRUE
  )
}

#' @export
vec_ptype2.logical.haven_labelled_lgl <- function(x, y, ...) {
  labelled_lgl(logical(), label=attr(y, "label", exact = TRUE))
}

#' @export
vec_ptype2.haven_labelled_lgl.logical <- function(x, y, ...) {
  vec_ptype2(y, x, ...)
}

#' @export
vec_ptype2.haven_labelled_lgl.haven_labelled_lgl <- function(x, y, ...) {
  # Prefer variable labels from LHS
  label <- replace_null(
    attr(x, "label", exact = TRUE),
    attr(y, "label", exact = TRUE)
  )

  labelled_lgl(logical(), label=label)
}

#' @export
vec_cast.haven_labelled_lgl.haven_labelled_lgl <- function(x, to, ...) {
  labelled_lgl(
    vec_data(x),
    label=attr(x, "label", exact = TRUE)
  )
}

#' @export
vec_cast.logical.haven_labelled_lgl <- function(x, to, ...) {
  vec_cast(vec_data(x), to)
}

#' @export
vec_cast.integer.haven_labelled_lgl <- function(x, to, ...) {
  vec_cast(vec_data(x), to)
}

#' @export
vec_cast.double.haven_labelled_lgl <- function(x, to, ...) {
  vec_cast(vec_data(x), to)
}

#####################################


# (Copied from haven::util_ext)
# TODO: Remove once vec_cast() preserves names.
# https://github.com/r-lib/vctrs/issues/623
vec_cast_named <- function(x, to, ...) {
  stats::setNames(vec_cast(x, to, ...), names(x))
}
