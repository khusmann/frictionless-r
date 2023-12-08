#' @import vctrs

#' @export
labelled_ext <- function(x, labels=NULL, label=NULL, levels=NULL,
                          ordered=NULL, ...) {
  if (is.character(x) || is.numeric(x)) {
    if (is.null(levels)) {
      haven::labelled(x, labels=labels, label=label)
    } else {
      labelled_enum(
        x, labels=labels, label=label, levels=levels, ordered=ordered
      )
    }
  } else if (is.logical(x)) {
    assertthat::assert_that(
      is.null(labels), is.null(levels), is.null(ordered),
      msg = "Logical values cannot have value labels, levels, or ordering"
    )
    labelled_lgl(x, label=label)
  } else {
    stop("Vector must be character, numeric, or logical")
  }
}

#' @export
labelled_enum <- function(x, labels=NULL, label=NULL, levels=NULL,
                          ordered=NULL, ...) {
  x <- vec_data(x)

  assertthat::assert_that(
    is.numeric(x) || is.character(x),
    msg = "Labeled enums must be numeric or character"
  )


  if (!is.null(labels)) {
    assertthat::assert_that(
      !is.null(names(labels)),
      msg = "Labels must have names"
    )

    labels <- vec_cast_named(labels, x)
  }

  if (is.null(levels)) {
    levels <- unique(x)
  } else {
    levels <- vec_cast(levels, x)
  }

  missing_levels <- paste0(setdiff(x, levels), collapse = ", ")

  assertthat::assert_that(
    nchar(missing_levels) == 0,
    msg = glue::glue("Missing levels: {missing_levels}")
  )

  assertthat::assert_that(
    is.null(label) || (is.character(label) && length(label) == 1),
    msg = "Label must be a character vector of length one."
  )

  ordered <- replace_null(ordered, FALSE)

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

#' @export
is.enum <- function(x, ...) {
  inherits(x, "haven_labelled_enum")
}

#' @export
is.ordered_enum <- function(x, ...) {
  is.enum(x) && attr(x, "ordered", exact=TRUE)
}

#' @export
vec_ptype_full.haven_labelled_enum <- function(x, ...) {
  paste0("labelled_enum<", vctrs::vec_ptype_full(vec_data(x)), ">")
}

#' @export
vec_ptype_abbr.haven_labelled_enum <- function(x, ...) {
  paste0("e", vec_ptype_abbr(vec_data(x)), "+lbl")
}

#' @export
obj_print_footer.haven_labelled_enum <- function(x, ...) {
  haven::print_labels(x)
  cat(paste(c("\nLevels:", levels(x), "\n")))
  invisible(x)
}

#' @export
levels.haven_labelled_enum <- function(x, ...) {
  attr(x, "levels", exact = TRUE)
}

#' @export
vec_ptype2.double.haven_labelled_enum <- function(x, y, ...) {
  data_type <- vec_ptype2(x, vec_data(y), ...)
  labelled_enum(
    data_type,
    labels = vec_cast_named(attr(y, "labels"), data_type),
    label = attr(y, "label", exact = TRUE),
    levels = vec_cast_named(levels(y), data_type),
    ordered = attr(y, "ordered")
  )
}

#' @export
vec_ptype2.integer.haven_labelled_enum <- vec_ptype2.double.haven_labelled_enum

#' @export
vec_ptype2.character.haven_labelled_enum <- vec_ptype2.double.haven_labelled_enum

#' @export
vec_ptype2.haven_labelled_enum.double <- function(x, y, ...) {
  vec_ptype2(y, x, ...)
}

#' @export
vec_ptype2.haven_labelled_enum.integer <- vec_ptype2.haven_labelled_enum.double

#' @export
vec_ptype2.haven_labelled_enum.haven_labelled_enum <- function(x, y, ...,
                                                               x_arg = "",
                                                               y_arg = "") {
  # Use x as the prototype if the input vectors have matching metadata
  if (identical(attributes(x), attributes(y))) {
    return(x)
  }

  data_type <- vec_ptype2(
    vec_data(x), vec_data(y), ..., x_arg = x_arg, y_arg = y_arg
  )

  # Prefer value labels from LHS
  x_labels <- vec_cast_named(attr(x, "labels"), data_type, x_arg = x_arg)
  y_labels <- vec_cast_named(attr(y, "labels"), data_type, x_arg = y_arg)
  labels <- combine_labels(x_labels, y_labels, x_arg, y_arg)

  # Take the union of the levels
  x_levels <- vec_cast(levels(x), data_type, x_arg = x_arg)
  y_levels <- vec_cast(levels(y), data_type, x_arg = y_arg)
  lvls <- union(x_levels, y_levels)

  # If one is ordered and the other isn't, drop ordering
  x_ordered <- attr(x, "ordered")
  y_ordered <- attr(y, "ordered")
  ordered <- (x_ordered && y_ordered && identical(x_levels, y_levels))

  # Prefer variable labels from LHS
  label <- replace_null(
    attr(x, "label", exact = TRUE),
    attr(y, "label", exact = TRUE)
  )

  labelled_enum(
    data_type,
    labels = labels,
    label = label,
    levels = lvls,
    ordered = ordered
  )
}

#' @export
vec_cast.double.haven_labelled_enum <- function(x, to, ...) {
  vec_cast(vec_data(x), to)
}

#' @export
vec_cast.integer.haven_labelled_enum <- function(x, to, ...) {
  vec_cast(vec_data(x), to)
}

#' @export
vec_cast.character.haven_labelled_enum <- function(x, to, ...) {
  if (is.character(x)) {
    vec_cast(vec_data(x), to, ...)
  } else {
    stop_incompatible_cast(x, to, ...)
  }
}

#' @export
vec_cast.haven_labelled_enum.haven_labelled_enum <- function(x, to, ...,
                                                             x_arg = "",
                                                             to_arg = "") {
  # Don't perform any processing if the input vectors have matching metadata
  if (identical(attributes(x), attributes(to))) {
    return(x)
  }

  out_data <- vec_cast(
    vec_data(x),
    vec_data(to),
    ...,
    x_arg = x_arg,
    to_arg = to_arg
  )

  x_label <- attr(x, "label", exact = TRUE)
  to_label <- attr(to, "label", exact = TRUE)

  x_labels <- attr(x, "labels")
  to_labels <- attr(to, "labels")

  x_levels <- levels(x)
  to_levels <- levels(to)

  x_ordered <- attr(x, "ordered")
  to_ordered <- attr(to, "ordered")

  out_label <- replace_null(to_label, x_label)
  out_labels <- replace_null(to_labels, x_labels)
  out_levels <- to_levels
  out_ordered <- replace_null(to_ordered, x_ordered)

  if (length(setdiff(x_levels, out_levels)) > 0) {
    stop_incompatible_cast(x, to, ..., x_arg=x_arg, to_arg=to_arg)
  }

  out <- labelled_enum(
    out_data,
    labels = out_labels,
    label = out_label,
    levels = out_levels,
    ordered = out_ordered,
  )

  if (!is.null(to_labels)) {
    lossy <- x %in% x_labels[!x_labels %in% out_labels]
    maybe_lossy_cast(
      out, x, to, lossy,
      x_arg = x_arg,
      to_arg = to_arg,
      details = paste0("Values are labelled in `", x_arg, "` but not in `", to_arg, "`.")
    )
  }


  out
}

#' @export
vec_cast.haven_labelled_enum.double <- function(x, to, ...) {
  vec_cast.haven_labelled_enum.haven_labelled_enum(
    labelled_enum(x), to, ...
  )
}

#' @export
vec_cast.haven_labelled_enum.integer <- function(x, to, ...) {
  vec_cast.haven_labelled_enum.haven_labelled_enum(
    labelled_enum(x), to, ...
  )
}

#' @export
vec_cast.haven_labelled_enum.character <- function(x, to, ...) {
  vec_cast.haven_labelled_enum.haven_labelled_enum(
    labelled_enum(x), to, ...
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


# (Copied from haven::utils)

# TODO: Remove once vec_cast() preserves names.
# https://github.com/r-lib/vctrs/issues/623
vec_cast_named <- function(x, to, ...) {
  stats::setNames(vec_cast(x, to, ...), names(x))
}

combine_labels <- function(x_labels, y_labels, x_arg, y_arg) {
  x_common <- x_labels[x_labels %in% y_labels]
  y_common <- y_labels[y_labels %in% x_labels]

  if (length(x_common) > 0) {
    x_common <- x_common[order(x_common)]
    y_common <- y_common[order(y_common)]

    problems <- x_common[names(x_common) != names(y_common)]
    if (length(problems) > 0) {
      problems <- cli::cli_vec(problems, list(vec_trunc = 10))

      cli::cli_warn(c(
        "{.var {x_arg}} and {.var {y_arg}} have conflicting value labels.",
        i = "Labels for these values will be taken from {.var {x_arg}}.",
        x = "Values: {.val {problems}}"
      ))
    }
  }

  c(x_labels, y_labels[!y_labels %in% x_labels])
}
