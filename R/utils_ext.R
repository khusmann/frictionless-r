#' Standardize channel names / suffixes from channel options
#'
#' @param channels A character vector representing the data channels to load.
#'   Available channels are `"values"` and `"missing"`. Use named vectors to
#'   control column suffixes, e.g.
#'   `c(values = "_values", missing = "_missing")`.
#' @return A named list. Names are channel names, values are suffixes.
#' @family helper functions
#' @noRd
channel_opt_standardize <- function(channels) {
  default_suffixes <- c(
    values = "_values",
    missing = "_missing"
  )

  if (is.null(names(channels))) {
    names(channels) <- rlang::rep_along(channels, "")
  }

  if (length(channels) == 1 && names(channels) == "") {
    channel_suffixes <- ""
  } else {
    channel_suffixes <- purrr::map2_vec(
      channels,
      names(channels),
      \(x, n) if (n == "") { default_suffixes[[x]] } else { x }
    )
  }

  channel_names <- purrr::map2_vec(
    channels,
    names(channels),
    \(x, n) dplyr::if_else(n == "", x, n)
  )

  purrr::set_names(channel_suffixes, channel_names)
}

#' Proposed extension of readr::read_delim
#'
#' Adds a `channels` argument that facilitates the loading of missing values
#'
#'
#' @inheritParams readr::read_delim
#' @param channels A character vector representing the data channels to load.
#'   Available channels are `"values"` and `"missing"`. Use named vectors to
#'   control column suffixes, e.g.
#'   `c(values = "_values", missing = "_missing")`.
#' @return A `tibble()` of loaded values.
#' @family helper functions
#' @noRd
read_delim_ext <- function(file, delim, na = c("", "NA"), col_types = NULL,
                           col_select = NULL, channels = "values", ...) {

  channels <- channel_opt_standardize(channels)

  # Only include col types of selected cols (to avoid warning)
  if (is.null(col_select)) {
    selected_col_types <- col_types
  } else {
     selected_col_types <- col_types[tidyselect::eval_select(
      rlang::enquo(col_select),
      col_types,
    )]
  }

  readr_cols <- purrr::map(
    selected_col_types,
    function(c) {
      if (is.collector(c)) {
        c
      } else {
        c$data_type
      }
    }
  )

  label_fn <- purrr::map(
    selected_col_types,
    function(c) {
      if (is.collector(c)) {
        \(x) x
      } else {
        \(x) labelled_ext(x, c$labels, c$label, c$levels, c$ordered)
      }
    }
  )

  missing_label_fn <- purrr::map(
    selected_col_types,
    function(c) {
      if (is.collector(c)) {
        \(x) x
      } else {
        \(x) haven::labelled(x, c$missing_labels, c$label)
      }
    }
  )

  string_df <- readr::read_delim(
    file,
    delim,
    col_select = {{col_select}},
    col_types = readr::cols(.default=readr::col_character()),
    ...
  )

  result <- NULL

  if ("values" %in% names(channels)) {
    # Commenting out because https://github.com/tidyverse/readr/issues/1526
    #values_df <- string_df |>
    #  readr::type_convert(
    #    col_types=readr_cols,
    #    na=na,
    #  ) |>
    #  dplyr::rename_with(\(x) paste0(x, channels[["values"]]))

    # Until the issue is fixed, let's just re-read the csv...
    values_df <- readr::read_delim(
        file,
        delim,
        col_types=readr_cols,
        col_select = {{col_select}},
        na=na,
        ...
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          \(x) label_fn[[dplyr::cur_column()]](x)
        )
      ) |>
      dplyr::rename_with(\(x) paste0(x, channels[["values"]]))

    result <- dplyr::bind_cols(result, values_df)
  }

  if ("missing" %in% names(channels)) {
    missing_df <- string_df |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          \(x) dplyr::if_else(x %in% na, x, NA_character_)
        )
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          \(x) missing_label_fn[[dplyr::cur_column()]](x)
        )
      ) |>
      dplyr::rename_with(\(x) paste0(x, channels[["missing"]]))

    result <- dplyr::bind_cols(result, missing_df)
  }
  result
}

is.collector <- function(x) inherits(x, "collector")
