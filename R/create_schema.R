#' Create a Table Schema for a data frame
#'
#' Creates a [Table Schema](https://specs.frictionlessdata.io/table-schema/) for
#' a data frame, listing all column names and types as field names and
#' (converted) types.
#'
#' @param data A data frame.
#' @return List describing a Table Schema.
#' @family create functions
#' @export
#' @section Table schema properties:
#' The Table Schema will be created from the data frame columns:
#'
#' - `name`: contains the column name.
#' - `title`: not set.
#' - `description`: not set.
#' - `type`: contains the converted column type (see further).
#' - `format`: not set and can thus be considered `default`.
#'   This is also the case for dates, times and datetimes, since
#'   [readr::write_csv()] used by [write_package()] will format those to ISO8601
#'   which is considered the default.
#'   Datetimes in local or non-UTC timezones will be converted to UTC before
#'   writing.
#' - `constraints`: not set, except for factors (see further).
#' - `missingValues`: not set.
#'   [write_package()] will use the default `""` for missing values.
#' - `primaryKey`: not set.
#' - `foreignKeys`: not set.
#'
#' ## Field types
#'
#' The column type will determine the field `type`, as follows:
#'
#' - `character` as
#'   [string](https://specs.frictionlessdata.io/table-schema/#string).
#' - `Date` as [date](https://specs.frictionlessdata.io/table-schema/#date).
#' - `difftime` as
#'   [number](https://specs.frictionlessdata.io/table-schema/#number).
#' - `factor` as
#'   [string](https://specs.frictionlessdata.io/table-schema/#string) with
#'   factor levels as `enum`.
#' - [hms::hms()] as
#'   [time](https://specs.frictionlessdata.io/table-schema/#time).
#' - `integer` as
#'   [integer](https://specs.frictionlessdata.io/table-schema/#integer).
#' - `logical` as.
#'   [boolean](https://specs.frictionlessdata.io/table-schema/#boolean).
#' - `numeric` as
#'   [number](https://specs.frictionlessdata.io/table-schema/#number).
#' - `POSIXct`/`POSIXlt` as
#'   [datetime](https://specs.frictionlessdata.io/table-schema/#datetime).
#' - Any other type as
#'   [any](https://specs.frictionlessdata.io/table-schema/#any).
#' @examples
#' # Create a data frame
#' df <- data.frame(
#'   id = c(as.integer(1), as.integer(2)),
#'   timestamp = c(
#'     as.POSIXct("2020-03-01 12:00:00", tz = "EET"),
#'     as.POSIXct("2020-03-01 18:45:00", tz = "EET")
#'   ),
#'   life_stage = factor(c("adult", "adult"), levels = c("adult", "juvenile"))
#' )
#'
#' # Create a Table Schema from the data frame
#' schema <- create_schema(df)
#' str(schema)
create_schema <- function(data) {
  # Check data
  check_data(data)

  # Columns with all NA are considered logical by R (and read_delim)
  # Set those to character, since string is a better default for Table Schema
  data_as_list <- lapply(data, function(x) {
    if (is.logical(x) && all(is.na(x))) {
      as.character(x)
    } else {
      x
    }
  })

  # Create fields (a list of lists)
  fields <- purrr::imap(data_as_list, function(x, name) {
    # Name
    name <- if (is.na(name)) "" else name

    # Type
    type <- paste(class(x), collapse = ",") # When data type is a vector
    type <- dplyr::recode(type,
      "character" = "string",
      "Date" = "date",
      "difftime" = "number",
      "factor" = "string",
      "hms,difftime" = "time", # Data read using col_time()
      "integer" = "integer",
      "logical" = "boolean",
      "numeric" = "number", # Includes double
      "POSIXct,POSIXt" = "datetime", # Includes POSIXlt,POSIXt
      .default = "any"
    )

    # Enumeration
    enum <- levels(x)

    # Create field list
    list(
      name = name,
      type = type,
      constraints = list(
        enum = enum
      )
    )
  })

  # Create schema
  schema <- list(
    fields = unname(fields) # Creates [] rather than {}
  )

  # Remove elements that are NULL or empty list
  schema <- clean_list(
    schema,
    function(x) is.null(x) || length(x) == 0L,
    recursive = TRUE
  )

  return(schema)
}
