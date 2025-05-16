#' Create a message from a data frame
#'
#' This function is especially helpful for cases when a data frame
#' of missing or unset values is at hand.
#' Trim unneeded columns, then call this function
#' to create a string with rows separated by semicolons and entries separated by commas.
#'
#' @param df The data frame to be converted to a message
#'
#' @return A string with rows separated by semicolons and entries separated by commas.
#'
#' @export
#'
#' @examples
#' data.frame(a = c(1, 2, 3), b = c("a", "b", "c")) |> df_to_msg()
df_to_msg <- function(df) {

  titles <- paste0(names(df), collapse = " ")

  rows <- lapply(1:nrow(df), function(r) {
    df[r, ] |>
      as.list() |>
      paste(collapse = " ")
    }
  ) |>
    paste(collapse = "\n")

  gridline <- rep("=", times = nchar(titles)) |> paste0(collapse = "")

  paste0(titles, "\n", gridline, "\n", rows)
}

#' Fuzzy grep, matches pattern that are close, but not identical
#' @examples
#' colnames(iris)
#' p <- sprintf("(%s){~%i}", "Spela", 2)
#' grep(pattern = p, x = colnames(iris), ignore.case = FALSE)
#' @keywords internal
#' @noRd
fuzzy_grep <- function(x, pattern, precision = NULL) {
  if (is.null(precision)) {
    precision <- round(nchar(pattern) / 3)
  }
  if (precision > nchar(pattern)) {
    return(NULL)
  }
  p <- sprintf("(%s){~%i}", pattern, precision)
  grep(pattern = p, x = x, ignore.case = FALSE)
}

colnames(iris)[fuzzy_grep(colnames(iris), pattern = "Spela")]

#' create a message string to tell user about matches that could possibly
#' be the string they were looking for
#'
#' @keywords internal
#' @noRd
.misspelled_string <- function(source, searchterm, default_message = NULL) {
  if (is.null(searchterm) || length(searchterm) < 1) {
    return(default_message)
  }
  # used for many matches
  more_found <- ""
  # init default
  msg <- ""
  # guess the misspelled string
  possible_strings <- unlist(lapply(searchterm, function(s) {
    source[.fuzzy_grep(source, s)] # nolint
  }), use.names = FALSE)
  if (length(possible_strings)) {
    msg <- "Did you mean "
    if (length(possible_strings) > 1) {
      # make sure we don't print dozens of alternatives for larger data frames
      if (length(possible_strings) > 5) {
        more_found <- sprintf(
          " We even found %i more possible matches, not shown here.",
          length(possible_strings) - 5
        )
        possible_strings <- possible_strings[1:5]
      }
      msg <- paste0(msg, "one of ", datawizard::text_concatenate(possible_strings, enclose = "\"", last = " or "))
    } else {
      msg <- paste0(msg, "\"", possible_strings, "\"")
    }
    msg <- paste0(msg, "?", more_found)
  } else {
    msg <- default_message
  }
  # no double white space
  insight::trim_ws(msg)
}


