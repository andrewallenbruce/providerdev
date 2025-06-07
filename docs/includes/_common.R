
# https://blog.djnavarro.net/posts/2021-04-18_pretty-little-clis/

ansi_aware_handler <- function(x, options) {
  paste0(
    "<pre class=\"r-output\"><code>",
    fansi::sgr_to_html(x = x, warn = FALSE, term.cap = "256"),
    "</code></pre>"
  )
}

knitr::knit_hooks$set(
  output  = ansi_aware_handler,
  message = ansi_aware_handler,
  warning = ansi_aware_handler,
  error   = ansi_aware_handler
)

options(
  digits         = 3,
  width          = 80,
  str            = strOptions(strict.width = "cut"),
  crayon.enabled = TRUE,
  scipen         = 999
)
knitr::opts_chunk$set(
  comment   = "",
  collapse  = TRUE,
  dev       = "ragg_png",
  dpi       = 300,
  out.width = "70%",
  fig.width = 6,
  fig.align = 'center',
  fig.asp   = 0.618  # 1 / phi
  )

library(here)
library(crayon)
library(tidyverse)
library(RcppSimdJson)
library(rlang)
library(collapse)
library(fastplyr)
library(cheapr)
library(clock)
library(stringi)
library(httr2)
library(glue)
library(S7)
library(arrow)

library(provider)
library(providertwo)

library(datawizard)
library(see)
# theme_set(theme_modern())

# library(gt)
# library(pointblank)
# library(curl)
# library(weburl)
# library(urlparse)

out2fig <- function(out_width = 0.95, out_width_default = 0.7, fig_width_default = 6) {
  fig_width_default * out_width / out_width_default
}

browse_link <- function(x, link) {
  link <- match.arg(link, c("dictionary", "site", "references"))

  switch(
    link,
    dictionary = browseURL(x@metadata$dictionary),
    site       = browseURL(x@metadata$site),
    references = browseURL(x@metadata$references),
    cli::cli_abort(c("x" = "Invalid argument: {x}"))
  )
}

unscore <- function(x) gsub("___owner$", "", x, perl = TRUE)
charbin <- function(x) val_match(x, "N" ~ 0L, "Y" ~ 1L, "No" ~ 0L, "Yes" ~ 1L) |> as.integer()
as_prop <- function(x) case(x == "0" ~ 0, is_na(x) ~ NA_real_, .default = as.double(x) / 100)

purse <- function(
  x,
  pre = paste0(cli::symbol$bullet, " "),
  wid = 0,
  max = 20,
  sep = " "
) {
  terse::terse(
    x = x,
    prefix = pre,
    width = wid,
    max_vec_len = max,
    config = list(gsep = sep
                  # colour_nth = "lightblue",
                  # nth = 3
                  )
  )
}

wrap <- function(string,
                 width = 80,
                 indent = 0,
                 exdent = 0,
                 whitespace_only = TRUE) {

  strc <- function(..., sep = "", collapse = NULL) {
    providertwo:::check_string(sep)
    providertwo:::check_string(collapse, allow_null = TRUE)
    dots <- list(...)
    dots <- dots[!purrr::map_lgl(dots, is.null)]
    vctrs::vec_size_common(!!!dots)
    rlang::inject(stringi::stri_c(!!!dots, sep = sep, collapse = collapse))
  }

  providertwo:::check_number_decimal(width)
  if (width <= 0) width <- 1
  providertwo:::check_number_whole(indent)
  providertwo:::check_number_whole(exdent)
  providertwo:::check_bool(whitespace_only)
  out <- stringi::stri_wrap(
    string,
    width = width,
    indent = indent,
    exdent = exdent,
    whitespace_only = whitespace_only,
    simplify = FALSE
  )
  vapply(out, strc, collapse = "\n", character(1))
}


print_ls <- function(ls, prefix = "", postfix = "") {

  if (length(ls) == 0) cat("<empty>\n")
  if (length(names(ls)) != length(ls)) stop("all elements must be named")

  ls <- lapply(ls, as.character)
  cat(
    sprintf(
      "%s%s%s %s ",
      prefix,
      format(paste0("[", toupper(names(ls)), "]"), justify = "right"),
      postfix,
      ls),
    sep = "\n")

  invisible(ls)
}

print_meta <- function(x, ...) {

  dims     <- function(x) if (x@dimensions@pages == 1) "rows" else c("rows", "pages")
  inj_list <- function(e1, e2) list2(!!!e1, !!!e2)

  meta <- switch(
    class(x)[1],
    care_endpoint = c("modified", "periodicity", "temporal", "dictionary", "site", "references", "resources", "download"),
    caid_endpoint = c("modified", "download", "dictionary"),
    open_endpoint = c("modified", "download"),
    pro_endpoint  = c("issued", "modified", "released", "dictionary", "site", "download"),
    hgov_endpoint = c("issued", "modified", "periodicity", "download")
  )

  inj_list(c(props(x@dimensions)[dims(x)], fields = fnobs(prop(
    x@dimensions, "fields"
  ) |> names())), end@metadata[meta]) |>
    print_ls(...)
}

print_resources <- function(x) {
  list_resources(x) |>
    glue_data_col(
      "[{green {format(toupper(ext), justify = 'right')}}] ",
      "{red {format(str_squish(size), justify = 'right')}} ",
      "{year} {bold {blue {file}}}\n ",
      # "<{underline {silver {download}}}>",
      .na = cli::symbol$menu
    )
}

print_dict <- function(x, type = "tbl") {

  dict <- switch(
    type,
    lst = set_names(unname(x), names(x)),
    tbl = set_names(x$description, x$field)
  )

  glue_col(
    "{silver {format(seq_along(names(dict)), justify = 'right')}} ",
    "{bold {red {underline {names(dict)}}}}: ",
    "{silver {unname(dict)}}\n\n"
    )
}

print_dict_tbl <- function(x, w = 50) {
  dict <- set_names(wrap(x$description, width = w), x$field)
  glue_col("{bold {red {underline {names(dict)}}}}\n{silver {unname(dict)}}\n\n")
}

print_dict_list <- function(x, w = 50) {
  dict <- set_names(wrap(unname(x), width = w), names(x))
  glue_col("{bold {red {underline {names(dict)}}}}\n{silver {unname(dict)}}\n\n")
}

caid_dictionary <- function(nm = NULL) {
  if (is.null(nm)) return(read_csv(here("data/caid_data_dictionary.csv"), show_col_types = FALSE))

  read_csv(here("data/caid_data_dictionary.csv"), show_col_types = FALSE) |>
    providertwo:::subset_detect(j = title, p = nm)
}
