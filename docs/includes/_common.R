
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

options(crayon.enabled = TRUE, scipen = 999, digits = 3)

library(here)
library(tidyverse)
library(RcppSimdJson)
library(collapse)
library(fastplyr)
library(cheapr)
library(clock)
library(stringi)
library(httr2)
library(glue)
library(S7)
library(rlang)
library(arrow)

library(provider)
library(providertwo)

library(gt)
library(pointblank)
library(datawizard)
library(see)
library(ggplot2)
theme_set(theme_modern())

# library(curl)
# library(weburl)
# library(urlparse)

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

purse <- \(
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
    config = list(gsep = sep, colour_nth = "lightblue", nth = 3)
  )
}

wrap <- function(string,
                 width = 80,
                 indent = 0,
                 exdent = 0,
                 whitespace_only = TRUE) {

  strc <- function(..., sep = "", collapse = NULL) {
    # rlang::check_string(sep)
    # rlang::check_string(collapse, allow_null = TRUE)
    dots <- list(...)
    dots <- dots[!purrr::map_lgl(dots, is.null)]
    vctrs::vec_size_common(!!!dots)
    rlang::inject(stringi::stri_c(!!!dots, sep = sep, collapse = collapse))
  }

  # rlang::check_number_decimal(width)
  if (width <= 0)
    width <- 1
  # rlang::check_number_whole(indent)
  # rlang::check_number_whole(exdent)
  # rlang::check_bool(whitespace_only)
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

dims   <- function(x) if (x@dimensions@pages == 1) "rows" else c("rows", "pages")
inj_ls <- function(e1, e2) list2(!!!e1, !!!e2)

print_meta <- function(x, api, ...) {

  meta <- switch(
    match.arg(api, c("care", "caid", "open", "pro", "hgov")),
    care = c("modified", "periodicity", "temporal", "dictionary", "site", "references", "resources", "download"),
    open = c("modified", "download"),
    pro = c("issued", "modified", "released", "dictionary", "site", "download")
  )

  inj_ls(
    props(x@dimensions)[dims(x)],
    end@metadata[meta]) |>
    print_ls(...)
}

print_resources <- function(x) {
  list_resources(x) |>
    glue_data("[{format(toupper(ext), justify = 'right')}] ",
              "{year} {file} ",
              "({format(size, justify = 'left')})",
              .na = cli::symbol$ellipsis)
}

print_dict_tbl <- function(x) {
  dict <- set_names(wrap(dict$description, width = 50), dict$field)
  glue_col("{red {underline {names(dict)}}}\n{silver {unname(dict)}}\n\n")
}

print_dict_list <- function(x) {
  dict <- set_names(wrap(unname(x), width = 50), names(x))
  glue_col("{red {underline {names(dict)}}}\n{silver {unname(dict)}}\n\n")
}
