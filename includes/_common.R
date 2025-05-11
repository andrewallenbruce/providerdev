
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

time <- c("modified", "periodicity", "temporal")
link <- c("dictionary", "site", "references", "resources", "download")

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
