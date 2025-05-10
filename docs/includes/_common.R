
# https://blog.djnavarro.net/posts/2021-04-18_pretty-little-clis/

ansi_aware_handler <- function(x, options) {
  paste0(
    "<pre class=\"r-output\"><code>",
    fansi::sgr_to_html(x = x, warn = FALSE, term.cap = "256"),
    "</code></pre>"
  )
}

knitr::knit_hooks$set(
  output = ansi_aware_handler,
  message = ansi_aware_handler,
  warning = ansi_aware_handler,
  error = ansi_aware_handler
)

options(crayon.enabled = TRUE)
options(scipen = 999, digits = 3)

library(collapse)
library(fastplyr)
library(cheapr)
library(clock)
library(stringi)
library(tidyverse)
library(httr2)
library(glue)
library(here)
library(S7)
library(rlang)
library(gt)
library(pointblank)

library(provider)
library(providertwo)

library(arrow)
library(RcppSimdJson)

# library(curl)
# library(weburl)
# library(urlparse)

# yank  <- \(x) x[[1]]

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

print_ls <- function(ls, prefix = "", postfix = " | ") {

  if (length(ls) == 0) cat("<empty>\n")
  if (length(names(ls)) != length(ls)) stop("all elements must be named")

  ls <- lapply(ls, as.character)
  cat(
    sprintf(
      "%s%s%s< %s >",
      prefix,
      format(names(ls), justify = "right"),
      postfix,
      ls),
    sep = "\n")

  invisible(ls)
}
