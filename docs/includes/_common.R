
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
library(stringi)
library(tidyverse)
library(httr2)
library(glue)
library(here)
library(S7)
library(rlang)
library(gt)

library(provider)
library(providertwo)

library(arrow)
library(RcppSimdJson)

# library(curl)
# library(weburl)
# library(urlparse)

yank  <- \(x) x[[1]]

purse <- \(
  x,
  pre = "- ",
  wid = 0,
  max = 20,
  sep = " "
) {
  terse::terse(
    x = x,
    prefix = pre,
    width = wid,
    max_vec_len = max,
    config = list(gsep = sep, ansi = FALSE)
  )
}
