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
purse <- \(x, pre = "- ", wid = 0, max = 20, sep = " ") terse::terse(x = x, prefix = pre, width = wid, max_vec_len = max, config = list(gsep = sep, ansi = FALSE))

