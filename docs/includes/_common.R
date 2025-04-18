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

# @export
# This will be exported by S7 next release.
`:=` <- function(left, right) {
  name <- substitute(left)
  if (!is.symbol(name))
    stop("left hand side must be a symbol")

  right <- substitute(right)
  if (!is.call(right))
    stop("right hand side must be a call")

  if (is.symbol(cl <- right[[1L]]) &&
      as.character(cl) %in% c("function", "new.env")) {
    # attach "name" attr for usage like:
    # foo := function(){}
    # foo := new.env()
    right <- eval(right, parent.frame())
    attr(right, "name") <- as.character(name)
  } else {
    # for all other usage,
    # inject name as a named arg, so that
    #   foo := new_class(...)
    # becomes
    #   foo <- new_class(..., name = "foo")

    right <- as.call(c(as.list(right), list(name = as.character(name))))

    ## skip check; if duplicate 'name' arg is an issue the call itself will signal an error.
    # if (hasName(right, "name")) stop("duplicate `name` argument.")

    ## alternative code path that injects `name` as positional arg instead
    # right <- as.list(right)
    # right <- as.call(c(right[[1L]], as.character(name), right[-1L]))
  }

  eval(call("<-", name, right), parent.frame())
}
