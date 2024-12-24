knitr::opts_chunk$set(
  dev        = "ragg_png",
  dpi        = 320,
  out.width  = "100%",
  fig.width  = 8,
  fig.asp    = 0.818,
  fig.retina = 2,
  fig.align  = "center",
  fig.show   = "hold"
)

btn_link <- function(href, label) {

  htmltools::tags$a(
    href = href,
    class = "btn btn-outline-secondary",
    role = "button",
    target = "_blank",
    htmltools::tags$i(class = "bi bi-box-arrow-up-right"),
    label
  )
}

purse <- function(x,
                  pre = "- ",
                  wid = 0,
                  sep = " ") {
  terse::terse(
    x = x,
    prefix = pre,
    width = wid,
    config = list(
      gsep = sep,
      ansi = FALSE))
}

options(scipen = 999, digits = 3)

library(tidyverse)
library(provider)
library(fuimus)
library(httr2)
library(arrow)
