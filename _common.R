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

btn_link <- \(href, label) {

  htmltools::tags$a(
    href = href,
    class = "btn btn-outline-secondary",
    role = "button",
    target = "_blank",
    htmltools::tags$i(class = "bi bi-box-arrow-up-right"),
    label
  )
}

purse <- \(x, pre = "- ", wid = 0, sep = " ") {
  terse::terse(
    x = x,
    prefix = pre,
    width = wid,
    config = list(
      gsep = sep,
      ansi = FALSE))
}

replace_open_columns <- \(x) stringr::str_replace_all(x, c(":" = "_", "%" = "", "@" = ""))
remove_at_symbol     <- \(x) fuimus::sf_remove(s = x, p = "@", fix = TRUE)
flatten_column       <- \(column) purrr::map_chr(column, \(x) paste0(fuimus::delist(x), collapse = ", "))

clean_open_description <- \(x) {
  stringr::str_replace_all(
    x,
    c("\n" = ". ",
      "<p><strong>NOTE: </strong>This is a very large file and, depending on your network characteristics and software, may take a long time to download or fail to download. Additionally, the number of rows in the file may be larger than the maximum rows your version of <a href=\"https://support.microsoft.com/en-us/office/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3\">Microsoft Excel</a> supports. If you can't download the file, we recommend engaging your IT support staff. If you are able to download the file but are unable to open it in MS Excel or get a message that the data has been truncated, we recommend trying alternative programs such as MS Access, Universal Viewer, Editpad or any other software your organization has available for large datasets.</p>" = ""
    ))
}

options(scipen = 999, digits = 3)

library(tidyverse)
library(provider)
library(fuimus)
library(httr2)
library(arrow)
