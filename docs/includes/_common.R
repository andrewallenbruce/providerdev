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

library(listviewerlite)

# library(curl)
# library(weburl)
# library(urlparse)

yank  <- \(x) x[[1]]
purse <- \(x, pre = "- ", wid = 0, max = 20, sep = " ") terse::terse(x = x, prefix = pre, width = wid, max_vec_len = max, config = list(gsep = sep, ansi = FALSE))
remove_at_symbol <- \(x) sf_remove(s = x, p = "@", fix = TRUE)

npi_kind <- c(
  1225701881,
  1174270805,
  1235702796,
  1962116806,
  1013647569,
  1306500665,
  1982296737,
  1083295638,
  1841967825,
  1891390084,
  1275117269,
  1992338701,
  1891355863,
  1548743511,
  1023473279,
  1861857013,
  1689182859,
  1982059275
)

# btn_link <- \(href, label) {
#
#   htmltools::tags$a(
#     href                    = href,
#     class                   = "btn btn-outline-secondary",
#     role                    = "button",
#     target                  = "_blank",
#     htmltools::tags$i(class = "bi bi-box-arrow-up-right"),
#     label
#   )
# }


# main_data_arrow <- \() {
#
#   x <- arrow::read_json_arrow(
#     file          = "https://data.cms.gov/data.json",
#     col_select    = c("dataset"),
#     as_data_frame = TRUE) |>
#     arrow::to_duckdb() |>
#     dplyr::collect()
#
#   collapse::qTBL(x[["dataset"]][[1]]) |>
#     collapse::fmutate(
#       bureauCode   = delist(bureauCode),
#       language     = delist(language),
#       programCode  = delist(programCode),
#       references   = delist(references),
#       theme        = flatten_column(theme),
#       keyword      = flatten_column(keyword)) |>
#     collapse::frename(remove_at_symbol)
# }

# main_data_rcpp <- \() {
#
#   x <- RcppSimdJson::fload("https://data.cms.gov/data.json")
#
#   dataset <- collapse::qTBL(x[["dataset"]]) |>
#     collapse::fmutate(
#       bureauCode   = delist(bureauCode),
#       language     = delist(language),
#       programCode  = delist(programCode),
#       references   = delist(references),
#       theme        = flatten_column(theme),
#       keyword      = flatten_column(keyword)) |>
#     collapse::frename(remove_at_symbol)
#
#   distro <- collapse::fselect(dataset, distribution) |>
#     tidyr::unnest(distribution) |>
#     collapse::frename(remove_at_symbol)
#
#   list(
#     context             = x[["@context"]],
#     id                  = x[["@id"]],
#     type                = x[["@type"]],
#     conformsTo          = x[["conformsTo"]],
#     describedBy         = x[["describedBy"]],
#     dataset             = collapse::fselect(dataset, -distribution) |> remove_all_na(),
#     distribution_latest = collapse::fsubset(distro, description %==% "latest") |> remove_all_na(),
#     distribution_api    = collapse::fsubset(distro, not_na(format) & na(description)) |> remove_all_na(),
#     distribution_csv    = collapse::fsubset(distro, mediaType %==% "text/csv") |> remove_all_na()
#   )
# }

# provider_data <- \() {
#   httr2::request("https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items") |>
#     httr2::req_perform() |>
#     httr2::resp_body_json(simplifyVector = TRUE) |>
#     dplyr::tibble() |>
#     tidyr::unnest_wider(contactPoint, names_sep = "_") |>
#     tidyr::unnest_wider(publisher, names_sep = "_") |>
#     tidyr::unnest_wider(distribution, names_sep = "_") |>
#     dplyr::mutate(
#       bureauCode  = delist(bureauCode),
#       programCode = delist(programCode),
#       keyword     = flatten_column(keyword),
#       theme       = flatten_column(theme)) |>
#     dplyr::rename_with(remove_at_symbol) |>
#     fuimus::remove_all_na() |>
#     dplyr::filter(sf_ndetect(title, "Office Visit Costs$"))
# }
