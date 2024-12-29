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

purse <- \(x,
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

replace_open_columns <- \(x) stringr::str_replace_all(x, c(":" = "_", "%" = "", "@" = ""))
remove_at_symbol     <- \(x) fuimus::sf_remove(s = x, p = "@", fix = TRUE)
flatten_column       <- \(i) purrr::map_chr(i, \(x) paste0(fuimus::delist(x), collapse = ", "))

clean_open_description <- \(x) {
  stringr::str_replace_all(
    x,
    c("\n" = ". ",
      "<p><strong>NOTE: </strong>This is a very large file and, depending on your network characteristics and software, may take a long time to download or fail to download. Additionally, the number of rows in the file may be larger than the maximum rows your version of <a href=\"https://support.microsoft.com/en-us/office/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3\">Microsoft Excel</a> supports. If you can't download the file, we recommend engaging your IT support staff. If you are able to download the file but are unable to open it in MS Excel or get a message that the data has been truncated, we recommend trying alternative programs such as MS Access, Universal Viewer, Editpad or any other software your organization has available for large datasets.</p>" = ""
    ))
}

recode_iso_8601 <- \(x) {
  kit::nswitch(
    x,
    "R/P10Y",   "Decennial",
    "R/P4Y",    "Quadrennial",
    "R/P1Y",    "Annual",
    "R/P0.5M",  "Bimonthly",
    "R/P2M",    "Bimonthly",
    "R/P0.5W",  "Biweekly",
    "R/P2W",    "Biweekly",
    "R/P3.5D",  "Semiweekly",
    "R/P1D",    "Daily",
    "R/P6M",    "Semiannual",
    "R/P2Y",    "Biennial",
    "R/P3Y",    "Triennial",
    "R/P0.33W", "Three Times a Week",
    "R/P0.33M", "Three Times a Month",
    "R/PT1S",   "Continuously Updated",
    "R/P1M",    "Monthly",
    "R/P3M",    "Quarterly",
    "R/P4M",    "Three Times a Year",
    "R/P1W",    "Weekly",
    "R/PT1H",   "Hourly")
}

main_catalog <- \() {

  x <- arrow::read_json_arrow(
    file = "https://data.cms.gov/data.json",
    col_select = c("@context",
                   "@id",
                   "@type",
                   "conformsTo",
                   "describedBy"),
    as_data_frame = FALSE) |>
    dplyr::collect()

  list(
    context     = x$`@context`,
    id          = x$`@id`,
    type        = x$`@type`,
    conformsTo  = x$conformsTo,
    describedBy = x$describedBy)
}

main_data_arrow <- \() {

  x <- arrow::read_json_arrow(
    file          = "https://data.cms.gov/data.json",
    col_select    = c("dataset"),
    as_data_frame = TRUE) |>
    arrow::to_duckdb() |>
    dplyr::collect()

  collapse::qTBL(x[["dataset"]][[1]]) |>
    collapse::fmutate(
      bureauCode   = delist(bureauCode),
      language     = delist(language),
      programCode  = delist(programCode),
      references   = delist(references),
      theme        = flatten_column(theme),
      keyword      = flatten_column(keyword)) |>
    collapse::frename(remove_at_symbol)
}

main_data_rcpp <- \() {

  x <- RcppSimdJson::fload("https://data.cms.gov/data.json")

  dataset <- collapse::qTBL(x[["dataset"]]) |>
    collapse::fmutate(
      bureauCode   = delist(bureauCode),
      language     = delist(language),
      programCode  = delist(programCode),
      references   = delist(references),
      theme        = flatten_column(theme),
      keyword      = flatten_column(keyword)) |>
    collapse::frename(remove_at_symbol)

  distro <- collapse::fselect(dataset, distribution) |>
    tidyr::unnest(distribution) |>
    collapse::frename(remove_at_symbol)

  list(
    context             = x[["@context"]],
    id                  = x[["@id"]],
    type                = x[["@type"]],
    conformsTo          = x[["conformsTo"]],
    describedBy         = x[["describedBy"]],
    dataset             = collapse::fselect(dataset, -distribution) |> remove_all_na(),
    distribution_latest = collapse::fsubset(distro, description %==% "latest") |> remove_all_na(),
    distribution_api    = collapse::fsubset(distro, not_na(format) & na(description)) |> remove_all_na(),
    distribution_csv    = collapse::fsubset(distro, mediaType %==% "text/csv") |> remove_all_na()
  )
}

provider_data <- \() {

  httr2::request(
    "https://data.cms.gov/provider-data/api/1/metastore/schemas/dataset/items"
  ) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    dplyr::tibble() |>
    tidyr::unnest_wider(contactPoint, names_sep = "_") |>
    tidyr::unnest_wider(publisher, names_sep = "_") |>
    tidyr::unnest_wider(distribution, names_sep = "_") |>
    dplyr::mutate(
      bureauCode  = delist(bureauCode),
      programCode = delist(programCode),
      keyword     = flatten_column(keyword),
      theme       = flatten_column(theme)) |>
    dplyr::rename_with(remove_at_symbol) |>
    fuimus::remove_all_na()

}

offset_sequence <- \(rows, size = 5000) if (rows <= size) return(rows) else 0:round(rows / size) * size

options(scipen = 999, digits = 3)

library(collapse)
library(tidyverse)
library(provider)
library(fuimus)
library(httr2)
library(arrow)
library(here)
library(S7)
