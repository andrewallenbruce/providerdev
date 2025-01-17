
replace_open_columns <- \(x) {
  stringr::str_replace_all(
    x,
    c(":" = "_",
      "%" = "",
      "@" = "",
      "$" = "",
      "properties_" = "pr_")
    )
  }

clean_open_description <- \(x) {
  stringr::str_replace_all(
    x,
    c("\n" = ". ",
      "<p><strong>NOTE: </strong>This is a very large file and, depending on your network characteristics and software, may take a long time to download or fail to download. Additionally, the number of rows in the file may be larger than the maximum rows your version of <a href=\"https://support.microsoft.com/en-us/office/excel-specifications-and-limits-1672b34d-7043-467e-8e27-269d656771c3\">Microsoft Excel</a> supports. If you can't download the file, we recommend engaging your IT support staff. If you are able to download the file but are unable to open it in MS Excel or get a message that the data has been truncated, we recommend trying alternative programs such as MS Access, Universal Viewer, Editpad or any other software your organization has available for large datasets.</p>" = ""
    )
  )
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
