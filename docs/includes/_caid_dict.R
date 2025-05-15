x <- fload("https://data.medicaid.gov/api/1/metastore/schemas/dataset/items?show-reference-ids") |>
  as_tbl() |>
  slt(distribution)

x <- rowbind(x$distribution, fill = TRUE)

x <- new_tbl(url = funique(unlist(get_elem(x$data, "describedBy")), use.names = FALSE))

caid_data_dictionary <- x$url |>
  map(request) |>
  req_perform_parallel(on_error = "continue") |>
  map(function(resp) {
      x <- resp_body_string(resp) |>
        fparse() |>
        _[["data"]]

      new_tbl(
        title = x$title,
        field = x$fields$name,
        description = providertwo:::remove_non_ascii(gsub("\r\n", " ", x$fields$description))) |>
        providertwo:::map_na_if()
  }
      )

caid_data_dictionary |>
  list_rbind() |>
  write_csv(file = "data/caid_data_dictionary.csv")
