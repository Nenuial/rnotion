sort_params <- list(list(property = "Page début", direction = "ascending"))

rni_get_database("eb53d1d3d1bd4bdaa258b7deca1a62f9", sort_params) |>
  rni_properties_tibble() -> tbl
  dplyr::select(start = `Page début`, end = `Page fin`, square = Case, notes = Notes) |>
  dplyr::mutate(notes = purrr::map_chr(notes, geovizr::gvz_md_to_latex)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), as.character)) |>
  tidyr::replace_na(list(end = "")) |>
  dplyr::rowwise() |>
  dplyr::transmute(text = stringr:::str_c(dplyr::c_across(), collapse = " & ")) |>
  dplyr::mutate(text = glue::glue("{text}\\\\")) |>
  dplyr::pull(text) |>
  stringr::str_c(collapse = "\n") |>
  cat()
