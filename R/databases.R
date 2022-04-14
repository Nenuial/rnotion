#' Get all elements of a database
#'
#' @param id The database id
#' @param sort_params A list of sort parameters
#' @param cursor An optional cursor for multipage requests
#'
#' @return A list of properties
#' @export
rni_get_database <- function(id, cursor = NULL, ...) {
  if(is.null(cursor)) {
    rni_api_client()$databases$query(
      database_id = id,
      ...
    ) -> result
  } else {
    rni_api_client()$databases$query(
      database_id = id,
      start_cursor = cursor,
      ...
    ) -> result
  }

  if(result$has_more) {
    rni_get_database(id, cursor = result$next_cursor, ...) -> result_more
    c(result$results, result_more)
  } else {
    result$results
  }
}

#' Format a notion database as tibble
#'
#' @param db A list of notion pages
#'
#' @return A tibble
#' @export
rni_properties_tibble <- function(db) {
  db |>
    purrr::map_df(rni_page_properties) |>
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~stringr::str_replace_all(.x, r"(^\n\n|\n\n$)","")
      )
    )
}
