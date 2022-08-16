#' Get propoerties from a page
#'
#' @param id A notion page id
#'
#' @return A list
#' @export
rni_get_page_properties <- function(id) {
  rni_api_client()$pages$retrieve(id)$properties
}

#' Process page properties
#'
#' @param page A notion page object
#'
#' @return A string
#' @keywords internal
rni_page_properties <- function(page) {
  page$properties |> purrr::map_dfr(rni_to_md)
}

#' Insert a page into a database or page
#'
#' @param parent A list with type (`database_id` or `page_id`) and the corresponding id
#' @param properties A list of properties
#' @param ...
#'
#' @return Nothing
#' @export
rni_add_page <- function(parent, properties, ...) {
  rni_api_client()$pages$create(
    parent = parent,
    properties = properties,
    ...
  )
}


