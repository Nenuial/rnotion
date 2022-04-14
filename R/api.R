#' Get API Client from notion-sdk-py
#'
#' @return A client object
#' @export
rni_api_client <- function() {
  notion <- reticulate::import("notion_client")

  notion$Client(auth = Sys.getenv("NOTION_API"))
}
