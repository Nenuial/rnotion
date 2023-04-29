#' Get page block
#'
#' @param id Page id
#'
#' @return A list of blocks
#' @export
rni_get_page_blocks <- function(id) {
  rni_api_client()$blocks$children$list(block_id = id)$results
}

#' Recursively render a block
#'
#' @param id The block id string
#'
#' @return A markdown string
#' @export
rni_render_blocks <- function(id, render_level = 0) {
  markdown <- ""

  blocks <- rni_api_client()$blocks$children$list(block_id = id)

  blocks$results |>
    purrr::map_chr(~rni_parse_block(.x, render_level)) |>
    paste0(collapse = "") -> markdown

  return(markdown)
}

#' Parse notion blocks
#'
#' @param block A notion block object
#'
#' @return A string
#' @export
rni_parse_block <- function(block, render_level) {
  markdown <- ""

  if(block$has_children) {
    render_level <- render_level + 1
    rni_render_blocks(block$id, render_level) |>
      pander::pandoc.indent(level = render_level) -> markdown
  }

  paste0(rni_to_md(block), markdown)
}

#' Format notion block into markdown
#'
#' @param block A notion block object
#'
#' @return A string
#' @export
rni_to_md <- function(block) {
  markdown <- ""

  if(stringr::str_detect(block$type, "heading_")) {
    header_level <- as.integer(stringr::str_match(block$type, "heading_(\\d{1})")[,2])
    paste0(
      pander::pandoc.header.return(
        rni_text(block),
        level = header_level
      ),
      "\n"
    ) -> markdown
  }

  if(block$type == "number") {
    block[[block$type]] -> markdown
  }

  if(block$type == "paragraph") {
    pander::pandoc.p.return(
      rni_text(block)
    ) -> markdown
  }

  if(block$type == "title") {
    rni_rich_text(block) |>
      paste0(collapse = "") -> markdown
  }

  if(block$type == "rich_text") {
    rni_rich_text(block) |>
      paste0(collapse = "") -> markdown
  }

  if(block$type == "select") {
    block$select$name -> markdown
  }

  if(block$type == "multi_select") {
    block$multi_select |> purrr::map(~purrr::pluck(.x, "name")) |> purrr::list_c() |> stringr::str_c(collapse = ", ") -> markdown
  }

  if(block$type == "phone_number") {
    block$phone_number -> markdown
  }

  if(block$type == "email") {
    block$email -> markdown
  }

  if(block$type == "image") {
    url <- block$image$file$url
    fs::dir_create("_images")
    dest <- fs::file_temp(tmp_dir = "_images", ext = stringr::str_match(url, ".*/([^?]*)")[,2])

    download.file(url, dest)

    pander::pandoc.image.return(
      dest
    ) -> markdown
  }

  if(block$type == "code") {
    rni_code(block) -> markdown
  }

  if(block$type == "date") {
    rni_date(block) -> markdown
  }

  if(block$type == "bulleted_list_item") {
    pander::pandoc.list.return(
      rni_text(block),
      style = "bullet",
      add.end.of.list = F
    ) -> markdown
  }

  if(block$type == "numbered_list_item") {
    pander::pandoc.list.return(
      rni_text(block),
      style = "ordered",
      add.end.of.list = F
    ) -> markdown
  }

  return(markdown)
}

#' Apply text formatting
#'
#' @param block A notion block object
#'
#' @return A vector of strings
#' @keywords internal
rni_text <- function(block) {
  block[[block$type]]$rich_text |>
    purrr::map_chr(rni_annotate_text) |>
    paste(sep = "", collapse = "")
}

#' Apply text formatting for rich text properties
#'
#' @param block A notion block object
#'
#' @return A vector of strings
#' @keywords internal
rni_rich_text <- function(block) {
  block[[block$type]] |>
    purrr::map_chr(rni_annotate_text)
}

#' Format code blocks
#'
#' @param block A notion block object
#'
#' @return A string
#' @keywords internal
rni_code <- function(block) {
  block$code$text[[1]]$text$content |>
    stringr::str_split("\n") |>
    purrr::pluck(1) |>
    knitr:::parse_block(
      params.src = "r",
      header = "",
      markdown_mode = T
    ) |>
    knitr:::call_block()
}

#' Transform date properties
#'
#' @param block A notion block object
#'
#' @return A lubridate interval
#' @keywords internal
rni_date <- function(block) {
  if(is.null(block$date$end)) {
    lubridate::interval(
      block$date$start,
      block$date$start
    )
  } else {
    lubridate::interval(
      block$date$start,
      block$date$end
    )
  }
}

#' Format notion text
#'
#' @param text A notion text object
#'
#' @return A formated string
#' @keywords internal
rni_annotate_text <- function(text) {
  plain_text <- text$plain_text

  if(text$annotations$italic)
    plain_text <- paste0("*", plain_text, "*")

  if(text$annotations$strikethrough)
    plain_text <- paste0("~~", plain_text, "~~")

  if(text$annotations$bold)
    plain_text <- paste0("**", plain_text, "**")

  if(text$annotations$code)
    plain_text <- paste0("`", plain_text, "`")

  if(text$annotations$underline)
    plain_text <- paste0("[", plain_text, "]{.underline}")

  if(!is.null(text$text$link)) {
    plain_text <- paste0("[", plain_text, "](", text$text$link$url, ")")
  }

  return(plain_text)
}


