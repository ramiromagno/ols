spec_property <- function() {

  tspec_df(
    tib_chr("iri"),
    tib_chr("lang"),
    tib_variant("description"),
    tib_variant("synonyms"),
    tib_variant("annotation"),
    tib_chr("label"),
    tib_chr("ontology_name"),
    tib_chr("ontology_prefix"),
    tib_chr("ontology_iri"),
    tib_lgl("is_obsolete"),
    tib_lgl("is_defining_ontology"),
    tib_lgl("has_children"),
    tib_lgl("is_root"),
    tib_chr("short_form"),
    tib_chr("obo_id"),
    tib_df(
      "_links",
      .names_to = ".names",
      tib_chr("href"),
    ),
  )
}

#' @importFrom rlang .data
get_properties <- function(property_id = NULL) {

  json <-
    if (is.null(property_id)) {
      json <- get("/api/properties/")
      lst <- flatten(json, "properties", embedded = TRUE)
    } else {
      #json <- get("/api/properties/{property_id}", property_id = property_id)
      #lst <- flatten(json, embedded = FALSE)
      stop("`property_id` not implemented")
    }

  tbl01 <- tibblify(lst, spec_property())

  tbl02 <- tbl01
  colnames(tbl02) <- snakecase::to_snake_case(colnames(tbl01))

  # annotation01 <-
  #   tbl02 |>
  #   dplyr::select(c("obo_id", "annotation")) |>
  #   tidyr::unnest("annotation")
  #
  # annotation02 <- annotation01
  # colnames(annotation02) <- snakecase::to_snake_case(colnames(annotation01))

  tbl02

}
