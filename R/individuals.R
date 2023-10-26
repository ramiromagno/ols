spec_individual <- function() {

  tspec_df(
    tib_chr("iri"),
    tib_chr("lang"),
    tib_variant("description"),
    tib_variant("synonyms"),
    tib_row(
      "annotation",
      tib_variant("has_obo_namespace"),
      tib_variant("id"),
      tib_variant("has_alternative_id", required = FALSE),
    ),
    tib_chr("type"),
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
    tib_chr("in_subset"),
    tib_df(
      "_links",
      .names_to = ".names",
      tib_chr("href"),
    ),
  )
}

#' @importFrom rlang .data
get_individuals <- function(individual_id = NULL) {

  json <-
    if (is.null(individual_id)) {
      json <- get("/api/individuals/")
      lst <- flatten(json, "individuals", embedded = TRUE)
    } else {
      #json <- get("/api/individuals/{individual_id}", individual_id = individual_id)
      #lst <- flatten(json, embedded = FALSE)
      stop("`individual_id` not implemented")
    }

  tbl01 <- tibblify(lst, spec_individual())

  tbl02 <- tbl01
  colnames(tbl02) <- snakecase::to_snake_case(colnames(tbl01))

  annotation01 <-
    tbl02 |>
    dplyr::select(c("obo_id", "annotation")) |>
    tidyr::unnest("annotation")

  annotation02 <- annotation01
  colnames(annotation02) <- snakecase::to_snake_case(colnames(annotation01))

  list(individuals = tbl02, annotations = annotation02)

}
