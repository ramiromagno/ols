spec_term <- function() {

  tspec_df(
    tib_chr("iri"),
    tib_chr("lang"),
    tib_variant("description"),
    tib_variant("synonyms"),
    tib_row(
      "annotation",
      tib_variant("charge", required = FALSE),
      tib_variant("formula", required = FALSE),
      tib_variant("has_dbxref", required = FALSE),
      tib_variant("has_obo_namespace", required = FALSE),
      tib_variant("id", required = FALSE),
      tib_variant("inchi", required = FALSE),
      tib_variant("inchikey", required = FALSE),
      tib_variant("mass", required = FALSE),
      tib_variant("monoisotopicmass", required = FALSE),
      tib_variant("smiles", required = FALSE),
      tib_variant("has obsolescence reason", required = FALSE),
      tib_variant("term replaced by", required = FALSE),
    ),
    tib_chr("label"),
    tib_chr("ontology_name"),
    tib_chr("ontology_prefix"),
    tib_chr("ontology_iri"),
    tib_lgl("is_obsolete"),
    tib_chr("term_replaced_by"),
    tib_lgl("is_defining_ontology"),
    tib_lgl("has_children"),
    tib_lgl("is_root"),
    tib_chr("short_form"),
    tib_chr("obo_id"),
    tib_variant("in_subset"),
    tib_variant("obo_definition_citation"),
    tib_df(
      "obo_xref",
      tib_chr("database"),
      tib_chr("id"),
      tib_variant("description"),
      tib_chr("url"),
    ),
    tib_variant("obo_synonym"),
    tib_lgl("is_preferred_root"),
    tib_df(
      "_links",
      .names_to = ".names",
      tib_chr("href"),
    ),
  )

}

parse_terms <- function(lst) {

  tbl01 <- tibblify(lst, spec_term())

  tbl02 <- tbl01
  colnames(tbl02) <- snakecase::to_snake_case(colnames(tbl01))

  annotation01 <-
    tbl02 |>
    dplyr::select(c("obo_id", "annotation")) |>
    tidyr::unnest("annotation")

  annotation02 <- annotation01
  colnames(annotation02) <- snakecase::to_snake_case(colnames(annotation01))

  list(terms = tbl02, annotation = annotation02)

}

#' @importFrom rlang .data
get_terms <- function(term_id = NULL) {

  json <-
    if (is.null(term_id)) {
      json <- get("/api/terms/")
      lst <- flatten(json, "terms", embedded = TRUE)
    } else {
      #json <- get("/api/terms/{term_id}", term_id = term_id)
      #lst <- flatten(json, embedded = FALSE)
      stop("`term_id` not implemented")
    }

  parse_terms(lst)

}

# get_terms_by_ontology(ontology_id = "doid")

get_terms_by_ontology <- function(ontology_id = NULL) {

  if (is.null(ontology_id)) return(get_terms())

  json <- get("/api/ontologies/{ontology_id}/terms", ontology_id = ontology_id)
  lst <- flatten(json, "terms", embedded = TRUE)

  parse_terms(lst)

}
