# spec_ontologies <- function() {
#   tib_df(
#     "ontologies",
#     lang = tib_chr("lang"),
#     ontology_id = tib_chr("ontologyId"),
#     loaded = tib_chr("loaded"),
#     updated = tib_chr_date("updated"),
#     status = tib_chr("status"),
#     message = tib_chr("message"),
#     version = tib_chr("version"),
#     file_hash = tib_chr("fileHash"),
#     load_attempts = tib_int("loadAttempts"),
#     number_of_terms = tib_int("numberOfTerms"),
#     number_of_properties = tib_int("numberOfProperties"),
#     number_of_individuals = tib_int("numberOfIndividuals"),
#     config = spec_config()
#   )
# }
#
# spec_config <- function() {
#   tib_df(
#     id = tib_chr("id"),
#     version_iri = tib_chr("versionIri"),
#     namespace = tib_chr("namespace"),
#     preferred_prefix = tib_chr("preferredPrefix"),
#     title = tib_chr("title"),
#     description = tib_chr("description"),
#     homepage = tib_chr("homepage"),
#     version = tib_chr("version"),
#     mailing_list = tib_chr("mailingList"),
#     tracker = tib_chr("tracker"),
#     logo = tib_chr("logo"),
#     creators = tib_chr("creators"),
#     annotations = tib_chr("annotations"),
#     file_location = tib_chr("fileLocation"),
#     obo_slims = tib_chr("oboSlims"),
#     label_property = tib_chr("labelProperty"),
#     definition_properties = tib_chr("definitionProperties"),
#     synonym_properties = tib_chr("synonymProperties"),
#     hierarchical_properties = tib_chr("hierarchicalProperties"),
#     base_uris = tib_chr("baseUris"),
#     hidden_properties = tib_chr("hiddenProperties"),
#     preferred_rootTerms = tib_chr("preferredRootTerms"),
#     is_skos = tib_chr("isSkos"),
#     allow_download = tib_chr("allowDownload")
#   )
# }

# spec_ontology_by_id <- function() {
#
#   tib_df(
#     "ontologies",
#     tib_variant("languages"),
#     tib_chr("lang"),
#     tib_chr("ontologyId"),
#     tib_chr("loaded"),
#     tib_chr("updated"),
#     tib_chr("status"),
#     tib_chr("message"),
#     tib_chr("version"),
#     tib_chr("fileHash"),
#     tib_int("loadAttempts"),
#     tib_int("numberOfTerms"),
#     tib_int("numberOfProperties"),
#     tib_int("numberOfIndividuals"),
#     tib_row(
#       "config",
#       ontology_id = tib_chr("id"),
#       tib_chr("versionIri"),
#       tib_chr("namespace"),
#       tib_chr("preferredPrefix"),
#       tib_chr("title"),
#       tib_chr("description"),
#       tib_chr("homepage"),
#       tib_chr("version"),
#       tib_chr("mailingList"),
#       tib_chr("tracker"),
#       tib_chr("logo"),
#       tib_variant("creators"),
#       tib_chr("annotations"),
#       tib_chr("fileLocation"),
#       tib_lgl("oboSlims"),
#       tib_chr("labelProperty"),
#       tib_variant("definitionProperties"),
#       tib_variant("synonymProperties"),
#       tib_variant("hierarchicalProperties"),
#       tib_variant("baseUris"),
#       tib_variant("hiddenProperties"),
#       tib_variant("preferredRootTerms"),
#       tib_lgl("isSkos"),
#       tib_lgl("allowDownload"),
#     ),
#     tib_variant("baseUris"),
#     tib_df("_links",
#            .names_to = ".names",
#            tib_chr("href"),),
#   )
#
# }

spec_ontology <- function() {

  tspec_df(
    tib_variant("languages"),
    tib_chr("lang"),
    tib_chr("ontologyId"),
    tib_chr("loaded"),
    tib_chr("updated"),
    tib_chr("status"),
    tib_chr("message"),
    tib_chr("version"),
    tib_chr("fileHash"),
    tib_int("loadAttempts"),
    tib_int("numberOfTerms"),
    tib_int("numberOfProperties"),
    tib_int("numberOfIndividuals"),
    tib_row(
      "config",
      ontology_id = tib_chr("id"),
      tib_chr("versionIri"),
      tib_chr("namespace"),
      tib_chr("preferredPrefix"),
      tib_chr("title"),
      tib_chr("description"),
      tib_chr("homepage"),
      tib_chr("version"),
      tib_chr("mailingList"),
      tib_chr("tracker"),
      tib_chr("logo"),
      tib_variant("creators"),
      tib_chr("annotations"),
      tib_chr("fileLocation"),
      tib_lgl("oboSlims"),
      tib_chr("labelProperty"),
      tib_variant("definitionProperties"),
      tib_variant("synonymProperties"),
      tib_variant("hierarchicalProperties"),
      tib_variant("baseUris"),
      tib_variant("hiddenProperties"),
      tib_variant("preferredRootTerms"),
      tib_lgl("isSkos"),
      tib_lgl("allowDownload"),
    ),
    tib_variant("baseUris"),
    tib_df("_links",
           .names_to = ".names",
           tib_chr("href"),),
  )

}


#' @importFrom rlang .data
get_ontologies <- function(ontology_id = NULL) {

  json <-
    if (is.null(ontology_id)) {
      json <- get("/api/ontologies/")
      lst <- flatten(json, "ontologies", embedded = TRUE)
    } else {
      json <- get("/api/ontologies/{ontology_id}", ontology_id = ontology_id)
      lst <- flatten(json, embedded = FALSE)
    }

  tbl01 <- tibblify(lst, spec_ontology())

  tbl02 <- tbl01
  colnames(tbl02) <- snakecase::to_snake_case(colnames(tbl01))

  config01 <- tbl02$config
  config02 <- config01
  colnames(config02) <- snakecase::to_snake_case(colnames(config01))

  list(ontologies = tbl02, config = config02)

}
