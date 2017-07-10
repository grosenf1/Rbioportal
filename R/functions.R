#
# sample client for NCBO Annotator in R
# Author: Gabriel Rosenfeld (modified from Andrew Su)
#

#' Annotation of text with NCBO ontologies
#'
#' Annotates a queryText with NCBO ontologies
#'
#' @param queryText character - the string to annotate
#' @param apikey character - your unique API key (see NCBO account information on NCBO website)
#' @param ontologies character - the acronym of the ontology you wish to use (e.g. "NCIT")
#' @param expand_mappings boolean - TRUE means that the following manual mappings will be used in annotation: UMLS, REST, CUI, OBOXREF
#' @param longest_only boolean - TRUE means that only the longest match for a given phrase will be returned
#'
#' @return list parsed JSON call from the API
#'
#' @examples
#' annotate_it("gata4")
#'
#' @export
#'
annotate_it <- function( queryText, apikey = yourAPIkey, ontologies = NULL,
                         expand_mappings = FALSE, longest_only = FALSE) {
  assertthat::assert_that(assertthat::is.string(queryText))
  # Error: is.string(queryText) is not TRUE
  assertthat::assert_that(assertthat::is.string(yourAPIkey))
  # Error: is.string(yourAPIkey) is not TRUE
  assertthat::assert_that(assertthat::is.string(ontologies) | is.null(ontologies))
  # Error: is.string(ontologies) or is.null(ontologies) is not TRUE
  assertthat::assert_that(assertthat::is.flag(expand_mappings))
  # Error: is.flag(expand_mappings) is not TRUE
  assertthat::assert_that(assertthat::is.flag(longest_only))
  # Error: is.flag(longest_only) is not TRUE

  # Login to BioPortal to get apikey
  res<- RCurl::postForm('http://data.bioontology.org/annotator',
                 "text"=queryText,
                 "apikey"=apikey,
                 "ontologies"={ifelse(is.null(ontologies), "", paste0(ontologies, collapse = ","))},
                 "require_exact_match"="true",
                 "mappings"="all",
                 "expand_mappings"=ifelse(expand_mappings, "true", "false"),
                 "longest_only"=ifelse(longest_only, "true", "false"),
                 "format"="json")

  res <- jsonlite::fromJSON(res)
  return(res)
}

#' Get all current NCBO ontologies
#'
#' Returns all current NCBO ontologies and their metadata
#'
#' @param apikey character - your unique API key (see NCBO account information on NCBO website)
#'
#' @return list parsed JSON call from the API
#'
#' @examples
#' o <- get_ontologies()
#'
#' @export
#'
get_ontologies <- function( apikey = yourAPIkey ) {
  assertthat::assert_that(assertthat::is.string(yourAPIkey))
  # Error: is.string(ontologies) or is.null(ontologies) is not TRUE

  # Login to BioPortal to get apikey
  res<- RCurl::getForm('http://data.bioontology.org/ontologies',
                        "apikey"=apikey)

  res <- jsonlite::fromJSON(res)
  return(res)
}


#' Search NCBO ontology terms
#'
#' Searches NCBO ontology terms with queryText
#'
#' @param queryText character - the search term
#' @param apikey character - your unique API key (see NCBO account information on NCBO website)
#' @param ontologies character - the acronym of the ontology you wish to use (e.g. "NCIT")
#' @param require_exact_match boolean - default (TRUE)
#' @param root_only boolean - default (FALSE) true means performs root node only search
#'
#' @return list parsed JSON call from the API
#'
#' @examples
#' search_it("gata4")
#'
#' @export
#'
search_it <- function( queryText, apikey = yourAPIkey, require_exact_match = TRUE,
                       ontologies = NULL, roots_only = FALSE) {
  assertthat::assert_that(assertthat::is.string(queryText))
  # Error: is.string(queryText) is not TRUE
  assertthat::assert_that(assertthat::is.string(yourAPIkey))
  # Error: is.string(yourAPIkey) is not TRUE
  assertthat::assert_that(assertthat::is.string(ontologies) | is.null(ontologies))
  # Error: is.string(ontologies) or is.null(ontologies) is not TRUE
  assertthat::assert_that(assertthat::is.flag(require_exact_match))
  # Error: is.flag(require_exact_match) is not TRUE
  assertthat::assert_that(assertthat::is.flag(roots_only))
  # Error: is.flag(roots_only) is not TRUE

  # Login to BioPortal to get apikey
  res<- RCurl::postForm('http://data.bioontology.org/search',
                 "q"=queryText,
                 "apikey"=apikey,
                 "ontologies"={ifelse(is.null(ontologies), "", paste0(ontologies, collapse = ","))},
                 "require_exact_match"=ifelse(require_exact_match, "true", "false"),
                 "roots_only"=ifelse(roots_only, "true", "false"))

  res <- jsonlite::fromJSON(res)
  return(res)
}

#' Recommend NCBO ontologies using recommender service
#'
#' Recommends NCBO ontologies to use based on queryText
#'
#' @param queryText character - the search term
#' @param apikey character - your unique API key (see NCBO account information on NCBO website)
#'
#' @return list parsed JSON call from the API
#'
#' @examples
#' recommend_it("gata4")
#'
#' @export
#'
recommend_it <- function( queryText, apikey = yourAPIkey) {
  assertthat::assert_that(assertthat::is.string(queryText))
  # Error: is.string(queryText) is not TRUE
  assertthat::assert_that(assertthat::is.string(yourAPIkey))
  # Error: is.string(yourAPIkey) is not TRUE

  # Login to BioPortal to get apikey
  res<- RCurl::postForm('http://data.bioontology.org/recommender',
                 "input"=queryText,
                 "apikey"=apikey)

  res <- jsonlite::fromJSON(res)
  return(res)
}

#' Set global variable for your API key for other API functions
#'
#' Sets global variable yourAPIkey
#'
#' @param yourAPIkey character - your unique API key (see NCBO account information on NCBO website)
#'
#' @return global variable yourAPIkey set to your unique API key
#'
#' @examples
#' set_key("xxxxx-xxxx-xxxx-xxxx")
#'
#' @export
#'
set_key <- function(yourAPIkey){
  assertthat::assert_that(assertthat::is.string(yourAPIkey))
  # Error: is.string(yourAPIkey) is not TRUE

  yourAPIkey <<- yourAPIkey
}
