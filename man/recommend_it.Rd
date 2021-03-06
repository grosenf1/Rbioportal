% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{recommend_it}
\alias{recommend_it}
\title{Recommend NCBO ontologies using recommender service}
\usage{
recommend_it(queryText, apikey = yourAPIkey)
}
\arguments{
\item{queryText}{character - the search term}

\item{apikey}{character - your unique API key (see NCBO account information on NCBO website)}
}
\value{
list parsed JSON call from the API
}
\description{
Recommends NCBO ontologies to use based on queryText
}
\examples{
recommend_it("gata4")

}
