\name{.getEFetchResults}
\title{Parse The Results}
\usage{.getEFetchResults(EFObj)}
\description{Parses the results parameter names and calls the appropriate function of \code{\link[=NCBIResult-class]{NCBIResult}}, 
depending on whether the parameter is a complex type or a simple type.}
\value{The modified object with the results, if no errors occured.}
\alias{.getEFetchResults,EFetchClass-method}
\alias{.getEFetchResults}
\author{Martin Schumann}
\arguments{\item{EFObj}{An object of this class.}}
