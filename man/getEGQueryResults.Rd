\name{.getEGQueryResults}
\title{Parse The Results}
\usage{.getEGQueryResults(EGQObj)}
\description{Parses the results parameter names and calls the appropriate function of \code{\link[=NCBIResult-class]{NCBIResult}}, 
depending on whether it is a complex type or a simple type.}
\value{The provided object with a modified results list, if no errors occured.}
\alias{.getEGQueryResults,EGQueryClass-method}
\alias{.getEGQueryResults}
\author{Martin Schumann}
\arguments{\item{EGQObj}{An object of this class.}}
