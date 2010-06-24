\name{getResultParameterByName}
\title{Get First Level Parameter By Name}
\usage{getResultParameterByName(NCBIObject, name)}
\description{Returns further parameter of a specific complex type and NULL, if name wasn't a complex type.
The function only returns the parameter for a result parameter from the first level. If there are further complex types contained,
the results (see \code{\link[=NCBIResult-class]{NCBIResult}}) list will show them.}
\value{The parameter names for the given complex type. The result can contain more complex types.}
\alias{getResultParameterByName,NCBIClass-method}
\alias{getResultParameterByName}
\author{Martin Schumann}
\arguments{\item{NCBIObject}{The current instance of the NCBIClass.}
\item{name}{The name of the complex type, the parameter names should be returned for.}}
