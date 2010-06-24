\name{.parseRequest}
\title{Parsing The Request}
\usage{.parseRequest(thisObj, obj)}
\description{Returns the request of the given object as a vector. The vector will look like: 
c("parameter-1 name", "parameter-1 value, "parameter-2 name", "parameter-2 value, ..., "parameter-n name", "parameter-n value).
This structure is required for the interfaces of each web service operation in the java part.
The returned vector will be used as argument for the java interface.}
\value{A vector with the request parameters and their values.}
\alias{.parseRequest,NCBIClass-method}
\alias{.parseRequest}
\seealso{\code{\link[=NCBIRequest-class]{NCBIRequest}}}
\author{Martin Schumann}
\arguments{\item{thisObj}{Object from the NCBIClass to access the interface.}
\item{obj}{An object of the operation classes.}}
