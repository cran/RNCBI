\name{getLastResultParameter}
\title{Get Last Results Parameter}
\usage{getLastResultParameter(NCBIObject)}
\description{This will get the parameter of the last result from the NCBIClass object. 
The returned vector will contain the first level result parameter. This means, 
that each parameter could contain more parameter names, if it is a complex type.}
\value{A vector with the parameter names of the last result. Or NULL, if there are no last results.}
\alias{getLastResultParameter,NCBIClass-method}
\alias{getLastResultParameter}
\author{Martin Schumann}
\seealso{\code{\link[=getResultParameterByName,NCBIClass-method]{getResultParameterByName}} \code{\link[=NCBIResult-class]{NCBIResult}}}
\arguments{\item{NCBIObject}{The current instance of the NCBIClass.}}
