\name{EFetch}
\alias{EFetch}
\title{New EFetch Object}
\usage{EFetch(NCBIObject, database)}
\description{This function creates an object of the EFetch class and returns it. 
It takes the database name for the EFetch operation as additional argument.}
\value{The initiated EFetch Object.}
\seealso{\code{\link[=EFetchClass-class]{EFetchClass}} \code{\link[=getEFetchDatabases,NCBIClass-method]{getEFetchDatabases}}}
\author{Martin Schumann}
\arguments{\item{NCBIObject}{The current instance of the NCBIClass.}
\item{database}{The name of the EFetch database for the request.}}
