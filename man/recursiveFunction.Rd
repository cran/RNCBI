\name{.recursiveFunction}
\title{Recursive Function}
\usage{.recursiveFunction(opObj, xmldoc, xml_array, xml_object, xml_entry, xml_attr_name, xml_attr_length)}
\description{Creates the complex structure of the results list from a given xml document.
This function works recursive and only should be used from within this class.}
\value{The complex list with the results from the xml document.}
\alias{.recursiveFunction,NCBIResult-method}
\alias{.recursiveFunction}
\author{Martin Schumann}
\arguments{\item{opObj}{An object of this class.}
\item{xmldoc}{The xml document, which should be parsed.}
\item{xml_array}{Keyword for an array in the xml document.}
\item{xml_object}{Keyword for an object in the xml document.}
\item{xml_entry}{Keyword for an entry in the xml document.}
\item{xml_attr_name}{Keyword for the name attribute in the xml document.}
\item{xml_attr_length}{Keyword for the length attribute in the xml document.}}
