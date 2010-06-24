\name{NCBI}
\alias{NCBI}
\title{The First Step To Use The NCBI Webservice}
\usage{NCBI(proxy="", debug=FALSE, tidy=TRUE)}
\description{This creates an object of the RInterface and stores it in the NCBI class.
The program uses the proxy settings from the R environment, but you can set the proxy manually, too.
If your proxy server requires authentication, then use the \code{\link[=setProxyUser,NCBIClass-method]{setProxyUser}} function.}
\value{An object of the NCBI class.}
\seealso{\code{\link[=NCBIClass-class]{NCBIClass}} \code{\link[=setProxyUser,NCBIClass-method]{setProxyUser}}}
\author{Martin Schumann}
\arguments{\item{proxy}{The proxy as a string like "http://host:port". Can be omitted, if the proxy settings from the R environment are correct.}
\item{debug}{Whether to debug or not to debug.}
\item{tidy}{Whether to remove empty elements from the EFetch results or not.}}
\examples{\dontrun{
# For further examples please see RNCBIManual.pdf in inst/doc.
# This will create an object of the NCBIClass with a proxy set. 
ncbi <- NCBI("http://some.host.com:1234")
# This will set the user and password for the proxy
setProxyUser(user="user", password="secretpassword")
}}
