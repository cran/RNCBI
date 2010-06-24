# These functions will help.
# Most of them are generating functions, which will prevent the user from using
# "new" to create an object of a specific class.
# 
# Author: Martin Schumann
###############################################################################

## generating function for the NCBI class

#' This creates an object of the RInterface and stores it in the NCBI class.
#' The program uses the proxy settings from the R environment, but you can set the proxy manually, too.
#' If your proxy server requires authentication, then use the \code{\link[=setProxyUser,NCBIClass-method]{setProxyUser}} function.
#' @examples \dontrun{
#' # For further examples please see RNCBIManual.pdf in inst/doc.
#' # This will create an object of the NCBIClass with a proxy set. 
#' ncbi <- NCBI("http://some.host.com:1234")
#' # This will set the user and password for the proxy
#' setProxyUser(user="user", password="secretpassword")
#' }
#' @title The First Step To Use The NCBI Webservice
#' @param proxy The proxy as a string like "http://host:port". Can be omitted, if the proxy settings from the R environment are correct.
#' @param debug Whether to debug or not to debug.
#' @param tidy Whether to remove empty elements from the EFetch results or not.
#' @return An object of the NCBI class.
#' @seealso \code{\link[=NCBIClass-class]{NCBIClass}} \code{\link[=setProxyUser,NCBIClass-method]{setProxyUser}}
#' @author Martin Schumann
#' @export
NCBI <- function(proxy="", debug=FALSE, tidy=TRUE) {
	new("NCBIClass", proxy, debug, tidy)
}

#' This function creates an object of the EInfo class and returns it.
#' @example \dontrun{
#' ncbi <- NCBI()
#' einfo <- EInfo(ncbi)
#' }
#' @title New EInfo Object
#' @param NCBIObject The current instance of the NCBIClass. The instance of the NCBI class. Required to get access to the java interface.
#' @return An object of the EInfo class.
#' @seealso \code{\link[=EInfoClass-class]{EInfoClass}}
#' @author Martin Schumann
#' @export
EInfo <- function(NCBIObject) {
	new("EInfoClass", NCBIObject)
}

#' This function creates an object of the ESearch class and returns it.
#' @example \dontrun{
#' ncbi <- NCBI()
#' esearch <- ESearch(ncbi)
#' }
#' @title New ESearch Object
#' @param NCBIObject The current instance of the NCBIClass. 
#' @return An object of the ESearch class.
#' #' @seealso \code{\link[=ESearchClass-class]{ESearchClass}}
#' @author Martin Schumann
#' @export
ESearch <- function(NCBIObject) {
	new("ESearchClass", NCBIObject)
}

#' This function creates an object of the EGQuery class and returns it.
#' @example \dontrun{
#' ncbi <- NCBI()
#' egquery <- EGQuery(ncbi)
#' }
#' @title New EGQuery Object
#' @param NCBIObject The current instance of the NCBIClass. 
#' @return An object of the EGQuery class.
#' #' @seealso \code{\link[=EGQueryClass-class]{EGQueryClass}} 
#' @author Martin Schumann
#' @export
EGQuery <- function(NCBIObject) {
	new("EGQueryClass", NCBIObject)
}

#' This function creates an object of the ELink class and returns it.
#' @example \dontrun{
#' ncbi <- NCBI()
#' elink <- ELink(ncbi)
#' }
#' @title New ELink Object
#' @param NCBIObject The current instance of the NCBIClass. 
#' @return An object of the ELink class.
#' @seealso \code{\link[=ELinkClass-class]{ELinkClass}} 
#' @author Martin Schumann
#' @export
ELink <- function(NCBIObject) {
	new("ELinkClass", NCBIObject)
}

#' This function creates an object of the EPost class and returns it.
#' @example \dontrun{
#' ncbi <- NCBI()
#' epost <- EPost(ncbi)
#' }
#' @title New EPost Object
#' @param NCBIObject The current instance of the NCBIClass. 
#' @return An object of the EPost class.
#' @seealso \code{\link[=EPostClass-class]{EPostClass}}
#' @author Martin Schumann
#' @export
EPost <- function(NCBIObject) {
	new("EPostClass", NCBIObject)
}

#' This function creates an object of the ESpell class and returns it.
#' @example \dontrun{
#' ncbi <- NCBI()
#' espell <- ESpell(ncbi)
#' }
#' @title New ESpell Object
#' @param NCBIObject The current instance of the NCBIClass. 
#' @return An object of the ESpell class.
#' @seealso \code{\link[=ESpellClass-class]{ESpellClass}}
#' @author Martin Schumann
#' @export
ESpell <- function(NCBIObject) {
	new("ESpellClass", NCBIObject)
}

#' This function creates an object of the ESummary class and returns it.
#' @example \dontrun{
#' ncbi <- NCBI()
#' esummary <- ESummary(ncbi)
#' }
#' @title New ESummary Object
#' @param NCBIObject The current instance of the NCBIClass. 
#' @return An object of the ESummary class.
#' @seealso \code{\link[=ESummaryClass-class]{ESummaryClass}}
#' @author Martin Schumann
#' @export
ESummary <- function(NCBIObject) {
	new("ESummaryClass", NCBIObject)
}

## takes a database as init argument
#' This function creates an object of the EFetch class and returns it. 
#' It takes the database name for the EFetch operation as additional argument.
#' @example \dontrun{
#' ncbi <- NCBI()
#' efetch <- EFetch(ncbi, "pubmed")
#' }
#' @title New EFetch Object
#' @param NCBIObject The current instance of the NCBIClass. 
#' @param database The name of the EFetch database for the request.
#' @return The initiated EFetch Object.
#' @seealso \code{\link[=EFetchClass-class]{EFetchClass}}
#' @seealso \code{\link[=getEFetchDatabases,NCBIClass-method]{getEFetchDatabases}}
#' @author Martin Schumann
#' @export
EFetch <- function(NCBIObject, database) {
	efetch <- new("EFetchClass", NCBIObject, toString(database))
	efetch
}

#' Send an EInfo request to the NCBI Webservice. The EInfo object should have been prepared before making any requests.
#' @example \dontrun{
#' einfo <- setRequestParameter(einfo, "db", "pubmed")
#' einfo <- requestEInfo(einfo)
#' results <- getResults(einfo)
#' }
#' @title Send Request
#' @param einfo The initiated EInfo object.
#' @return The EInfo object with the results inside. 
#' @author Martin Schumann
#' @export
requestEInfo <- function(einfo) {
	einfo <- .requestEInfoMethod(einfo)
	einfo
}

#' Send an ESearch request to the NCBI Webservice. The ESearch object should have been prepared before making any requests.
#' @example \dontrun{
#' esearch <- ESearch(ncbi)
#' esearch <- setRequestParameter(esearch, "db", "pmc")
#' esearch <- setRequestParameter(esearch, "term", "stem+cells+AND+free+fulltext[filter]")
#' esearch <- setRequestParameter(esearch, "retmax", 15)
#' esearch <- requestESearch(esearch)
#' results <- getResults(esearch)
#' }
#' @title Send Request
#' @param esearch The initiated ESearch object.
#' @return The ESearch object with the results inside.
#' @author Martin Schumann
#' @export
requestESearch <- function(esearch) {
	esearch <- .requestESearchMethod(esearch)
	esearch
}

#' Send an EGQuery request to the NCBI Webservice. The EGQuery object should have been prepared before making any requests.
#' @example \dontrun{
#' egquery <- EGQuery(ncbi)
#' egquery <- setRequestParameter(egquery, "term", "mouse")
#' egquery <- requestEGQuery(egquery)
#' results <- getResults(egquery)
#' }
#' @title Send Request
#' @param egquery The initiated EGQuery object.
#' @return The EGQuery object with the results inside.
#' @author Martin Schumann
#' @export
requestEGQuery <- function(egquery) {
	egquery <- .requestEGQueryMethod(egquery)
	egquery
}

#' Send an ELink request to the NCBI Webservice. The ELink object should have been prepared before making any requests.
#' @example \dontrun{
#' elink <- ELink(ncbi)
#' elink <- setRequestParameter(elink, "db", "protein")
#' elink <- setRequestParameter(elink, "dbfrom", "nuccore")
#' elink <- setRequestParameter(elink, "id", c(48819, 7140345))
#' elink <- requestELink(elink)
#' results <- getResults(elink)
#' }
#' @title Send Request
#' @param elink The initiated ELink object.
#' @return The ELink object with the results inside.
#' @author Martin Schumann
#' @export
requestELink <- function(elink) {
	elink <- .requestELinkMethod(elink)
	elink
}

#' Send an EPost request to the NCBI Webservice. The EPost object should have been prepared before making any requests.
#' @example \dontrun{
#' epost <- EPost(ncbi)
#' epost <- setRequestParameter(epost, "db", "pubmed")
#' epost <- setRequestParameter(epost, "id", c(123, 456, 37281, 983621))
#' epost <- requestEPost(epost)
#' results <- getResults(epost)
#' }
#' @title Send Request
#' @param epost The initiated EPost object.
#' @return The EPost object with the results inside.
#' @author Martin Schumann
#' @export
requestEPost <- function(epost) {
	epost <- .requestEPostMethod(epost)
	epost
}

#' Send an ESpell request to the NCBI Webservice. The ESpell object should have been prepared before making any requests.
#' @example \dontrun{
#' 
#' }
#' @title Send Request
#' @param espell The initiated ESpell object.
#' @return The ESpell object with the results inside.
#' @author Martin Schumann
#' @export
requestESpell <- function(espell) {
	espell <- .requestESpellMethod(espell)
	espell
}

#' Send an ESummary request to the NCBI Webservice. The ESummary object should have been prepared before making any requests.
#' @example \dontrun{
#' 
#' }
#' @title Send Request
#' @param esummary The initiated ESummary object.
#' @return The ESummary object with the results inside.
#' @author Martin Schumann
#' @export
requestESummary <- function(esummary) {
	esummary <- .requestESummaryMethod(esummary)
	esummary
}

#' Send an EFetch request to the NCBI Webservice. The EFetch object should have been prepared before making any requests.
#' @example \dontrun{
#' 
#' }
#' @title Send Request
#' @param efetch The initiated EFetch object.
#' @return The EFetch object with the results inside.
#' @author Martin Schumann
#' @export
requestEFetch <- function(efetch) {
	efetch <- .requestEFetchMethod(efetch)
	efetch
}