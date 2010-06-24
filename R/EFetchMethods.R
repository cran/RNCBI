# This is the class for EFetch.
# It holds the request parameter and the results, after the request was fired.
# The request parameter are not available until the database was selected for the efetch operation.
# 
# Author: Martin Schumann
###############################################################################

## the user has to specify the request parameter in the request variable from this class
## the init method for this class
setMethod("initialize", "EFetchClass", function(.Object, NCBIObj, database) {
			# init the efetch operation with the database name
			.jcall(NCBIObj@interface,"V","initEFetchOperation",toString(database))
			# if the database is empty or wrong, the java part will indicate this and ErrorCaused will be true
			if (!.jcall(NCBIObj@interface,"Z","isErrorCaused")) {
				# fill the request list with parameter names
				requestVector <- getRequestParameter(NCBIObj, "efetch")
				# create an empty list with the length of the requestVector
				.Object@request <- vector("list", length(requestVector))
				# assign the names of the request parameter to the list
				names(.Object@request) <- requestVector
			}
			.Object@NCBIObj <- NCBIObj
#			.Object@results <- new("NCBIResult")
			.Object
		})

## This runs run_eFetch operation.
#' Calls the appropriate interface method for this operation and stores the results.
#' \section{Warning}{
#' This function should not be called by the user.
#' }
#' @param EFObj An object of this class
#' @return The modified object from the arguments, if no errors occured.
#' @usage .requestEFetchMethod(EFObj)
#' @seealso \code{\link[=requestEFetch]{requestEFetch}}
#' @aliases .requestEFetchMethod,EFetchClass-method
#' @aliases .requestEFetchMethod
#' @author Martin Schumann
#' @export
setMethod(".requestEFetchMethod", "EFetchClass", function(EFObj) {
			NCBIObj <- EFObj@NCBIObj
			# delete previous results
			EFObj@results <- list()
			# an error could be caused during the initialization of efetch
			if (!.jcall(NCBIObj@interface,"Z","isErrorCaused")) {
				# parse the request list and create a vector
				argArray <- .parseRequest(EFObj@NCBIObj, EFObj)
				.jcall(NCBIObj@interface,"V","EFetchInterface",.jarray(argArray))
				# look again for errors
				if (!.jcall(NCBIObj@interface,"Z","isErrorCaused")) {
					# now call the function to save the results in the NCBIResult object
					EFObj <- .getEFetchResults(EFObj)
				}
			}
			# return modified object
			EFObj
		})

## This will get the results of the request from the java interface
#' Parses the results parameter names and calls the appropriate function of \code{\link[=NCBIResult-class]{NCBIResult}}, 
#' depending on whether the parameter is a complex type or a simple type.
#' @title Parse The Results
#' @param EFObj An object of this class.
#' @return The modified object with the results, if no errors occured.
#' @usage .getEFetchResults(EFObj)
#' @aliases .getEFetchResults,EFetchClass-method
#' @aliases .getEFetchResults
#' @author Martin Schumann
#' @export
setMethod(".getEFetchResults", "EFetchClass", function(EFObj) {
			false <- EFObj@NCBIObj@rinterface_false
			true <- EFObj@NCBIObj@rinterface_true
			paras <- getLastResultParameter(EFObj@NCBIObj)
			# find the complex type of the result
			for (i in 1:length(paras)-1) {
				if (paras[i+1]==true) {
					EFObj <- .getComplexType(EFObj, paras[i])
				}
				if (paras[i+1]==false) {
					# this is a simple type, so simply assign it.
					EFObj <- .getSimpleType(EFObj, paras[i])
				}
				# take one step forward
				i <- i+1
			}
			# return this modified object
			EFObj
		})
