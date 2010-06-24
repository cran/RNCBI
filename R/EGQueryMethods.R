# This is the class for EGQuery.
# It holds the request parameter and the results, after the request was fired.
# 
# Author: Martin Schumann
###############################################################################

# the user has to specify the request parameter in the request variable from this class
## the init method for this class
setMethod("initialize", "EGQueryClass", function(.Object, NCBIObj) {
			# fill the request list with parameter names
			requestVector <- getRequestParameter(NCBIObj, "run_eGquery")
			# create an empty list with the length of the requestVector
			.Object@request <- vector("list", length(requestVector))
			# assign the names of the request parameter to the list
			names(.Object@request) <- requestVector
			.Object@NCBIObj <- NCBIObj
#			.Object@results <- new("NCBIResult")
			.Object
		})

## This runs run_eGQuery operation.
#' Calls the appropriate interface method for this operation and stores the results.
#' \section{Warning}{
#' This function should not be called by the user.
#' }
#' @param EGQObj An object of this class.
#' @return The provided object with the results, if no errors occured.
#' @usage .requestEGQueryMethod(EGQObj)
#' @seealso \code{\link[=requestEGQuery]{requestEGQuery}}
#' @aliases .requestEGQueryMethod,EGQueryClass-method
#' @aliases .requestEGQueryMethod
#' @author Martin Schumann
#' @export
setMethod(".requestEGQueryMethod", "EGQueryClass", function(EGQObj) {
			NCBIObj <- EGQObj@NCBIObj
			# delete previous results
			EGQObj@results <- list()
			# parse the request list and create a vector
			argArray <- .parseRequest(EGQObj@NCBIObj, EGQObj)
			.jcall(NCBIObj@interface,"V","EGQueryInterface",.jarray(argArray))
			if (!.jcall(NCBIObj@interface,"Z","isErrorCaused")) { 
				# now call the function to save the results in the NCBIResult object
				EGQObj <- .getEGQueryResults(EGQObj)
			}
			# return modified object
			EGQObj
		})

## This will get the results of the request from the java interface
#' Parses the results parameter names and calls the appropriate function of \code{\link[=NCBIResult-class]{NCBIResult}}, 
#' depending on whether it is a complex type or a simple type.
#' @title Parse The Results
#' @param EGQObj An object of this class. 
#' @return The provided object with a modified results list, if no errors occured.
#' @usage .getEGQueryResults(EGQObj)
#' @aliases .getEGQueryResults,EGQueryClass-method
#' @aliases .getEGQueryResults
#' @author Martin Schumann
#' @export
setMethod(".getEGQueryResults", "EGQueryClass", function(EGQObj) {
			false <- EGQObj@NCBIObj@rinterface_false
			true <- EGQObj@NCBIObj@rinterface_true
			paras <- getLastResultParameter(EGQObj@NCBIObj)
			# find the complex type of the result
			for (i in 1:length(paras)-1) {
				if (paras[i+1]==true) {
					EGQObj <- .getComplexType(EGQObj, paras[i])
				}
				if (paras[i+1]==false) {
					# this is a simple type, so simply assign it.
					EGQObj <- .getSimpleType(EGQObj, paras[i])
				}
				# take one step forward
				i <- i+1
			}
			# return this modified object
			EGQObj
		})


