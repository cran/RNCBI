# This is the class for EPost.
# It holds the request parameter and the results, after the request was fired.
# 
# Author: Martin Schumann
###############################################################################

# the user has to specify the request parameter in the request variable from this class
## the init method for this class
setMethod("initialize", "EPostClass", function(.Object, NCBIObj) {
			# fill the request list with parameter names
			requestVector <- getRequestParameter(NCBIObj, "epost")
			# create an empty list with the length of the requestVector
			.Object@request <- vector("list", length(requestVector))
			# assign the names of the request parameter to the list
			names(.Object@request) <- requestVector
			.Object@NCBIObj <- NCBIObj
#			.Object@results <- new("NCBIResult")
			.Object
		})

## This runs run_ePost operation.
#' Calls the appropriate interface method for this operation and stores the results.
#' \section{Warning}{
#' This function should not be called by the user.
#' }
#' @param EPObj An object of this class.
#' @return The object with the results, if no errors occured.
#' @usage .requestEPostMethod(EPObj)
#' @seealso \code{\link[=requestEPost]{requestEPost}}
#' @aliases .requestEPostMethod,EPostClass-method
#' @aliases .requestEPostMethod
#' @author Martin Schumann
#' @export
setMethod(".requestEPostMethod", "EPostClass", function(EPObj) {
			NCBIObj <- EPObj@NCBIObj
			# delete previous results
			EPObj@results <- list()
			# parse the request list and create a vector
			argArray <- .parseRequest(EPObj@NCBIObj, EPObj)
			.jcall(NCBIObj@interface,"V","EPostInterface",.jarray(argArray))
			if (!.jcall(NCBIObj@interface,"Z","isErrorCaused")) { 
				# now call the function to save the results in the NCBIResult object
				EPObj <- .getEPostResults(EPObj)
			}
			# return modified object
			EPObj
		})

## This will get the results of the request from the java interface
#' Parses the results parameter names and calls the appropriate function of \code{\link[=NCBIResult-class]{NCBIResult}}, 
#' depending on whether the parameter is a complex type or a simple type.
#' @title Parse The Results
#' @param EPObj An object of this class.
#' @return The object containing the results, if no errors occured.
#' @usage .getEPostResults(EPObj)
#' @aliases .getEPostResults,EPostClass-method
#' @aliases .getEPostResults
#' @author Martin Schumann
#' @export
setMethod(".getEPostResults", "EPostClass", function(EPObj) {
			false <- EPObj@NCBIObj@rinterface_false
			true <- EPObj@NCBIObj@rinterface_true
			paras <- getLastResultParameter(EPObj@NCBIObj)
			# find the complex type of the result
			for (i in 1:length(paras)-1) {
				if (paras[i+1]==true) {
					EPObj <- .getComplexType(EPObj, paras[i])
				}
				if (paras[i+1]==false) {
					# this is a simple type, so simply assign it.
					EPObj <- .getSimpleType(EPObj, paras[i])
				}
				# take one step forward
				i <- i+1
			}
			# return this modified object
			EPObj
		})

