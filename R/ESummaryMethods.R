# This is the class for ESummary.
# It holds the request parameter and the results, after the request was fired.
# 
# Author: Martin Schumann
###############################################################################

# the user has to specify the request parameter in the request variable from this class
## the init method for this class
setMethod("initialize", "ESummaryClass", function(.Object, NCBIObj) {
			# fill the request list with parameter names
			requestVector <- getRequestParameter(NCBIObj, "esummary")
			# create an empty list with the length of the requestVector
			.Object@request <- vector("list", length(requestVector))
			# assign the names of the request parameter to the list
			names(.Object@request) <- requestVector
			.Object@NCBIObj <- NCBIObj
#			.Object@results <- new("NCBIResult")
			.Object
		})

## This runs run_eSummary operation.
#' Calls the appropriate interface method for this operation and stores the results.
#' \section{Warning}{
#' This function should not be called by the user.
#' }
#' @param ESObj An object of this class.
#' @return The object with the results, if no errors occured.
#' @usage .requestESummaryMethod(ESObj)
#' @seealso \code{\link[=requestESummary]{requestESummary}}
#' @aliases .requestESummaryMethod,ESummaryClass-method
#' @aliases .requestESummaryMethod
#' @author Martin Schumann
#' @export
setMethod(".requestESummaryMethod", "ESummaryClass", function(ESObj) {
			NCBIObj <- ESObj@NCBIObj
			# delete previous results
			ESObj@results <- list()
			# parse the request list and create a vector
			argArray <- .parseRequest(ESObj@NCBIObj, ESObj)
			.jcall(NCBIObj@interface,"V","ESummaryInterface",.jarray(argArray))
			if (!.jcall(NCBIObj@interface,"Z","isErrorCaused")) { 
				# now call the function to save the results in the NCBIResult object
				ESObj <- .getESummaryResults(ESObj)
			}
			# return modified object
			ESObj
		})

## This will get the results of the request from the java interface
#' Parses the results parameter names and calls the appropriate function of \code{\link[=NCBIResult-class]{NCBIResult}}, 
#' depending on whether it is a complex type or a simple type.
#' @title Parse The Results
#' @param ESObj An object of this class.
#' @return The object with the results, if no errors occured.
#' @usage .getESummaryResults(ESObj)
#' @aliases .getESummaryResults,ESummaryClass-method
#' @aliases .getESummaryResults
#' @author Martin Schumann
#' @export
setMethod(".getESummaryResults", "ESummaryClass", function(ESObj) {
			false <- ESObj@NCBIObj@rinterface_false
			true <- ESObj@NCBIObj@rinterface_true
			paras <- getLastResultParameter(ESObj@NCBIObj)
			# find the complex type of the result
			for (i in 1:length(paras)-1) {
				if (paras[i+1]==true) {
					ESObj <- .getComplexType(ESObj, paras[i])
				}
				if (paras[i+1]==false) {
					# this is a simple type, so simply assign it.
					ESObj <- .getSimpleType(ESObj, paras[i])
				}
				# take one step forward
				i <- i+1
			}
			# return this modified object
			ESObj
		})


