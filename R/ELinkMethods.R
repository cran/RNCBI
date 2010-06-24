# This is the class for ELink.
# It holds the request parameter and the results, after the request was fired.
# 
# Author: Martin Schumann
###############################################################################

# the user has to specify the request parameter in the request variable from this class
## the init method for this class
setMethod("initialize", "ELinkClass", function(.Object, NCBIObj) {
			# fill the request list with parameter names
			requestVector <- getRequestParameter(NCBIObj, "elink")
			# create an empty list with the length of the requestVector
			.Object@request <- vector("list", length(requestVector))
			# assign the names of the request parameter to the list
			names(.Object@request) <- requestVector
			.Object@NCBIObj <- NCBIObj
#			.Object@results <- new("NCBIResult")
			.Object
		})

## This runs run_eLink operation.
#' Calls the appropriate interface method for this operation and stores the results.
#' \section{Warning}{
#' This function should not be called by the user.
#' }
#' @param ELObj An object of this class.
#' @return The modified object with the results, if no errors occured.
#' @usage .requestELinkMethod(ELObj)
#' @seealso \code{\link[=requestELink]{requestELink}}
#' @aliases .requestELinkMethod,ELinkClass-method
#' @aliases .requestELinkMethod
#' @author Martin Schumann
#' @export
setMethod(".requestELinkMethod", "ELinkClass", function(ELObj) {
			NCBIObj <- ELObj@NCBIObj
			# delete previous results
			ELObj@results <- list()
			# parse the request list and create a vector
			argArray <- .parseRequest(ELObj@NCBIObj, ELObj)
			.jcall(NCBIObj@interface,"V","ELinkInterface",.jarray(argArray))
			if (!.jcall(NCBIObj@interface,"Z","isErrorCaused")) { 
				# now call the function to save the results in the NCBIResult object
				ELObj <- .getELinkResults(ELObj)
			}
			# return modified object
			ELObj
		})

## This will get the results of the request from the java interface
#' Parses the results parameter names and calls the appropriate function of \code{\link[=NCBIResult-class]{NCBIResult}}, 
#' depending on whether the parameter is a complex type or a simple type.
#' @title Parse The Results
#' @param ELObj An object of this class.
#' @return The modified object with the results, if no errors occured.
#' @usage .getELinkResults(ELObj)
#' @aliases .getELinkResults,ELinkClass-method
#' @aliases .getELinkResults
#' @author Martin Schumann
#' @export
setMethod(".getELinkResults", "ELinkClass", function(ELObj) {
			false <- ELObj@NCBIObj@rinterface_false
			true <- ELObj@NCBIObj@rinterface_true
			paras <- getLastResultParameter(ELObj@NCBIObj)
			# find the complex type of the result
			for (i in 1:length(paras)-1) {
				if (paras[i+1]==true) {
					ELObj <- .getComplexType(ELObj, paras[i])
				}
				if (paras[i+1]==false) {
					# this is a simple type, so simply assign it.
					ELObj <- .getSimpleType(ELObj, paras[i])
				}
				# take one step forward
				i <- i+1
			}
			# return this modified object
			ELObj
		})


