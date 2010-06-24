# This is the class for ESpell.
# It holds the request parameter and the results, after the request was fired.
# 
# Author: Martin Schumann
###############################################################################

# the user has to specify the request parameter in the request variable from this class
## the init method for this class
setMethod("initialize", "ESpellClass", function(.Object, NCBIObj) {
			# fill the request list with parameter names
			requestVector <- getRequestParameter(NCBIObj, "espell")
			# create an empty list with the length of the requestVector
			.Object@request <- vector("list", length(requestVector))
			# assign the names of the request parameter to the list
			names(.Object@request) <- requestVector
			.Object@NCBIObj <- NCBIObj
#			.Object@results <- new("NCBIResult")
			.Object
		})

## This runs run_eSpell operation.
#' Calls the appropriate interface method for this operation and stores the results.
#' \section{Warning}{
#' This function should not be called by the user.
#' }
#' @param ESObj An object of this class.
#' @return The object with the results, if no errors occured.
#' @usage .requestESpellMethod(ESObj)
#' @seealso \code{\link[=requestESpell]{requestESpell}}
#' @aliases .requestESpellMethod,ESpellClass-method
#' @aliases .requestESpellMethod
#' @author Martin Schumann
#' @export
setMethod(".requestESpellMethod", "ESpellClass", function(ESObj) {
			NCBIObj <- ESObj@NCBIObj
			# delete previous results
			ESObj@results <- list()
			# parse the request list and create a vector
			argArray <- .parseRequest(ESObj@NCBIObj, ESObj)
			.jcall(NCBIObj@interface,"V","ESpellInterface",.jarray(argArray))
			if (!.jcall(NCBIObj@interface,"Z","isErrorCaused")) { 
				# now call the function to save the results in the NCBIResult object
				ESObj <- .getESpellResults(ESObj)
			}
			# return modified object
			ESObj
		})

## This will get the results of the request from the java interface
#' Parses the results parameter names and calls the appropriate function of \code{\link[=NCBIResult-class]{NCBIResult}}, 
#' depending on whether the parameter is a complex type or a simple type.
#' @title Parse The Results
#' @param ESObj An object of this class.
#' @return The object with the results.
#' @usage .getESpellResults(ESObj)
#' @aliases .getESpellResults,ESpellClass-method
#' @aliases .getESpellResults
#' @author Martin Schumann
#' @export
setMethod(".getESpellResults", "ESpellClass", function(ESObj) {
			false <- ESObj@NCBIObj@rinterface_false
			true <- ESObj@NCBIObj@rinterface_true
			paras <- getLastResultParameter(ESObj@NCBIObj)
			# find the complex type of the result
			for (i in 1:length(paras)-1) {
				if (paras[i+1]==true) {
#					print(paras[i])
					ESObj <- .getComplexType(ESObj, paras[i])
				}
				if (paras[i+1]==false) {
					# this is a simple type, so simply assign it.
#					print(paras[i])
					ESObj <- .getSimpleType(ESObj, paras[i])
				}
				# take one step forward
				i <- i+1
			}
			# return this modified object
			ESObj
		})



