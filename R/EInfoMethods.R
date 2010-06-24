# This is the class for EInfo.
# It holds the request parameter and the results, after the request was fired.
# The results slot comes from the NCBIResult class.
# 
# Author: Martin Schumann
###############################################################################

# the user has to specify the request parameter in the request variable from this class
## the init method for this class
setMethod("initialize", "EInfoClass", function(.Object, NCBIObj) {
			# fill the request list with parameter names
			requestVector <- getRequestParameter(NCBIObj, "einfo")
			# create an empty list with the length of the requestVector
			.Object@request <- vector("list", length(requestVector))
			# assign the names of the request parameter to the list
			names(.Object@request) <- requestVector
			.Object@NCBIObj <- NCBIObj
			.Object@isDBInfo <- FALSE
#			.Object@results <- new("NCBIResult")
			.Object
		})

## This runs run_eInfo operation.
## If all the parameters are empty, only a list of data base names is returned.
#' Calls the appropriate interface method for this operation and stores the results.
#' \section{Warning}{
#' This function should not be called by the user.
#' }
#' @param EInfoObj An object of this class. 
#' @return The object with the results, if no errors occured.
#' @usage .requestEInfoMethod(EInfoObj)
#' @seealso \code{\link[=requestEInfo]{requestEInfo}}
#' @aliases .requestEInfoMethod,EInfoClass-method
#' @aliases .requestEInfoMethod
#' @author Martin Schumann
#' @export
setMethod(".requestEInfoMethod", "EInfoClass", function(EInfoObj) {
			NCBIObj <- EInfoObj@NCBIObj
			# delete previous results
			EInfoObj@results <- list()
			# parse the request list and create a vector for the EInfoInterface
			# c("db",toString(EInfoObj@db),"tool",toString(EInfoObj@tool),"email",toString(EInfoObj@email))
			argArray <- .parseRequest(EInfoObj@NCBIObj, EInfoObj)
			.jcall(NCBIObj@interface,"V","EInfoInterface",.jarray(argArray))
			if (!.jcall(NCBIObj@interface,"Z","isErrorCaused")) { 
				# check whether this is a DBInfo or an array of data base names
				EInfoObj@isDBInfo <- .jcall(NCBIObj@interface,"Z","isDbInfo")
				# now call the function to save the results in the NCBIResult object
				EInfoObj <- .getEInfoResults(EInfoObj)
			}
			# return modified object
			EInfoObj
		})

## This will get the results of the request from the java interface
#' Parses the results parameter names and calls the appropriate function of \code{\link[=NCBIResult-class]{NCBIResult}}, 
#' depending on whether the parameter is a complex type or a simple type.
#' @title Parse The Results
#' @param EInfoObj An object of this class. 
#' @return The object with the results, if no errors occured.
#' @usage .getEInfoResults(EInfoObj)
#' @aliases .getEInfoResults,EInfoClass-method
#' @aliases .getEInfoResults
#' @author Martin Schumann
#' @export
setMethod(".getEInfoResults", "EInfoClass", function(EInfoObj) {
			false <- EInfoObj@NCBIObj@rinterface_false
			true <- EInfoObj@NCBIObj@rinterface_true
			if(EInfoObj@isDBInfo) {
				paras <- getLastResultParameter(EInfoObj@NCBIObj)
				# find the complex type of the result
				for (i in 1:length(paras)-1) {
					if (paras[i+1]==true) {
						EInfoObj <- .getComplexType(EInfoObj, paras[i])
					}
					if (paras[i+1]==false) {
						# this is a simple type, so simply assign it.
						EInfoObj <- .getSimpleType(EInfoObj, paras[i])
					}
					# take one step forward
					i <- i+1
				}
			} else {
				# an array it is, append this to the results
				res <- EInfoObj@results
				res[[length(res)+1]] <- .jcall(EInfoObj@NCBIObj@interface,"[S","getDbNames")
				names(res) <- c(names(res)[1:(length(res)-1)], "DBList")
				EInfoObj@results <- res
			}
			# return this modified object
			EInfoObj
		})
