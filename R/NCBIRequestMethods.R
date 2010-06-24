# The requestof a ncbi operation.
# This will provide functions to set the request parameter.
# 
# Author: Martin Schumann
###############################################################################

## the init method for this class
setMethod("initialize", "NCBIRequest", function(.Object, ...) {
			.Object@request <- list()
			.Object
		})

## This will return the result list.
#' This method will set the given request parameter and value to the request list.
#' @param opObj An object of this class.
#' @param parameter The parameter name, which should be set.
#' @param value The value to set.
#' @return The object from the arguments, but with modified request list.
#' @usage setRequestParameter(opObj, parameter, value)
#' @aliases setRequestParameter,NCBIRequest-method
#' @aliases setRequestParameter
#' @author Martin Schumann
#' @export
setMethod("setRequestParameter", "NCBIRequest", function(opObj, parameter, value) {
			# values should never be NULL
			# if the user wants to reset the request list, create a new object
			if (is.null(value)) {
				stop("Value not set.")
			}
			tryCatch({
						if (parameter %in% names(opObj@request)) {
							# set the specific request
							opObj@request[[parameter]] <- value
						} else {
							stop()
						}
					}, error = function(ex) {
						cat("Parameter \"", parameter, "\" not found!", "\n", sep="")
					}, finally = {
						return
					})
			# return the object
			return(opObj)
		})
