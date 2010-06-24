# The result of a request to the ncbi web service.
# This will hold all the results regardless of the called operation.
# 
# Author: Martin Schumann
###############################################################################

## the init method for this class
setMethod("initialize", "NCBIResult", function(.Object, ...) {
			.Object@results <- list()
			.Object
		})

## This will return the result list.
#' Returns the results of the given operation object.
#' @title The Results
#' @param obj An object of the operation classes.
#' @return The results list from the given operation object.
#' @usage getResults(obj)
#' @aliases getResults,NCBIResult-method
#' @aliases getResults
#' @author Martin Schumann
#' @export
setMethod("getResults", "NCBIResult", function(obj) {
			# return the results of this object
			return(obj@results)
		})

# The names of each list will be set at the end, if there is an entry
# each sublist will be named as it is created
#' Creates the complex structure of the results list from a given xml document.
#' This function works recursive and only should be used from within this class.
#' @title Recursive Function
#' @param opObj An object of this class.
#' @param xmldoc The xml document, which should be parsed.
#' @param xml_array Keyword for an array in the xml document.
#' @param xml_object Keyword for an object in the xml document.
#' @param xml_entry Keyword for an entry in the xml document.
#' @param xml_attr_name Keyword for the name attribute in the xml document.
#' @param xml_attr_length Keyword for the length attribute in the xml document.
#' @return The complex list with the results from the xml document.
#' @usage .recursiveFunction(opObj, xmldoc, xml_array, xml_object, xml_entry, xml_attr_name, xml_attr_length)
#' @aliases .recursiveFunction,NCBIResult-method
#' @aliases .recursiveFunction
#' @author Martin Schumann
#' @export
setMethod(".recursiveFunction", "NCBIResult", function(opObj, xmldoc, xml_array, xml_object, xml_entry, xml_attr_name, xml_attr_length) {
			l <- NULL
			if (is.null(xmldoc)) {
				return()
			} else if (xmldoc$name == xml_array) {
				# name will be the same, because this is an array, so we take the name from the first element
				name <- xmldoc[[1]]$attributes[[xml_attr_name]]
				# create a list with the name of the array elements
				l[[name]] <- NULL
				# each element has to be appended to this list
				for (i in 1:length(xmldoc)) {
					if (name %in% names(l)) {
						# pass the element of the current array element
						tmpL <- .recursiveFunction(opObj, xmldoc[[i]], xml_array, xml_object, xml_entry, xml_attr_name, xml_attr_length)
						# append with c operator
						l[[name]] <- c(l[[name]], tmpL)
					} else {
						l[[length(l)+1]] <- .recursiveFunction(opObj, xmldoc[[i]], xml_array, xml_object, xml_entry, xml_attr_name, xml_attr_length)
						names(l) <- c(names(l)[1:(length(l)-1)], name)
					}
				}
				# after that, the list l will be filled and we return it
				return(l)
			} else if (xmldoc$name == xml_object) {
				ll <- NULL
				# create a list with the name of the root of the current xml document
				mainName <- xmldoc$attributes[[xml_attr_name]]
				# for each object from the xml, we create a list with the name of the object
				for (j in 1:length(xmldoc)) {
					if (mainName %in% names(ll)) {
						tmpL <- .recursiveFunction(opObj, xmldoc[[j]], xml_array, xml_object, xml_entry, xml_attr_name, xml_attr_length)
						# append with c operator
						ll[[mainName]] <- c(ll[[mainName]], tmpL)
					} else {
						ll[[length(ll)+1]] <- .recursiveFunction(opObj, xmldoc[[j]], xml_array, xml_object, xml_entry, xml_attr_name, xml_attr_length)
						names(ll) <- c(names(ll)[1:(length(ll)-1)], mainName)
					}
				}
				# append the created list to the main list in this recursion and return it
				return(ll)
			} else if (xmldoc$name == xml_entry) {
				l <- list()
				name <- xmldoc$attributes[[xml_attr_name]]
				l[[name]] <- xmlValue(xmldoc)
				return(l)
			}
		})

## This will retrieve the complex type identified by "parName" from
## the java interface. 
## resObj is this class itself.
## The argument "opObj" is a representative of the calling class.
#' This will retrieve the complex type identified by "parName" from
#' the java interface. 
#' @title Complex Type
#' @param opObj An object of this class.
#' @param parName The parameter name of the complex type, which should be retrieved.
#' @return The same object as the arguments, but with a modified results list.
#' @usage .getComplexType(opObj, parName)
#' @aliases .getComplexType,NCBIResult-method
#' @aliases .getComplexType
#' @author Martin Schumann
#' @export
setMethod(".getComplexType", "NCBIResult", function(opObj, parName) {
			# the breakword to split the result strings
			breakword <- opObj@NCBIObj@rinterface_breakword
			# subset keyword
			subset <- opObj@NCBIObj@rinterface_subset
			# end keyword
			end <- opObj@NCBIObj@rinterface_end
			# replace the special character "{|}" with escaped ones
			breakword <- gsub("\\{", "\\\\{", breakword)
			breakword <- gsub("\\}", "\\\\}", breakword)
			subset <- gsub("\\{", "\\\\{", subset)
			subset <- gsub("\\}", "\\\\}", subset)
			end <- gsub("\\{", "\\\\{", end)
			end <- gsub("\\}", "\\\\}", end)
			# this will return a complex type with all its parameter and values
			tmp <- .jcall(opObj@NCBIObj@interface,"[S","getComplexType", toString(parName))
			if (is.null(tmp)) {
				return()
			}
			# if the result is "empty" (tmp contains this string), simply add it to the list and return
			if (length(tmp)==1) {
				l <- opObj@results
				l[[length(l)+1]] <- tmp
				names(l) <- c(names(l)[1:(length(l)-1)], parName)
				# back to NCBIResult object
				opObj@results <- l
				return(opObj)
			}
			splitRes <- NULL
			# create a new list with the names of the parameter from the first entry
			parameterNames <- unlist(strsplit(tmp[1], breakword))
			finalResults <- vector("list", length(parameterNames))
			names(finalResults) <- parameterNames
			# switch to another method, due to laziness, in case of an xml string as return value for the getComplexType
			if (any(grep("^<", tmp[2]))) {
				xml_array <- opObj@NCBIObj@xml_array
				xml_object <- opObj@NCBIObj@xml_object
				xml_entry <- opObj@NCBIObj@xml_entry
				xml_attr_name <- opObj@NCBIObj@xml_attr_name
				xml_attr_length <- opObj@NCBIObj@xml_attr_length
				recList <- NULL
				for (i in 2:length(tmp)) {
					# create the xml document with the parser
					xmldoc <- xmlRoot(xmlTreeParse(tmp[i], asText=TRUE))
					opObj@xmldoc <- xmldoc
#					print(xmldoc)
					recList <- .recursiveFunction(opObj, xmldoc, xml_array, xml_object, xml_entry, xml_attr_name, xml_attr_length)
					# in most cases, the recList will be of the length 1
					if (length(recList) == 1) {
						finalResults[[i-1]] <- recList[[1]]
					} else {
						finalResults[[i-1]] <- recList
					}
				}
				l <- opObj@results
				l[[length(l)+1]] <- finalResults
				names(l) <- c(names(l)[1:(length(l)-1)], parName)
				# back to NCBIResult object
				opObj@results <- l
				return(opObj)
			}
			# walk through an array of strings
			# the first string contains the names of the returned parameter
			# and the following string contain the values of these paramters
			for (i in 2:length(tmp)) {
				# split the string with the breakword
				splitRes <- unlist(strsplit(tmp[i], breakword))
				fRes <- finalResults[[i-1]]
				# this list will be added to the finalResults at the end
				theOneList <- list()
				tmpList <- list()
				resetList <- FALSE
				# iterate through the splitResult
				for (k in splitRes) {
					# if the current string contains the subset keyword, we have to split again
					if (any(grep(subset, k))) {
						if (any(grep(end, k))) {
							resetList <- TRUE
							# remove this "end" from the string
							k <- gsub(end, "", k)
						}
						# subset split
						ssS <- unlist(strsplit(k, subset))
						lSplit <- length(ssS)
						if (lSplit > 2) {
							# a lot of elements, hopefully only 3
							l <- list()
							l[[length(l)+1]] <- ssS[1]
							names(l) <- c(names(l)[1:(length(l)-1)], ssS[2])
							# append this list to tmpList with the name of ssS[3], if there isn't already one with this name
							if (ssS[3] %in% names(tmpList)) {
								# take the existing list from tmpList with ssS[3] as name
								tmpList[[ssS[3]]][[length(tmpList[[ssS[3]]])+1]] <- l
							} else {
								tmpList[[length(tmpList)+1]] <- l
								names(tmpList) <- c(names(tmpList)[1:(length(tmpList)-1)], ssS[3])
							}
						} else {
							tmpList[[length(tmpList)+1]] <- ssS[1]
							names(tmpList) <- c(names(tmpList)[1:(length(tmpList)-1)], ssS[2])
						}
						
						# this will end the current complex type
						if (resetList) {
							theOneList[[length(theOneList)+1]] <- tmpList
							# reset tmpList
							tmpList <- list()
							resetList <- FALSE
						}
					} else {
						# else we simply add k to the list
						theOneList[[length(theOneList)+1]] <- k
					}
				}
				# write back the list to position i-1
				finalResults[[i-1]] <- theOneList
			}
			l <- opObj@results
			l[[length(l)+1]] <- finalResults
			names(l) <- c(names(l)[1:(length(l)-1)], parName)
			# back to NCBIResult object
			opObj@results <- l
			return(opObj)
		})

## This will retrieve the simple type identified by "parName" from
## the java interface.
## resObj is this class itself.
## The argument "opObj" is a representative of the calling class.
#' This will retrieve the simple type identified by "parName" from
#' the java interface.
#' @title Simple Type
#' @param opObj Object if this class.
#' @param parName The parameter name for the simple type to retrieve.
#' @return The object from the arguments, but with modified results list.
#' @usage .getSimpleType(opObj, parName)
#' @aliases .getSimpleType,NCBIResult-method
#' @aliases .getSimpleType
#' @author Martin Schumann
#' @export
setMethod(".getSimpleType", "NCBIResult", function(opObj, parName) {
			# temporarily save the list to l, for better handling
			l <- opObj@results
			l[[length(l)+1]] <- .jcall(opObj@NCBIObj@interface,"S","getSimpleType", toString(parName))
			names(l) <- c(names(l)[1:(length(l)-1)], parName)
			# back to NCBIResult object
			opObj@results <- l
			return(opObj)
		})
