# This is the class for all the ncbi stuff.
# 
# Author: Martin Schumann
###############################################################################

## the init method for this class
## proxy - takes the proxy, if any
## debug - boolean whether to debug or not
## tidy - whether do post processing on the efetch results, to remove empty elements
setMethod("initialize", "NCBIClass", function(.Object, proxy="", debug=FALSE, tidy=TRUE, ...) {
			if (proxy=="") {
				.Object@interface <- .jnew("de.hzi.infg.RInterface",Sys.getenv("http_proxy"), debug, tidy)	
			} else {
				.Object@interface <- .jnew("de.hzi.infg.RInterface",proxy, debug, tidy)
			}
			.Object@rinterface_true <- .jcall(.Object@interface,"S", "getTrue")
			.Object@rinterface_false <- .jcall(.Object@interface,"S", "getFalse")
			.Object@rinterface_breakword <- .jcall(.Object@interface,"S", "getBreakword")
			.Object@rinterface_subset <- .jcall(.Object@interface,"S", "getSubset")
			.Object@rinterface_emptyvalue <- .jcall(.Object@interface,"S", "getEmptyValue")
			.Object@rinterface_end <- .jcall(.Object@interface,"S", "getEnd")
			.Object@xml_array <- .jcall(.Object@interface,"S", "getXmlArrayIndicator")
			.Object@xml_object <- .jcall(.Object@interface,"S", "getXmlObjectIndicator")
			.Object@xml_entry <- .jcall(.Object@interface,"S", "getXmlEntryIndicator")
			.Object@xml_attr_name <- .jcall(.Object@interface,"S", "getXmlNameAttribute")
			.Object@xml_attr_length <- .jcall(.Object@interface,"S", "getXmlLengthAttribute")
			.Object
		})

#' This will set the user name and password for the proxy.
#' @title Setting The User And Password
#' @param NCBIObject The current instance of the NCBIClass.  
#' @param user The username.
#' @param password The password.
#' @usage setProxyUser(NCBIObject, user, password="")
#' @aliases setProxyUser,NCBIClass-method
#' @aliases setProxyUser
#' @author Martin Schumann
#' @exportMethod setProxyUser
setMethod("setProxyUser", "NCBIClass", function(NCBIObject, user, password="") {
			.jcall(NCBIObject@interface, "V", "setProxyUser", toString(user), toString(password))
		})

## This will return the operations, which are provided by the RInterface for the NCBI web service.
## If the argument is FALSE, then a vector with the names will be returned.
## The default behaviour is to print the names.
#' Get a list of available operations from the java interface.
#' @title Available EUtils Operations
#' @param NCBIObject The current instance of the NCBIClass. 
#' @param printNames Whether to print the names or return a vector.
#' @return Either a vector with the names of the operations or nothing.
#' @usage getOperations(NCBIObject, printNames=TRUE)
#' @aliases getOperations,NCBIClass-method
#' @aliases getOperations
#' @author Martin Schumann
#' @exportMethod getOperations
setMethod("getOperations", "NCBIClass", function(NCBIObject, printNames=TRUE) {
			vec <- .jcall(NCBIObject@interface,"[S", "getOperations")
			dim(vec) = c(length(vec),1)
			if(printNames) {
				for (i in seq(along=vec)) {
					print(paste(i,vec[i]))
				}
			} else {
				return(vec)
			}
		})

## This will return the databases, which are provided by the Plugin for the run_eFetch operation 
## of the  NCBI web service. If the argument is FALSE, then a vector with the names will be returned.
## The default behaviour is to print the names.
#' Retrieves all available databases for the EFetch operation from the java interface.
#' @title Get All EFetch Database Names
#' @param NCBIObject The current instance of the NCBIClass. 
#' @param printNames Whether to print or to return a vector.
#' @return A vector with the names of the operation or nothing.
#' @usage getEFetchDatabases(NCBIObject, printNames=TRUE)
#' @aliases getEFetchDatabases,NCBIClass-method
#' @aliases getEFetchDatabases
#' @author Martin Schumann
#' @exportMethod getEFetchDatabases
setMethod("getEFetchDatabases", "NCBIClass", function(NCBIObject, printNames=TRUE) {
			vec <- .jcall(NCBIObject@interface,"[S", "getEFetchDatabases")
			dim(vec) = c(length(vec),1)
			if(printNames) {
				for (i in seq(along=vec)) {
					print(paste(i,vec[i]))
				}
			} else {
				return(vec)
			}
		})

## This should return the parameters of a specific request function.
## Case insensitive!
#' Retrieve a list of request parameter names for the given operation.
#' @title Get Parameter For The Request
#' @param NCBIObject The current instance of the NCBIClass.
#' @param req The name of the operation the parameter names should be retrieved.
#' @return A vector with the request parameter names for the given function.
#' @usage getRequestParameter(NCBIObject, req="")
#' @aliases getRequestParameter,NCBIClass-method
#' @aliases getRequestParameter
#' @author Martin Schumann
#' @exportMethod getRequestParameter
setMethod("getRequestParameter", "NCBIClass", function(NCBIObject, req="") {
			# boolean whether the parameter is a ncbi operation or not
			isFunc <- FALSE;
			# check whether this is a function of eutils
			if (req!="") {
				vec <- getOperations(NCBIObject, FALSE)
				for (i in seq(along = vec)) {
					if (tolower(unlist(strsplit(vec[i],"_"))[2]) == tolower(req)) {
						isFunc <- TRUE
					} else if (tolower(vec[i]) == tolower(req)) {
						isFunc <- TRUE
					}
				}
			}
			# get the parameters for this request function
			if (isFunc) {
				paras <- .jcall(NCBIObject@interface,"[S", "getRequestParameter", toString(req))
			} else {
				stop("Please provide a function of eutils.")	
			}
			return(paras)
		})

## This will get the parameters of the last result from the ncbi object.
## Returns NULL, if there is no last result.
#' This will get the parameter of the last result from the NCBIClass object. 
#' The returned vector will contain the first level result parameter. This means, 
#' that each parameter could contain more parameter names, if it is a complex type.
#' @title Get Last Results Parameter
#' @param NCBIObject The current instance of the NCBIClass. 
#' @return A vector with the parameter names of the last result. Or NULL, if there are no last results.
#' @usage getLastResultParameter(NCBIObject)
#' @aliases getLastResultParameter,NCBIClass-method
#' @aliases getLastResultParameter
#' @author Martin Schumann
#' @seealso \code{\link[=getResultParameterByName,NCBIClass-method]{getResultParameterByName}}
#' @seealso \code{\link[=NCBIResult-class]{NCBIResult}}
#' @exportMethod getLastResultParameter
setMethod("getLastResultParameter", "NCBIClass", function(NCBIObject) {
			return(.jcall(NCBIObject@interface,"[S","getResultParameter"))
		})


## Returns further parameter of a specific complex type.
## Returns NULL, if this wasn't a complex type or the parameter doesn't exists
#' Returns further parameter of a specific complex type and NULL, if name wasn't a complex type.
#' The function only returns the parameter for a result parameter from the first level. If there are further complex types contained,
#' the results (see \code{\link[=NCBIResult-class]{NCBIResult}}) list will show them.
#' @title Get First Level Parameter By Name
#' @param NCBIObject The current instance of the NCBIClass. 
#' @param name The name of the complex type, the parameter names should be returned for.
#' @return The parameter names for the given complex type. The result can contain more complex types.
#' @usage getResultParameterByName(NCBIObject, name)
#' @aliases getResultParameterByName,NCBIClass-method
#' @aliases getResultParameterByName
#' @author Martin Schumann
#' @exportMethod getResultParameterByName
setMethod("getResultParameterByName", "NCBIClass", function(NCBIObject, name) {
			return(.jcall(NCBIObject@interface,"[S","getResultParameterByName", toString(name)))
		})

## Returns the request of the given object as a vector
## The vector will look like: c("parameter-1 name", "parameter-1 value, "parameter-2 name", "parameter-2 value, ..., "parameter-n name", "parameter-n value)
## This structure is required for the interfaces of each web service operation in the java part
#' Returns the request of the given object as a vector. The vector will look like: 
#' c("parameter-1 name", "parameter-1 value, "parameter-2 name", "parameter-2 value, ..., "parameter-n name", "parameter-n value).
#' This structure is required for the interfaces of each web service operation in the java part.
#' The returned vector will be used as argument for the java interface.
#' @title Parsing The Request
#' @param thisObj Object from the NCBIClass to access the interface.
#' @param obj An object of the operation classes.
#' @return A vector with the request parameters and their values.
#' @usage .parseRequest(thisObj, obj)
#' @aliases .parseRequest,NCBIClass-method
#' @aliases .parseRequest
#' @seealso \code{\link[=NCBIRequest-class]{NCBIRequest}}
#' @author Martin Schumann
#' @exportMethod .parseRequest
setMethod(".parseRequest", "NCBIClass", function(thisObj, obj) {
			request <- obj@request
			argAr <- NULL
			for (i in names(request)) {
				# if this is null, we have to add an empty character instead
				# to preserve the structure of the arguments
				if (is.null(request[[i]])) {
					argAr <- c(argAr, i, "")
				} else {
					if (length(request[[i]]) == 1) {
						argAr <- c(argAr, i, request[[i]])
					} else {
						tmp <- NULL
						# an array of this request parameter
						for (k in 1:length(request[[i]])) {
							element <- request[[i]][[k]]
							if (k == length(request[[i]])) {
								# last element
								tmp <- paste(tmp, element, sep="")
							} else {
								tmp <- paste(tmp, element, thisObj@rinterface_breakword, sep="")
							}
						}
						argAr <- c(argAr, i, tmp)
					}
				}
			}
			return(argAr)
		})


