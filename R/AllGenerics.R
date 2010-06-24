# This contains all the setGeneric calls.
# 
# Author: Martin Schumann
###############################################################################

## the generics of NCBI:
## setProxyUser function
setGeneric("setProxyUser", function(NCBIObject, user, password="") {standardGeneric("setProxyUser")})
## getOperations function
setGeneric("getOperations", function(NCBIObject, printNames=TRUE) {standardGeneric("getOperations")})
## getEFetchDatabases function
setGeneric("getEFetchDatabases", function(NCBIObject, printNames=TRUE) {standardGeneric("getEFetchDatabases")})
# getRequestParameter function
setGeneric("getRequestParameter", function(NCBIObject, req="") {standardGeneric("getRequestParameter")})
# getLastResultParameter - somehow this can not be named "getResultParameter"
setGeneric("getLastResultParameter", function(NCBIObject) {standardGeneric("getLastResultParameter")})
# getResultParmaterByName
setGeneric("getResultParameterByName", function(NCBIObject, name) {standardGeneric("getResultParameterByName")})
# .parseRequest
setGeneric(".parseRequest", function(thisObj, obj) {standardGeneric(".parseRequest")})

## the generics of NCBIResults:
## .getComplexType function
#' Generic for getComplexType.
#' @param opObj Operation object.
#' @param parName Parameter name.
#' @return The modified operation obect.
#' @author Martin Schumann
setGeneric(".getComplexType", function(opObj, parName) {standardGeneric(".getComplexType")})
## .getSimpleType function
setGeneric(".getSimpleType", function(opObj, parName) {standardGeneric(".getSimpleType")})
## .recursiveFunction function
setGeneric(".recursiveFunction", function(opObj, xmldoc, xml_array, xml_object, xml_entry, xml_attr_name, xml_attr_length) {standardGeneric(".recursiveFunction")})
## .getResults function
setGeneric("getResults", function(obj) {standardGeneric("getResults")})

## the generics of NCBIRequest
## setRequestParameter
setGeneric("setRequestParameter", function(opObj, parameter, value) {standardGeneric("setRequestParameter")})

## the generics of EInfo:
## requestEInfo function
setGeneric(".requestEInfoMethod", function(EInfoObj) {standardGeneric(".requestEInfoMethod")})
## getResults function
setGeneric(".getEInfoResults", function(EInfoObj) {standardGeneric(".getEInfoResults")})

## the generics of EGQuery:
## requestEGQueryMethod function
setGeneric(".requestEGQueryMethod", function(EGQObj) {standardGeneric(".requestEGQueryMethod")})
## getEGQueryResults function
setGeneric(".getEGQueryResults", function(EGQObj) {standardGeneric(".getEGQueryResults")})

## the generics of EFetch:
## requestEFetchMethod function
setGeneric(".requestEFetchMethod", function(EFObj) {standardGeneric(".requestEFetchMethod")})
## getEFetchResults function
setGeneric(".getEFetchResults", function(EFObj) {standardGeneric(".getEFetchResults")})

## the generics of ESummary:
## requestESummaryMethod function
setGeneric(".requestESummaryMethod", function(ESObj) {standardGeneric(".requestESummaryMethod")})
## getESummaryResults function
setGeneric(".getESummaryResults", function(ESObj) {standardGeneric(".getESummaryResults")})

## the generics of ESpell:
## requestESpellMethod function
setGeneric(".requestESpellMethod", function(ESObj) {standardGeneric(".requestESpellMethod")})
## getESpellResults function
setGeneric(".getESpellResults", function(ESObj) {standardGeneric(".getESpellResults")})

## the generics of ESearch:
## requestESearchMethod function
setGeneric(".requestESearchMethod", function(ESObj) {standardGeneric(".requestESearchMethod")})
## getESearchResults function
setGeneric(".getESearchResults", function(ESObj) {standardGeneric(".getESearchResults")})

## the generics of EPost:
## requestEPostMethod function
setGeneric(".requestEPostMethod", function(EPObj) {standardGeneric(".requestEPostMethod")})
## getEPostResults function
setGeneric(".getEPostResults", function(EPObj) {standardGeneric(".getEPostResults")})

## the generics of ELink:
## requestELinkMethod function
setGeneric(".requestELinkMethod", function(ELObj) {standardGeneric(".requestELinkMethod")})
## getResults function
setGeneric(".getELinkResults", function(ELObj) {standardGeneric(".getELinkResults")})

