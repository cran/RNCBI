# This contains all the setClass calls.
#
# Author: Martin Schumann
###############################################################################

# defining the class, with interface as any object
# interface will be an instance of the RInterface class from java

#' This class holds the interface to the java part. Furthermore it contains some constants from the RInterface class from java.
#' @title This Is The Main Class Of The Package
#' @slot interface The interface to the java part.
#' @slot rinterface_true A character value for TRUE.
#' @slot rinterface_false A character value for FALSE.
#' @slot rinterface_breakword The keyword to split strings from java.
#' @slot rinterface_subset The keyword to split strings into subsets.
#' @slot rinterface_emptyvalue The keyword for an empty value.
#' @slot rinterface_end The keyword which indicates an end of a subset.
#' @slot xml_array Keyword for an array in the returned xml from the java interface.
#' @slot xml_object Keyword for an object in the returned xml from the java interface.
#' @slot xml_entry Keyword for an entry in the returned xml from the java interface.
#' @slot xml_attr_name Keyword for the name attribute in the returned xml from the java interface.
#' @slot xml_attr_length Keyword for the length attribute in the returned xml from the java interface.
#' @aliases NCBIClass-class
#' @author Martin Schumann
#' @exportClass NCBIClass
setClass("NCBIClass", representation(interface="ANY", rinterface_true="character", rinterface_false="character", rinterface_breakword="character", 
				rinterface_subset="character", rinterface_emptyvalue="character", rinterface_end="character", xml_array="character", xml_object="character",
				xml_entry="character", xml_attr_name="character", xml_attr_length="character"))
#' This class represents the results from java interface. Contains a list with the results and the xml document from the java interface.
#' @title Represents The Results From Java
#' @slot results The results list. Mostly very complex.
#' @slot xmldoc The xml document from the java interface. If this is empty, no xml document was returned from the interface.
#' @aliases NCBIResult-class
#' @author Martin Schumann
#' @exportClass NCBIResult
setClass("NCBIResult", representation(results="list", xmldoc="ANY"))
#' This class represents the request to the NCBI webservice. This is a simple list, which names are the request parameter for each operation. In case of the
#' @title Represents The Request To The NCBI Webservice
#' EFetch operation, this list has to be initiated after the database was specified.
#' @slot request The request list with the parameter names.
#' @aliases NCBIRequest-class
#' @author Martin Schumann
#' @exportClass NCBIRequest
setClass("NCBIRequest", representation(request="list"))
#' This class represents the EInfo operation. It contains NCBIResult and NCBIRequest to hold the results and the request respectively.
#' The EInfo operation contains a logical value to indicate whether the returned result is a DBInfo or not.
#' @title Represents The EInfo Operation
#' @slot NCBIObj This holds the NCBI object, which contains all the information that are required to connect to the Interface.
#' @slot isDBInfo Whether the result is an information about a database or not.
#' @seealso \code{\link{requestEInfo}}
#' @aliases EInfoClass-class
#' @author Martin Schumann
#' @exportClass EInfoClass
setClass("EInfoClass", representation(NCBIObj="NCBIClass", isDBInfo="logical"), contains=c("NCBIResult", "NCBIRequest"))
#' This class represents the EGQuery operation. It contains NCBIResult and NCBIRequest to hold the results and the request respectively.
#' @title Represents The EGQuery Operation
#' @slot NCBIObj This holds the NCBI object, which contains all the information that are required to connect to the Interface.
#' @seealso \code{\link{requestEGQuery}}
#' @aliases EGQueryClass-class
#' @author Martin Schumann
#' @exportClass EGQueryClass
setClass("EGQueryClass", representation(NCBIObj="NCBIClass"), contains=c("NCBIResult", "NCBIRequest"))
#' This class represents the EFetch operation. It contains NCBIResult and NCBIRequest to hold the results and the request respectively.
#' @title Represents The EFetch Operation
#' @aliases EFetchClass-class
#' @author Martin Schumann
#' @exportClass EFetchClass
setClass("EFetchClass", representation(NCBIObj="NCBIClass"), contains=c("NCBIResult", "NCBIRequest"))
#' This class represents the ESummary operation. It contains NCBIResult and NCBIRequest to hold the results and the request respectively.
#' @title Represents The ESummary Operation
#' @aliases ESummaryClass-class
#' @author Martin Schumann
#' @exportClass ESummaryClass
setClass("ESummaryClass", representation(NCBIObj="NCBIClass"), contains=c("NCBIResult", "NCBIRequest"))
#' This class represents the ESpell operation. It contains NCBIResult and NCBIRequest to hold the results and the request respectively.
#' @title Represents The ESpell Operation
#' @aliases ESpellClass-class
#' @author Martin Schumann
#' @exportClass ESpellClass
setClass("ESpellClass", representation(NCBIObj="NCBIClass"), contains=c("NCBIResult", "NCBIRequest"))
#' This class represents the ESearch operation. It contains NCBIResult and NCBIRequest to hold the results and the request respectively.
#' @title Represents The ESearch Operation
#' @aliases ESearchClass-class
#' @author Martin Schumann
#' @exportClass ESearchClass
setClass("ESearchClass", representation(NCBIObj="NCBIClass"), contains=c("NCBIResult", "NCBIRequest"))
#' This class represents the EPost operation. It contains NCBIResult and NCBIRequest to hold the results and the request respectively.
#' @title Represents The EPost Operation
#' @aliases EPostClass-class
#' @author Martin Schumann
#' @exportClass EPostClass
setClass("EPostClass", representation(NCBIObj="NCBIClass"), contains=c("NCBIResult", "NCBIRequest"))
#' This class represents the ELink operation. It contains NCBIResult and NCBIRequest to hold the results and the request respectively.
#' @title Represents The ELink Operation
#' @aliases ELinkClass-class
#' @author Martin Schumann
#' @exportClass ELinkClass
setClass("ELinkClass", representation(NCBIObj="NCBIClass"), contains=c("NCBIResult", "NCBIRequest"))
