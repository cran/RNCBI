# this function will be run, after the package was loaded
.onLoad <- function(libname,pkgname) {
	# ensures to start the JVM
#	.jpackage(pkgname, lib.loc=libname)
	.jpackage(pkgname)
} 
