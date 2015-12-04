.onLoad <- function(lib, pkg){
   library.dynam("streer", pkg, lib)
}
.onUnload <- function(libpath)
    library.dynam.unload("streer", libpath)
