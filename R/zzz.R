
.onLoad <- function(libname, pkgname) {
    ## Load font tables once
    assign("sysFonts", sys_fonts(), envir=layoutEngineEnv)
    assign("fontTable", fonttable(), envir=layoutEngineEnv)

    ## Initialise package options
    ## (backend packages will override this)
    options("layoutEngine.backend"=nullEngine)
}
