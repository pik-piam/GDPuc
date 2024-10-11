.onAttach <- function(libname, pkgname) {
  if (rlang::is_installed("madrat"))
    madrat::madratAttach(pkgname)
}

.onDetach <- function(libpath) {
  if (rlang::is_installed("madrat"))
    madrat::madratDetach(libpath)
}
