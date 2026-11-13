# Registers the print methods of the shared classes unless another package of
# the family has already registered its identical copy.
.onLoad <- function(libname, pkgname) {
  .dl_register_print()
}
