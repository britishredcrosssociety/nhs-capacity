.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "www",
    system.file(
      "www",
      package = "nhscapacity"
    )
  )
}

.onUnload <- function(libname, pkgname) {
   shiny::removeResourcePath("www")
}