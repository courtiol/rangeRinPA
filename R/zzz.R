#' Provide some basic information about the code of the package
#'
#' This is a function for development purpose only. This function provides the
#' number of functions and lines of code of the package. The results depend on
#' whether the function is called from the compiled or the raw package. When
#' called from the compiled package, the number of functions only gives the
#' number of exported functions, and the number of lines of code no longer
#' includes documentation and examples.
#'
#' @note This function does not work with `devtools::load_all(".")`.
#'
#' @export
#'
package_info <- function() {
  message(paste("Number of exported functions =", length(ls("package:rangeRinPA")) - 1)) ## remove one for the pipe
  if (requireNamespace("R.utils", quietly = TRUE)) {
    files <- dir(paste0(find.package(package = "rangeRinPA"), "/R/"))
    filenames_R <- paste0(find.package(package = "rangeRinPA"), "/R/", files)
    lines_code <- sum(sapply(filenames_R, function(file) R.utils::countLines(file)))
    message(paste("Number of lines of code =", lines_code, "\nNote: the numbers varies if the package is loaded using library()\nand or via devtools (the latter counts everything, including documentation)!"))
  } else {
    message("Install the package R.utils for more info.")
  }
  return(invisible(NULL))
}
