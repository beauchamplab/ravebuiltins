# This function helps you get rid of dev check warnings, however, you should avoid using this function as cran check will fail you
get_from_package <- function (f, pkg, internal = FALSE, ifNotFound = NULL, check = TRUE) 
{
  if (!check || package_installed(pkg)) {
    export_f = ifelse(internal, ":::", "::")
    f = do.call(export_f, list(pkg = pkg, name = f))
    return(f)
  }
  return(ifNotFound)
}

package_installed <- function (pkg) 
{
    system.file("", package = pkg) != ""
}
