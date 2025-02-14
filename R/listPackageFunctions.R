#' Extract Functions and Descriptions from an R Package
#'
#' This function extracts the list of functions contained within a specified R package and retrieves a brief description for each function from the package documentation. The description is obtained by parsing the Rd file associated with each function to extract the text from the \code{\title} field. In addition, the function determines whether each function is exported from the package by comparing against the package’s exported names.
#'
#' @param pkg A character string or an unquoted name specifying the package from which to extract functions. The package must be installed and accessible.
#'
#' @return A data frame with three columns. The \code{Function} column lists the names of the functions found in the package. The \code{Description} column contains the brief descriptions extracted from each function’s documentation, and the \code{Exported} column is a logical vector indicating whether the function is exported from the package.
#'
#' @details The function first accesses the package namespace using \code{asNamespace} and retrieves all objects using \code{ls}. It filters these objects to include only functions. For each function, the associated help file is retrieved using \code{utils::help} and the Rd file is extracted with \code{utils:::.getHelpFile}. The internal helper function \code{getRdTitle} is then used to parse the Rd object and extract the text in the \code{\title} field. Finally, the function assembles the output into a data frame that also includes an indicator of whether each function is exported.
#'
#' @examples
#' \dontrun{
#' # Extract functions from the stats package along with their descriptions and export status.
#' tab <- listPackageFunctions("stats")
#' print(tab)
#' }
#'
#' @export
listPackageFunctions <- function(pkg) {
  ns <- asNamespace(pkg)
  fun_names <- ls(ns, all.names = TRUE)
  fun_names <- fun_names[sapply(fun_names, function(x) is.function(get(x, envir = ns)))]
  exported_names <- getNamespaceExports(pkg)

  res <- data.frame(Function = character(), Description = character(), stringsAsFactors = FALSE)

  for (f in fun_names) {
    h <- utils::help(f, package = as.character(pkg))
    desc <- ""
    if (length(h) > 0) {
      rd <- tryCatch(utils:::.getHelpFile(h), error = function(e) NULL)
      if (!is.null(rd)) {
        desc <- getRdTitle(rd)
      }
    }
    exported <- f %in% exported_names
    res <- rbind(res, data.frame(Function = f, Description = desc, Exported = exported, stringsAsFactors = FALSE))
  }

  return(res)
}

#' Extract Title from an Rd Object
#'
#' This internal helper function traverses an Rd object to extract the text from the \code{\title} field. It is used by \code{listPackageFunctions} to obtain a concise description for each documented function.
#'
#' @param rd An Rd object typically obtained via \code{utils:::.getHelpFile}.
#'
#' @return A character string containing the title extracted from the Rd object. If the title is not found, an empty string is returned.
#'
#' @keywords internal
getRdTitle <- function(rd) {
  title <- ""
  for (el in rd) {
    if (is.list(el) && !is.null(attr(el, "Rd_tag")) && attr(el, "Rd_tag") == "\\title") {
      title <- paste(unlist(el), collapse = " ")
      break
    }
  }
  title
}
