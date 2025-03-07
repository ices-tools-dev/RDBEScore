

# Script for getting functions and description from the package

path <- "./WGRDBES-EST/personal/Kirsten/"

# Functions from Richard ----

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

# Past version ----

v <- "fc7683c"

library(remotes)

install_github(paste0("ices-tools-dev/RDBEScore@", v), build_vignettes = F)

library(RDBEScore)
devtools::package_info("RDBEScore")
function_table <- listPackageFunctions("RDBEScore")

function_table$Description <- gsub("\"", "'", function_table$Description)

write.table(function_table, paste0(path, "functions_from_", v, ".csv"), row.names = F, sep = ";;")
