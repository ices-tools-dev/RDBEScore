## Snapshot of the ICES code lists used by checkRDBESCodes()
##
## Downloads every ICES vocabulary code type that checkRDBESCodes() checks
## (ratioFieldCodeTypes() in R/ratioHelpers.R) and writes them to
## inst/extdata/rdbes-code-snapshot.csv. getRDBESCodeList() reads this file,
## so estimation never needs vocab.ices.dk and icesVocab stays in Suggests.
##
## Run from the RDBEScore repository root when vocab.ices.dk is reachable,
## check the diff of the CSV and commit it. A code type that cannot be
## downloaded keeps its rows from the previous snapshot.
##
## Needs icesVocab and devtools.

library(data.table)
devtools::load_all(".")

snapshotFile <- "inst/extdata/rdbes-code-snapshot.csv"
codeTypes <- sort(unique(ratioFieldCodeTypes()$codeType))

previous <- if (file.exists(snapshotFile)) {
  fread(snapshotFile, colClasses = "character", na.strings = "")
} else {
  data.table(codeType = character(), Key = character(),
             Description = character(), Deprecated = character(),
             Retrieved = character())
}

snapshot <- rbindlist(lapply(codeTypes, function(ct) {
  tryCatch({
    raw <- as.data.table(icesVocab::getCodeList(ct))
    if (!nrow(raw)) stop("empty code list")
    deprecated <- if ("Deprecated" %in% names(raw)) {
      as.logical(raw$Deprecated)
    } else {
      rep(FALSE, nrow(raw))
    }
    deprecated[is.na(deprecated)] <- FALSE
    message("Downloaded ", ct, ": ", nrow(raw), " codes")
    data.table(codeType = ct, Key = as.character(raw$Key),
               Description = as.character(raw$Description),
               Deprecated = as.character(deprecated),
               Retrieved = as.character(Sys.Date()))
  }, error = function(e) {
    warning("Keeping the previous snapshot of ", ct, ": ",
            conditionMessage(e), call. = FALSE)
    previous[codeType == ct,
             .(codeType, Key, Description, Deprecated, Retrieved)]
  })
}), use.names = TRUE)

setorder(snapshot, codeType, Key)
fwrite(snapshot, snapshotFile, na = "")
