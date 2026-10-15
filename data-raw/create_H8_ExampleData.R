# Import H8 example data downloaded from production RDBES on 2026-10-14 (#279).
# CS, VD and SL passed upload screening. CL/CE are local, unvalidated tables.
# AI-assisted: Yes
# Review: rix133
# Notes/scope: Imports saved ZIPs and renumbers IDs while preserving links.

ddir <- file.path('data-raw', 'exampleData')
zipFiles <- file.path(ddir, c('H8ExampleEE1_CS_2026-10-14.zip',
                             'H8ExampleEE1_VD_2026-10-14.zip',
                             'H8ExampleEE1_SL_2026-10-14.zip'))
H8ExampleEE1 <- RDBEScore::createRDBESDataObject(
  zipFiles, castToCorrectDataTypes = TRUE, strict = TRUE
)

# The VD country/year export includes two vessels outside this CS extract.
# Keep the vessels referenced by VS and LE using their downloaded IDs.
vesselIds <- unique(c(H8ExampleEE1$VS$VDid, H8ExampleEE1$LE$VDid))
vesselIds <- vesselIds[!is.na(vesselIds)]
if (any(!vesselIds %in% H8ExampleEE1$VD$VDid)) {
  stop('A vessel referenced by CS is missing from the VD export.')
}
H8ExampleEE1$VD <- H8ExampleEE1$VD[H8ExampleEE1$VD$VDid %in% vesselIds]

# Use the test-country vessel flag consistently with local CL/CE (rix133).
data.table::set(H8ExampleEE1$VD, j = 'VDflgCtry',
                value = rep('ZW', nrow(H8ExampleEE1$VD)))

# Recover ancestor IDs omitted from the CS export using downloaded parent links.
teIndex <- match(H8ExampleEE1$VS$TEid, H8ExampleEE1$TE$TEid)
vsIndex <- match(H8ExampleEE1$LE$VSid, H8ExampleEE1$VS$VSid)
if (anyNA(teIndex) || anyNA(vsIndex) ||
    anyDuplicated(H8ExampleEE1$TE$TEid) || anyDuplicated(H8ExampleEE1$VS$VSid)) {
  stop('CS contains missing or ambiguous TE/VS parent links.')
}
H8ExampleEE1$VS$SDid <- H8ExampleEE1$TE$SDid[teIndex]
H8ExampleEE1$LE$TEid <- H8ExampleEE1$VS$TEid[vsIndex]

# Restore the three missing DE text fields exported as empty strings.
for (col in c('DEnoSampReason', 'DEauxVarName', 'DEauxVarUnit')) {
  value <- H8ExampleEE1$DE[[col]]
  value[!is.na(value) & value == ''] <- NA_character_
  data.table::set(H8ExampleEE1$DE, j = col, value = value)
}

# Local CL/CE retained from preparation of H8_2025_02_11.rds. The archive's
# README records provenance. No vessel memberships have been invented.
localArchive <- file.path(ddir, 'H8ExampleEE1_local_CL_CE_2026-10-14.zip')
localConnection <- unz(localArchive, 'CL_CE.rds', open = 'rb')
localTables <- tryCatch(readRDS(localConnection), finally = close(localConnection))
H8ExampleEE1$CL <- data.table::copy(localTables$CL)
H8ExampleEE1$CE <- data.table::copy(localTables$CE)

# Check that the vessel references are consistent across VS, LE and VD.
vdIndex <- match(H8ExampleEE1$VS$VDid, H8ExampleEE1$VD$VDid)
if (anyNA(vdIndex) || anyDuplicated(H8ExampleEE1$VD$VDid) ||
    anyNA(H8ExampleEE1$VS$VDid) || anyNA(H8ExampleEE1$LE$VDid) ||
    anyNA(H8ExampleEE1$VS$VSencrVessCode) || anyNA(H8ExampleEE1$LE$LEencrVessCode) ||
    anyNA(H8ExampleEE1$VD$VDencrVessCode) ||
    any(H8ExampleEE1$LE$VDid != H8ExampleEE1$VS$VDid[vsIndex]) ||
    any(H8ExampleEE1$LE$LEencrVessCode != H8ExampleEE1$VS$VSencrVessCode[vsIndex]) ||
    any(H8ExampleEE1$VS$VSencrVessCode != H8ExampleEE1$VD$VDencrVessCode[vdIndex])) {
  stop('Vessel IDs and encrypted codes disagree between VS, LE and VD.')
}
# Use the H8 hierarchy order, including reference tables, when building ID maps.
# Sequence numbers retain the original example order despite server reordering.
# Encrypted vessel codes and BVfishId are identifiers of observations, not table
# primary keys, and remain unchanged.
tableOrder <- RDBEScore::getTablesInRDBESHierarchy(8)
tableOrder <- unique(c(tableOrder, names(H8ExampleEE1)))
idMaps <- list()
for (tbl in tableOrder) {
  table <- H8ExampleEE1[[tbl]]
  if (is.null(table)) next
  idField <- paste0(tbl, 'id')
  if (!idField %in% names(table)) next
  oldIds <- table[[idField]]
  if (anyNA(oldIds) || anyDuplicated(oldIds)) {
    stop('Cannot renumber missing or duplicated primary IDs in ', tbl, '.')
  }
  seqField <- paste0(tbl, 'seqNum')
  sortFields <- c(intersect(seqField, names(table)), idField)
  data.table::setorderv(table, sortFields)
  idMaps[[idField]] <- list(old = table[[idField]], new = seq_len(nrow(table)))
}
# Apply each map to its primary key and every reference to that key. Construct
# all maps first so that old and new numbers can never be mixed during mapping.
for (tbl in tableOrder) {
  table <- H8ExampleEE1[[tbl]]
  if (is.null(table)) next
  idFields <- grep('^[A-Z]{2}id$', names(table), value = TRUE)
  for (idField in idFields) {
    value <- table[[idField]]
    present <- !is.na(value)
    if (!any(present)) next
    map <- idMaps[[idField]]
    index <- match(value[present], map$old)
    if (is.null(map) || anyNA(index)) {
      stop('Unresolved foreign key in ', tbl, '$', idField, '.')
    }
    value[present] <- map$new[index]
    data.table::set(table, j = idField, value = value)
  }
  data.table::setkeyv(table, paste0(tbl, 'id'))
}
RDBEScore::validateRDBESDataObject(H8ExampleEE1, checkDataTypes = TRUE, strict = TRUE)
usethis::use_data(H8ExampleEE1, overwrite = TRUE)
