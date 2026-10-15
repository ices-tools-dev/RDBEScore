#' Select the original example records from a fresh export and restore its IDs.
#'
#' @param downloaded Fresh RDBESDataObject.
#' @param original Original example RDBESDataObject used for the upload.
#' @return Downloaded values in the original row order, with original table IDs.
#' @details Matches all non-ID fields and previously matched parent/reference
#'   IDs. Only the documented upload code migrations, ISO date formatting and
#'   empty versus missing fields are normalized for matching. Unexpected value
#'   changes or missing observations stop preparation. Extra exported records
#'   are excluded because this function deliberately reproduces the original
#'   example. Database IDs are replaced together, including foreign keys.
#'   Corrected H1 species lists follow SS names and the server's SL/IS records,
#'   as accepted by rix133 on 2026-10-15.
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
prepareDownloadedExample <- function(downloaded, original) {
  result <- data.table::copy(downloaded)
  original <- data.table::copy(original)
  # Accepted correction: the SS list name identifies the intended species list.
  # Preserve its historical numeric ID, but use the matching server list's
  # metadata and species membership. This corrects the old mismatched lists
  # without changing the sampling observations or their identifiers.
  for (id in unique(original$SS$SLid)) {
    listName <- unique(original$SS$SSspecListName[original$SS$SLid == id])
    row <- which(original$SL$SLid == id)
    if (length(listName) != 1L || length(row) != 1L) stop('Ambiguous original species-list links.')
    if (original$SL$SLspeclistName[row] != listName) {
      candidates <- which(downloaded$SL$SLspeclistName == listName &
                           downloaded$SL$SLyear == original$SL$SLyear[row] &
                           downloaded$SL$SLcou == original$SL$SLcou[row] &
                           downloaded$SL$SLcatchFrac == original$SL$SLcatchFrac[row])
      if (length(candidates) != 1L) stop('Cannot resolve species list ', listName)
      for (field in setdiff(names(original$SL), 'SLid')) {
        data.table::set(original$SL, i = row, j = field,
                        value = downloaded$SL[[field]][candidates])
      }
      oldSpecies <- which(original$IS$SLid == id)
      newSpecies <- which(downloaded$IS$SLid == downloaded$SL$SLid[candidates])
      if (length(oldSpecies) != length(newSpecies)) {
        stop('Species-list correction changes membership count for ', listName)
      }
      for (field in setdiff(names(original$IS), c('ISid', 'SLid'))) {
        data.table::set(original$IS, i = oldSpecies, j = field,
                        value = downloaded$IS[[field]][newSpecies])
      }
    }
  }
  hierarchy <- unique(original$DE$DEhierarchy)
  stopifnot(length(hierarchy) == 1L)
  tables <- unique(c('VD', 'SL', 'IS',
                     RDBEScore::getTablesInRDBESHierarchy(hierarchy), names(original)))
  maps <- list()
  selected <- list()
  normalize <- function(value, field) {
    if (is.numeric(value)) {
      output <- sprintf('%.15g', value)
    } else output <- as.character(value)
    output[is.na(value)] <- ''
    if (grepl('Date$|date$', field)) {
      slash <- grepl('^[0-9]{2}/[0-9]{2}/[0-9]{4}$', output)
      output[slash] <- format(as.Date(output[slash], '%d/%m/%Y'), '%Y-%m-%d')
    }
    if (field %in% c('DEsampScheme', 'CLsampScheme', 'CEsampScheme')) {
      output[output == 'National Routine'] <- 'WGRDBES-EST TEST 1'
    }
    if (field %in% c('CLdBasSciWeig', 'CEdBasSciEff')) output[output == 'Estimate'] <- 'Estimated'
    if (field %in% c('CLdSouSciWeig', 'CEdSouSciEff')) output[output == 'Combcd'] <- 'CombOD'
    if (field %in% c('CLdSoucstatRect', 'CEsoucStatRect')) output[output == 'AdjPosData'] <- 'PosDat'
    if (field %in% c('CLfreshWatNam', 'CEfreshWatNam')) output[output == ''] <- 'NA'
    if (field == 'CLexpDiff') output[output == 'NoDiff'] <- 'NoDiffKnown'
    output
  }
  signature <- function(table, columns) {
    values <- lapply(columns, function(field) normalize(table[[field]], field))
    do.call(paste, c(values, sep = '\034'))
  }
  for (tbl in tables) {
    old <- original[[tbl]]
    current <- downloaded[[tbl]]
    if (is.null(old)) { result[tbl] <- list(NULL); next }
    id <- paste0(tbl, 'id')
    if (!nrow(old)) { result[[tbl]] <- data.table::copy(old); next }
    if (is.null(current) || !nrow(current)) stop('Missing downloaded records in ', tbl)
    if (!setequal(names(old), names(current))) {
      stop('Changed columns in ', tbl, ': added ',
           paste(setdiff(names(current), names(old)), collapse = ', '), '; removed ',
           paste(setdiff(names(old), names(current)), collapse = ', '))
    }
    if (anyNA(current[[id]]) || anyDuplicated(current[[id]]) ||
        anyNA(old[[id]]) || anyDuplicated(old[[id]])) stop('Invalid primary IDs in ', tbl)
    candidate <- data.table::copy(current)
    foreign <- intersect(grep('^[A-Z]{2}id$', names(old), value = TRUE), names(maps))
    for (field in foreign) {
      map <- maps[[field]]
      data.table::set(candidate, j = field,
                      value = map$old[match(current[[field]], map$new)])
    }
    fields <- c(setdiff(names(old), grep('^[A-Z]{2}id$', names(old), value = TRUE)), foreign)
    oldKey <- signature(old, fields)
    newKey <- signature(candidate, fields)
    # Identical observations may occur more than once. Match occurrences in ID
    # order, preserving multiplicity rather than duplicating one downloaded row.
    numbered <- function(key) paste(key, ave(seq_along(key), key, FUN = seq_along), sep = '\035')
    index <- match(numbered(oldKey), numbered(newKey))
    if (anyNA(index)) {
      stop('Cannot match ', sum(is.na(index)), ' original ', tbl,
           ' records to the download. Check changed values before preparing data.')
    }
    maps[[id]] <- list(old = old[[id]], new = current[[id]][index])
    selected[[tbl]] <- index
    result[[tbl]] <- data.table::copy(current[index])
  }
  for (tbl in names(selected)) {
    old <- original[[tbl]]
    table <- result[[tbl]]
    for (field in grep('^[A-Z]{2}id$', names(old), value = TRUE)) {
      value <- table[[field]]
      present <- !is.na(value)
      if (any(present)) {
        map <- maps[[field]]
        restored <- map$old[match(value[present], map$new)]
        if (is.null(map) || anyNA(restored) ||
            any(is.na(old[[field]][present])) ||
            any(restored != old[[field]][present])) {
          stop('Changed or unresolved parent/reference links in ', tbl, '$', field)
        }
      }
      # Some exports omit ancestor IDs; recover them from the verified original
      # hierarchy after all populated downloaded relationships have been checked.
      data.table::set(table, j = field, value = old[[field]])
    }
    data.table::setkeyv(table, paste0(tbl, 'id'))
  }
  RDBEScore::validateRDBESDataObject(result, checkDataTypes = TRUE, strict = TRUE)
  result
}

#' Write prepared examples as CSV and ZIP test fixtures.
#' @param x Prepared RDBESDataObject.
#' @param hierarchy CS hierarchy number.
#' @param outputDir Dated test-fixture directory.
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
writeExampleFixtures <- function(x, hierarchy, outputDir) {
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  csvFiles <- character()
  for (tbl in names(x)) {
    if (is.null(x[[tbl]])) next
    values <- as.data.frame(x[[tbl]])
    mapping <- RDBEScore::mapColNamesFieldR
    mapping <- mapping[mapping$Table.Prefix == tbl, ]
    columns <- mapping$Field.Name[match(names(values), mapping$R.Name)]
    if (anyNA(columns)) stop('Missing CSV column mapping in ', tbl)
    names(values) <- columns
    file <- file.path(outputDir, paste0(RDBEScore::DefaultFileNames[[tbl]], '.csv'))
    if (any(vapply(values, function(value) {
      any(grepl('[,\r\n]', as.character(value)), na.rm = TRUE)
    }, logical(1)))) stop('Cannot write unquoted download CSV values containing commas/newlines in ', tbl)
    # The package's download reader deliberately disables CSV quote handling.
    utils::write.csv(values, file, row.names = FALSE, na = '', quote = FALSE)
    csvFiles <- c(csvFiles, normalizePath(file))
  }
  if (hierarchy == 1) {
    zipFile <- file.path(outputDir, 'H1_Example.zip')
    zip::zipr(zipFile, files = csvFiles, mode = 'cherry-pick')
    prepareH1ExampleZips(zipFile, outputDir)
    file.copy(file.path(outputDir, 'Design.csv'), file.path(outputDir, 'DE.csv'), overwrite = TRUE)
    file.copy(file.path(outputDir, 'SamplingDetails.csv'), file.path(outputDir, 'SD.csv'), overwrite = TRUE)
    # The original flat CSV fixture also contains local CL/CE and a deliberate
    # malformed-vessel test. These are separate from the packaged H1 CS example.
    oldDir <- file.path('tests', 'testthat', 'h1_v_20250211')
    for (file in c('CommercialLanding.csv', 'CommercialEffort.csv')) {
      file.copy(file.path(oldDir, file), file.path(outputDir, file), overwrite = TRUE)
    }
    file.copy(file.path(oldDir, 'BadVesselIdentifiers'), outputDir, recursive = TRUE)
  }
  invisible(outputDir)
}
