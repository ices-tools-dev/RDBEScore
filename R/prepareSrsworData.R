#' Prepare a complete SRSWOR sampling tree
#'
#' @param estObject An RDBESEstObject ending at the response's sampling table.
#' @param targetValue Name of a numeric response in the terminal table. Missing
#'   responses are retained for explicit omission by the estimator.
#' @param censusLinks Names of explicitly documented artificial census links.
#'
#' @return A list of ordered sampling-stage tables and terminal responses.
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: Pending
#' - Notes/scope: Supports one DE/SD branch, SRSWOR and census stages without
#'   explicit clustering or nested SA subsampling. Terminal sampled flags do not
#'   describe response availability, which is checked directly. Missing responses
#'   retain their sampled-unit nodes and declared design counts.
#' @keywords internal
prepareSrsworData <- function(estObject, targetValue,
                             censusLinks = character()) {
  validateRDBESEstObject(estObject)
  x <- as.data.frame(estObject)
  if (!nrow(x)) {
    stop("estObject must contain sampled units.", call. = FALSE)
  }
  if (!is.character(targetValue) || length(targetValue) != 1L ||
      is.na(targetValue) || !targetValue %in% names(x)) {
    stop("targetValue must name one column in estObject.", call. = FALSE)
  }
  if (!is.numeric(x[[targetValue]]) ||
      any(!is.na(x[[targetValue]]) & !is.finite(x[[targetValue]]))) {
    stop("Observed targetValue responses must be finite numeric values.",
         call. = FALSE)
  }
  if (all(is.na(x[[targetValue]]))) {
    stop("targetValue has no observed responses to estimate.", call. = FALSE)
  }
  if (!is.character(censusLinks) || anyNA(censusLinks) ||
      anyDuplicated(censusLinks)) {
    stop("censusLinks must be a character vector of unique table names.",
         call. = FALSE)
  }
  for (id in intersect(c("DEid", "SDid"), names(x))) {
    if (anyNA(x[[id]]) || length(unique(x[[id]])) != 1L) {
      stop("Only one complete DE/SD branch is supported. Check ", id, ".",
           call. = FALSE)
    }
  }

  stageColumns <- grep("^su[0-9]+table$", names(x), value = TRUE)
  stageNumbers <- as.integer(sub("^su([0-9]+)table$", "\\1", stageColumns))
  stageColumns <- stageColumns[order(stageNumbers)]
  stageNumbers <- sort(stageNumbers)
  if (!length(stageColumns) ||
      !identical(stageNumbers, seq_along(stageNumbers))) {
    stop("Sampling stages must be ordered consecutively as su1table, ",
         "su2table, and so on.", call. = FALSE)
  }
  tables <- vapply(stageColumns, function(column) {
    values <- unique(as.character(x[[column]]))
    if (length(values) != 1L || is.na(values) || !nzchar(values)) {
      stop("Each sampling stage must identify one complete table: ", column,
           ".", call. = FALSE)
    }
    values
  }, character(1))
  if (anyDuplicated(tables) || any(!grepl("^[A-Z]{2}$", tables))) {
    stop("Repeated tables and nested SA subsampling stages are not supported.",
         call. = FALSE)
  }
  if (!startsWith(targetValue, tail(tables, 1L))) {
    stop("targetValue must belong to the terminal sampling table (", 
         tail(tables, 1L), "). Create an estimation object stopping there.",
         call. = FALSE)
  }
  if (any(!censusLinks %in% tables)) {
    stop("Every censusLinks table must be present in estObject: ",
         paste(setdiff(censusLinks, tables), collapse = ", "), ".",
         call. = FALSE)
  }
  if ("SAsubSampleLevel" %in% names(x) &&
      any(!is.na(x$SAsubSampleLevel) & x$SAsubSampleLevel != 1L)) {
    stop("Nested SA subsampling is not supported.", call. = FALSE)
  }

  stages <- vector("list", length(tables))
  parentKeys <- rep(".root", nrow(x))
  for (stage in seq_along(tables)) {
    table <- tables[stage]
    prefix <- paste0("su", stage)
    idColumn <- paste0(table, "id")
    required <- c(idColumn, paste0(prefix, c(
      "stratification", "stratumName", "numSamp", "numTotal", "selectMeth"
    )))
    missing <- setdiff(required, names(x))
    if (length(missing)) {
      stop("Missing sampling metadata for ", table, ": ",
           paste(missing, collapse = ", "), ".", call. = FALSE)
    }
    fields <- c("stratification", "stratumName", "numSamp", "numTotal",
                "selectMeth", "clustering", "samp", "incProb", "selProb")
    available <- paste0(prefix, fields)
    available <- available[available %in% names(x)]
    node <- data.frame(parent = parentKeys,
                       unitId = as.character(x[[idColumn]]),
                       x[available], stringsAsFactors = FALSE,
                       check.names = FALSE)
    names(node) <- sub(paste0("^", prefix), "", names(node))
    if (anyNA(node$unitId) || any(!nzchar(node$unitId))) {
      stop("Missing sampled-unit IDs at ", table, ".", call. = FALSE)
    }
    node <- unique(node)
    if (anyDuplicated(node$unitId)) {
      stop("Conflicting parents or repeated sampling metadata for ", table,
           " unit IDs.", call. = FALSE)
    }
    rowIndex <- match(as.character(x[[idColumn]]), node$unitId)
    node$key <- paste0("s", stage, "_", seq_len(nrow(node)))

    node$method <- as.character(node$selectMeth)
    if (anyNA(node$method) ||
        any(!node$method %in% c("SRSWOR", "CENSUS"))) {
      unsupported <- unique(node$method[is.na(node$method) |
                          !node$method %in% c("SRSWOR", "CENSUS")])
      stop("Unsupported selection method at ", table, ": ",
           paste(unsupported, collapse = ", "),
           ". Only SRSWOR and CENSUS are supported.", call. = FALSE)
    }
    if ("clustering" %in% names(node) &&
        any(is.na(node$clustering) | as.character(node$clustering) != "N")) {
      stop("Explicit clustering is not supported at ", table, ".",
           call. = FALSE)
    }
    if ("samp" %in% names(node) &&
        (anyNA(node$samp) || any(!as.character(node$samp) %in% c("Y", "N")) ||
         (stage < length(tables) && any(as.character(node$samp) != "Y")))) {
      stop("Incomplete or unsampled intermediate units at ", table, ".",
           call. = FALSE)
    }
    stratification <- as.character(node$stratification)
    if (anyNA(stratification) || any(!stratification %in% c("Y", "N"))) {
      stop("stratification must be Y or N at ", table, ".", call. = FALSE)
    }
    parentStratification <- split(stratification, node$parent)
    if (any(vapply(parentStratification, function(values) {
      length(unique(values)) != 1L
    }, logical(1)))) {
      stop("Stratification flags must agree within each parent at ", table,
           ".", call. = FALSE)
    }
    node$stratum <- as.character(node$stratumName)
    if (any(stratification == "Y" &
            (is.na(node$stratum) | !nzchar(node$stratum)))) {
      stop("Stratified units require stratum names at ", table, ".",
           call. = FALSE)
    }
    node$stratum[stratification == "N"] <- ".unstratified"
    node$n <- node$numSamp
    node$N <- node$numTotal
    if (!is.numeric(node$n) || !is.numeric(node$N) ||
        any(!is.finite(node$n)) || any(!is.finite(node$N)) ||
        any(node$n < 1 | node$N < node$n | node$n != floor(node$n) |
            node$N != floor(node$N))) {
      stop("Sample and population sizes must be finite whole numbers ",
           "satisfying 1 <= n <= N at ", table, ".", call. = FALSE)
    }
    if (any(node$method == "CENSUS" & node$n != node$N)) {
      stop("CENSUS requires n = N at ", table, ".", call. = FALSE)
    }
    node$originaln <- node$n
    node$originalN <- node$N
    if (table %in% censusLinks) {
      if (any(node$method != "CENSUS") || anyDuplicated(node$parent)) {
        stop("An artificial census link must be CENSUS with exactly one ",
             "child per parent at ", table, ".", call. = FALSE)
      }
      if (any(node$n != 1)) {
        warning("Treating explicitly declared artificial census link ", table,
                " as one child per parent (effective n = N = 1). Original ",
                "counts are retained in the design metadata.", call. = FALSE)
      }
      node$n <- node$N <- rep(1, nrow(node))
    }

    groups <- split(seq_len(nrow(node)),
                    interaction(node$parent, node$stratum, drop = TRUE))
    for (rows in groups) {
      group <- node[rows, , drop = FALSE]
      if (length(unique(group$n)) != 1L ||
          length(unique(group$N)) != 1L ||
          length(unique(group$method)) != 1L ||
          length(unique(group$stratification)) != 1L) {
        stop("Inconsistent sampling metadata within a parent and stratum at ",
             table, ".", call. = FALSE)
      }
      if (nrow(group) != group$n[1L]) {
        stop("Observed sampled-unit count (", nrow(group),
             ") differs from declared n (", group$n[1L], ") at ", table,
             ". Check missing records and the sampling-unit definition. ",
             "Use censusLinks only for a documented artificial census link.",
             call. = FALSE)
      }
      if ("incProb" %in% names(group)) {
        probability <- group$incProb
        if (!is.numeric(probability) ||
            any(!is.na(probability) &
                (!is.finite(probability) |
                 abs(probability - group$n / group$N) > 1e-8))) {
          stop("Inclusion probabilities disagree with n/N at ", table, ".",
               call. = FALSE)
        }
      }
      if ("selProb" %in% names(group)) {
        probability <- group$selProb
        supplied <- probability[!is.na(probability)]
        if (!is.numeric(probability) ||
            any(!is.finite(supplied) | supplied <= 0 | supplied > 1) ||
            length(unique(supplied)) > 1L ||
            (group$method[1L] == "CENSUS" && any(supplied != 1))) {
          stop("Selection probabilities must be valid and equal within ",
               "SRSWOR samples, and one for CENSUS, at ", table, ".",
               call. = FALSE)
        }
      }
    }
    stages[[stage]] <- node[c("key", "parent", "stratum", "n", "N", "method",
                              "unitId", "originaln", "originalN")]
    stages[[stage]]$table <- table
    parentKeys <- node$key[rowIndex]
  }
  if (anyDuplicated(parentKeys)) {
    stop("Terminal sampling units must have exactly one response row. ",
         "Stop the estimation object at the response table.", call. = FALSE)
  }
  names(stages) <- tables
  list(stages = stages,
       response = x[[targetValue]][match(tail(stages, 1L)[[1L]]$key, parentKeys)],
       tables = unname(tables), targetValue = targetValue)
}
