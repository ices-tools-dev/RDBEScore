#' Run basic checks on selection methods and probabilities
#'
#' This function runs some basic checks on selection methods and
#' and probabilities of the different sampling tables of a hierarchy.
#' It should be run ahead of generateProbs to secure its correct execution
#' and for that reason it is included in the wrapper applyGenerateProbs.
#'
#' @param x - RDBES raw object
#' @param verbose - If TRUE prints the issue behind the stop
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return nothing
#'
#' @export
#'
#' @seealso \code{\link{applyGenerateProbs}}
#' \code{\link{generateProbs}}
#'
#' examples for now see
#' https://github.com/ices-eg/WK_RDBES/tree/master/WKRDB-EST2/chairs/Nuno
#'


runChecksOnSelectionAndProbs <- function(x,
                                         verbose = FALSE,
                                         strict = TRUE) {


  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(x, verbose = verbose, strict = strict)

  if (length(unique(x[["DE"]]$DEhierarchy)) > 1) stop(">1 hierarchy in data not
                                                    yet specified")


  targetTables <- getTablesInRDBESHierarchy(x[["DE"]]$DEhierarchy[1],
                                            includeTablesNotInSampHier = FALSE)
  targetTables <- targetTables[targetTables != "DE"]
  targetTables <- targetTables[targetTables != "FM"]
  parentId <- paste0(targetTables, "id")
  targetTables <- targetTables[targetTables != "SD"]
  parentId <- parentId[parentId != "BVid"]

  # aspects needing development
  if (any(!is.na(x[["SA"]]$SAparentID))) stop("multiple sub-sampling present
                                                in SA: not yet developed")

  for (i in targetTables) {
    print(paste("====", i, "====="))
    # Get the column names we'll need
    stratumNameCol <- paste0(i, "stratumName")
    stratificationCol <- paste0(i, "stratification")
    clusterNameCol <- paste0(i, "clusterName")
    clusteringCol <- paste0(i, "clustering")
    selectMethCol <- paste0(i, "selectMeth")
    selProbCol <- paste0(i, "selProb")
    incProbCol <- paste0(i, "incProb")
    numTotalCol <- paste0(i, "numTotal")
    numSampCol <- paste0(i, "numSamp")

    if (!is.null(x[[i]])) {
      # following code will be worth setting in data.table
      sampIdentifiers <-
        unique(x[[i]][[eval(noquote(parentId[targetTables == i]))]])
      print(paste0(
        length(sampIdentifiers), " unique ",
        noquote(parentId[targetTables == i])
      ))
      ls1 <- split(x[[i]], x[[i]][[eval(noquote(parentId[targetTables == i]))]])
      lapply(ls1, function(x, ...) {
        print(paste0(
          "analysing ", parentId[targetTables == i], ":",
          x[[parentId[targetTables == i]]][1]
        ))

        # stops
        # on not yet developed features
        # if (length(unique(x[[stratumNameCol]])) > 1 |
        #     any(x[[stratificationCol]] == "Y")) {
        #   if (verbose) {
        #     print(x)
        #   }
        #   stop("strata present: not yet specified")
        # }
        if (length(unique(x[[clusterNameCol]])) > 1 |
            any(x[[clusteringCol]] == "Y")) {
          if (verbose) {
            print(x)
          }
          stop("clusters present: not yet specified")
        }

        # on methods
        nonProbMethods <- c(
          "FIXED", "NPQSRSWOR", "NPCLQS-O", "NPCLQS-T",
          "NPCS", "NPJS", "NPQSRSWOR", "NPQSRSWR",
          "NPQSYSS", "Unknown"
        )
        if (any(x[[selectMethCol]] %in% nonProbMethods)) {
          if (verbose) {
            print(x)
          }
          stop("NonProbabilistic or Unknown selection methods present. Action
          needed: Overwrite with probabilistic alternative and re-run")
        }
        if (any(paste(x[[selectMethCol]], x[[selProbCol]]) %in%
          paste(c("UPSWR", "UPSWOR"), NA))) {
          if (verbose) {
            print(x)
          }
          stop("UP methods without selProb present. Action needed: specify
               probs and re-run")
        }
        if (any(paste(x[[selectMethCol]], x[[incProbCol]]) %in%
          paste(c("UPSWR", "UPSWOR"), NA))) {
          if (verbose) {
            print(x)
          }
          stop("UP methods without incProb present. Action needed: specify
               probs and re-run")
        }

        # on numbers and probs
        # if (length(unique(x[[numTotalCol]])) > 1) {
        #   if (verbose) {
        #     print(x)
        #   }
        #   stop("more than 1 numTotal per parentId not allowed when
        #        stratification == N")
        # }
        # if (length(unique(x[[numSampCol]])) > 1) {
        #   if (verbose) {
        #     print(x)
        #   }
        #   stop("more than 1 numSamp per parentId not allowed when
        #        stratification == N")
        # }

        if (length(unique(x[[selectMethCol]])) > 1) {
          print("warning: more than 1 selection method")
        }
        if (sum(x[[selectMethCol]] == "OutOfFrame") > 1) {
          print("warning: outOfFrame present")
        }
        if (any(x[[selectMethCol]] %in% c("WR")) == TRUE) {
          print("warning: WR selection methods present")
        }
        if (any(x[[selectMethCol]] %in% c("UPSWR", "UPSWOR")) == TRUE) {
          print("warning:UP selection methods present")
        }
        if (any(is.na(x[[numTotalCol]])) == TRUE) {
          print("warning: NAs in numTotal")
        }
        if (any(is.na(x[[numSampCol]])) == TRUE) {
          print("warning: NAs in numSamp")
        }
        # if (any(is.na(x[[numSampCol]])) == FALSE &
        #     unique(x[[numSampCol]]) != nrow(x)) {
        #   print("warning: numSamp!= nrows")
        # }
      })
    }
  }
}
