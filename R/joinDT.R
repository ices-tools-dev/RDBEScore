#' Join two tables using data.table
#'
#' Internal replacement for `dplyr::left_join()`, `dplyr::inner_join()` and
#' `dplyr::right_join()`, built on `data.table::merge()` with `sort = FALSE`.
#' The output follows dplyr's conventions:
#' * rows are returned in the order of `x`, each `x` row followed by all its
#'   matching `y` rows in the order they appear in `y`. A right join appends
#'   the unmatched `y` rows at the end;
#' * columns are the columns of `x` followed by the non-key columns of `y`;
#' * non-key columns found in both tables get `suffix` added to their names;
#' * `NA` key values match each other and many-to-many matches return all
#'   combinations;
#' * an integer key joined to a double key gives a double key.
#'
#' Differences from dplyr: if a suffixed name is already taken, `joinDT()`
#' gives an error (dplyr adds the suffix again). Other combinations of key
#' types follow data.table's join rules (e.g. a factor or logical key keeps the
#' type and levels of `x`, and joining factor or logical keys to other types
#' can give an error).
#'
#' The input tables are not modified.
#'
#' @param x,y data.frames or data.tables to join.
#' @param by Character vector of key column names. Use a named vector, e.g.
#'   `c("SSyear" = "SLyear")`, when the key columns have different names in
#'   `x` (names) and `y` (values). Only the `x` key columns are kept.
#' @param type The type of join: `"left"` keeps all rows of `x`, `"inner"`
#'   keeps only matching rows and `"right"` keeps all rows of `y`.
#' @param suffix Character vector of length 2 added to the names of non-key
#'   columns found in both `x` and `y`. Use `""` to keep the names of one
#'   table unchanged.
#'
#' @return A data.table if `x` is a data.table, otherwise a data.frame.
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Human review: rix133
#' - Notes/scope: Written to remove the dplyr dependency. Tested on
#'  every join made by the package test suite (~12k joins currently).
#'
#' @keywords internal
joinDT <- function(x, y, by, type = c("left", "inner", "right"),
                   suffix = c(".x", ".y")) {
  type <- match.arg(type)
  yBy <- unname(by)
  xBy <- if (is.null(names(by))) yBy else ifelse(names(by) == "", yBy, names(by))

  # convert first: merge() on a data.frame would dispatch to base R's merge().
  # A merge() warning (e.g. duplicated column names) is turned into an error
  out <- withCallingHandlers(
    merge(data.table::as.data.table(x), data.table::as.data.table(y),
          by.x = xBy, by.y = yBy,
          all.x = type == "left", all.y = type == "right",
          sort = FALSE, suffixes = suffix, allow.cartesian = TRUE),
    warning = function(w) stop(conditionMessage(w), call. = FALSE))

  # merge() returns the keys, the other x columns and the other y columns;
  # restore the column order of x
  xNames <- names(x)
  isKey <- xNames %in% xBy
  xNames[!isKey] <- names(out)[length(xBy) + seq_len(sum(!isKey))]
  data.table::setcolorder(out, xNames)

  # like dplyr, use a double key if the key is double in either table
  # (merge() can turn a double x key into integer when matching integer y)
  # this is needed because of Large ids. as.integer.or.dbl() stores an integer
  # id column as double if any value is above 2e9 (R's 32-bit integer limit)
  # It decides per column, so the same id can be integer in one table and double in another.
  for (k in seq_along(xBy)) {
    if (is.integer(out[[xBy[k]]]) &&
        (is.double(x[[xBy[k]]]) || is.double(y[[yBy[k]]]))) {
      data.table::set(out, j = xBy[k], value = as.double(out[[xBy[k]]]))
    }
  }
  if (!data.table::is.data.table(x)) data.table::setDF(out)
  out
}
