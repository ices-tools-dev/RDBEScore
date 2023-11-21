capture.output({
#Create list of dfs for comparison
list_with_nulls  <-  H1Example
list_of_dfs <- list_with_nulls[!(sapply(list_with_nulls, is.null))]
list_of_dfs <- lapply(list_of_dfs, as.data.frame)
colMapping <-   stats::setNames(mapColNamesFieldR$Field.Name, mapColNamesFieldR$R.Name)
list_of_dfs_long_names <- lapply(list_of_dfs, function(df, colMapping){
  colnames(df) <- colMapping[colnames(df)]
  df
}, colMapping)

test_that("error if key column is missing", {
  list_of_dfs$DE$DEid <- NULL

  expect_error(importRDBESDataDFS(list_of_dfs), "DEid does not exist in the input tables.")
})

test_that("error if key column has duplicate values", {
  list_of_dfs$DE$DEid <- 1

  expect_error(importRDBESDataDFS(list_of_dfs), "DEid does not have unique values.")
})


test_that("1 missing columns are added correctly", {
  list_of_dfs$DE$DEyear <- NULL

  # Define the expected column names for table1
  expected_cols <- colnames(H1Example$DE)


  # Call the function with addMissingColumns = TRUE
  dt <- importRDBESDataDFS(list_of_dfs, addMissingColumns = TRUE)

  # Check that the missing columns were added correctly

  expect_equal(colnames(dt$DE), expected_cols)
})

test_that("2 missing columns are added correctly", {
  list_of_dfs$DE$DEyear <- NULL
  list_of_dfs$DE$DEstratumName <- NULL

  # Define the expected column names for table1
  expected_cols <- colnames(H1Example$DE)
  dt <- importRDBESDataDFS(list_of_dfs, addMissingColumns = TRUE)
  expect_equal(colnames(dt$DE), expected_cols)
})

test_that("3 missing columns are added correctly", {
  list_of_dfs$DE$DEyear <- NULL
  list_of_dfs$DE$DEstratumName <- NULL
  list_of_dfs$VS$VSstratum <- NULL

  # Define the expected column names for table1
  expected_cols <- colnames(H1Example$DE)
  dt <- importRDBESDataDFS(list_of_dfs, addMissingColumns = TRUE)
  expect_equal(colnames(dt$DE), expected_cols)
  expect_equal(colnames(dt$VS), colnames(H1Example$VS))
})

test_that("column order not corrected if changed without adding missing columns", {
  list_of_dfs$DE <- list_of_dfs$DE[,c(2,1,3,4,5,6,7,8,9,15,11,12,13,14,10)]
  dt <- importRDBESDataDFS(list_of_dfs, addMissingColumns = FALSE)
  expect_equal(colnames(dt$DE), colnames(list_of_dfs$DE))
})

test_that("column order corrected if changed with adding missing columns", {
  list_of_dfs$DE <- list_of_dfs$DE[,c(2,1,3,4,5,6,7,8,9,15,11,12,13,14,10)]
  dt <- importRDBESDataDFS(list_of_dfs, addMissingColumns = TRUE)
  expect_equal(colnames(dt$DE), colnames(H1Example$DE))
})

})


