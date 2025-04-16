capture.output({
  test_that("listPackageFunctions returns a data frame with expected columns", {
    df <- listPackageFunctions("stats")
    expect_true(is.data.frame(df))
    expect_true(all(c("Function", "Description", "Exported") %in% names(df)))
  })

  test_that("Function column contains non-empty character values", {
    df <- listPackageFunctions("stats")
    expect_true(is.character(df$Function))
    expect_true(all(nzchar(df$Function)))
  })

  test_that("Exported column is logical", {
    df <- listPackageFunctions("stats")
    expect_true(is.logical(df$Exported))
  })



  test_that("Known function 'chisq.test' is present in package 'stats'", {
    df <- listPackageFunctions("stats")
    expect_true("chisq.test" %in% df$Function)
    #cehck for the description
    expect_true(df$Description[df$Function == "chisq.test"]  == "Pearson's Chi-squared Test for Count Data")
  })

})
