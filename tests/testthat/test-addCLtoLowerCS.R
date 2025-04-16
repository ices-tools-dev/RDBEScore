
# Mock RDBESDataObject
#TODO propery mock this
mock_rdbes <- H8ExampleEE1

# Define strata lists
strataListCS <- list(LEarea="27.3.d.28.1", LEmetier6 = "OTM_SPF_16-31_0_0", TEstratumName = month.name[1:3], SAspeCodeFAO = "SPR")
strataListCL <- list(CLarea="27.3.d.28.1", CLquar = 1, CLmetier6 = "OTM_SPF_16-31_0_0", CLspecFAO = "SPR")

# Test 1: Expect error if rdbes is not of class RDBESDataObject
test_that("addCLtoLowerCS throws an error if rdbes is not of class RDBESDataObject", {
  expect_error(
    addCLtoLowerCS(list(), strataListCS, strataListCL, combineStrata = TRUE, lowerHierarchy = "C", CLfields = c("CLoffWeight")),
    "No CL data in the input RDBESDataObject"
  )
})

# Test 2: Expect error if CL data is missing in rdbes
test_that("addCLtoLowerCS throws an error if CL data is missing in rdbes", {
  mock_rdbes_no_CL <- mock_rdbes
  mock_rdbes_no_CL$CL <- NULL
  expect_error(
    addCLtoLowerCS(mock_rdbes_no_CL, strataListCS, strataListCL, combineStrata = TRUE, lowerHierarchy = "C", CLfields = c("CLoffWeight")),
    "No CL data in the input RDBESDataObject"
  )
})

# Test 3: Check correct output with valid input
test_that("addCLtoLowerCS adds the correct sumCLoffWeight column with valid input", {
  result <- expect_warning(addCLtoLowerCS(mock_rdbes, strataListCS, strataListCL, combineStrata = TRUE, lowerHierarchy = "C", CLfields = c("CLoffWeight")))

  # Check that the result has the correct columns
  expect_true("sumCLoffWeight" %in% names(result))
  expect_equal(result$sumCLoffWeight[1], 396210)
})

# Test 4: Check that the function handles unsupported lowerHierarchy correctly
test_that("addCLtoLowerCS throws an error if lowerHierarchy is unsupported", {
  expect_error(
    addCLtoLowerCS(mock_rdbes, strataListCS, strataListCL, combineStrata = TRUE, lowerHierarchy = "A", CLfields = c("CLoffWeight")),
    "Lower hierarchy A not yet supported"
  )
})
