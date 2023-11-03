capture.output({  ## suppresses printing of console output when running test()
test_that("sorting a raw object works for H8", {
  correctOrder <- c('DE', 'SD', 'TE', 'VS', 'FT', 'LE', 'SS', 'SA', 'FM', 'BV',
                    'FO', 'LO', 'OS', 'VD', 'SL', 'CL', 'CE')
  expect_equal(names(sort(H8ExampleEE1)),correctOrder)
})

test_that("sorting a raw object works for H5", {
  correctOrder <- c('DE', 'SD', 'OS', 'LE', 'FT', 'SS', 'SA', 'FM', 'BV',
                    'VS', 'FO', 'TE', 'LO', 'VD', 'SL', 'CL', 'CE')
  expect_equal(names(sort(H5Example)),correctOrder)
})

test_that("sorting a raw object works for H1", {
  correctOrder <- c('DE', 'SD', 'VS', 'FT', 'FO', 'SS', 'SA', 'FM', 'BV',
                    'TE', 'LO', 'OS', 'LE', 'VD', 'SL', 'CL', 'CE')
  expect_equal(names(sort(H1Example)),correctOrder)
})

test_that("sorting a raw object does nothing if no hierarchy", {
  modH1Example <- H1Example
  modH1Example$DE$DEhierarchy <- NA
  expect_equal(names(sort(modH1Example)),names(H1Example))
})

test_that("sorting a raw object does nothing if no DE table", {
  obj <- createRDBESDataObject()
  expect_equal(names(sort(obj)),names(obj))
})

test_that("sorting a raw object does nothing if multiple hierarchies", {
  modH1Example <- H1Example
  modH1Example$DE$DEhierarchy[1] <- 2
  res <- suppressWarnings(sort(modH1Example))
  expect_equal(names(res),names(H1Example))
})

test_that("sorting a raw object does gives a warning if multiple hierarchies", {
  modH1Example <- H1Example
  modH1Example$DE$DEhierarchy[1] <- 2
  expect_warning(sort(modH1Example),
                 "No sort order for multiple hierarchies can be defined!")
})

test_that("sorting a raw object works for hierarchies 1-13 only", {
  modH1Example <- H1Example
  modH1Example$DE$DEhierarchy <- 20
  expect_error(sort(modH1Example),
                 "hierarchy parameter must be between 1 and 13")
})
})
