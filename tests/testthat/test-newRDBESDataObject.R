capture.output({  ## suppresses printing of console output when running test()

  test_that("newRDBESDataObject can create an empty object without errors
          or warnings",  {

            myObject <- expect_warning(newRDBESDataObject(),NA)
            myObject <-expect_error(newRDBESDataObject(),NA)
          })

  test_that("newRDBESDataObject returns correct fields",  {
            myObject <- newRDBESDataObject()
            fields <- c("DE","SD","VS","FT","FO","TE","LO","OS","LE","SS","SA","FM","BV","VD","SL","IS","CL","CE")
            expect_equal(sort(names(myObject)), sort(fields))
          })
}) ## end capture.output
