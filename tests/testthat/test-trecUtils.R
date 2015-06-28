context("TREC Utilities")

testfile_fulltopics <- system.file('extdata/tests/trec09.web.fulltopics',
                        package='evalIRamt', mustWork=T)

test_that("parse trec web fulltopics XML file", {

  parsedFile <- parseWebTopicDesc(testfile_fulltopics)
  desc_1 <-  "Find information on President Barack Obamas family history including genealogy national origins places and dates of birth etc."

  expect_equal(parsedFile[[1]]$type, "faceted")
  expect_equal(parsedFile[[1]]$text, "obama family tree")
  expect_equal(parsedFile[[1]]$desc, desc_1)
  for(i in seq(1, 50)) expect_equal(parsedFile[[i]]$number, i)
})
