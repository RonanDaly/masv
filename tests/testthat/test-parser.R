#### Import functions ####
library(testthat)
setwd('../..')
source("./R/parser.R")

#test_that("multiplication works", {
#  expect_equal(2 * 2, 4)
#})

#eset = parseExpressionSet("./inst/masv1test.tsv")

#f = names(eset@featureData@data)[1]

test_that("multiplication works", {
  #setwd('../..')
  eset = parseExpressionSet("./inst/masv1test.tsv")
  expect_equal(names(eset@featureData@data)[1], "mz")
})
