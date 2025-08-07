 #### Import functions ####
library(testthat)
#setwd('../..')
#source("./R/parser.R")

#test_that("multiplication works", {
#  expect_equal(2 * 2, 4)
#})

#eset = parseExpressionSet("./inst/masv1test.tsv")

#f = names(eset@featureData@data)[1]

test_that("test parseExpressionSets", {
  #setwd('../..')
  #print(getwd())
  e_set_list = parseExpressionSets(test_path("testdata", "masv1test.tsv"))
  e_sets = e_set_list$e_sets
  eset = e_sets[[1]]
  expect_equal(names(eset@featureData@data)[1], "mz")
})
