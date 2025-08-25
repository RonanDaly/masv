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
  e_set_list = parseExpressionSets(test_path("testdata", "masv2test.tsv"))
  e_sets = e_set_list$e_sets
  eset = e_sets[[1]]
  expect_equal(names(eset@featureData@data)[1], "mz")
})

test_that("test parseMultiDataSet", {
  multi = parseMultiDataSet(test_path("testdata", "masv2test.tsv"))
  set = names(multi)[[1]]
  print(set)
  # Get data
  data = assayData(multi[,set])
  data = data[[1]]
  data = data$exprs
  #data_sets[[set]] = data
  print('a')
  features = list(rownames(data))[[1]]
  feature1 = features[1]

  expect_equal(feature1, "Feature1")
})
