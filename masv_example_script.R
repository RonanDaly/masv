#BiocManager::install("MultiDataSet")
devtools::load_all()

library("MultiDataSet")


# Automatically generates basic documentation
#devtools::document()

masv_multi_data_set = parseMultiDataSet('./inst/masv2test.tsv')

# The MultiDataSet object may now be used for analysis

multi_data_set_to_masv(masv_multi_data_set, './inst/output_test2.tsv')