# Output eSet to masv file

# Get MASV type of given R type
getMASVType = function(rType) {
  if ( rType == 'double' || rType == 'numeric' ) {
    'float'
  } else if ( rType == 'character' ) {
    'string'
  } else if ( rType == 'integer' ) {
    'int'
  } else if ( rType == 'factor' ) {
    'factor'
  } else if ( rType == 'POSIXct' ) {
    'date'
  } else {
    stop('Unknown R type: ', rType)
  }
}

# Get data type of eSet
getMASVDataType = function(eset) {
  # Get assay data
  assayData = Biobase::exprs(eset)
  # Get assay data class
  assayDataClass = typeof(assayData)
  # Get MASV data type
  dataType = getMASVType(assayDataClass)
  # Return data type
  dataType
}

# Convert NAs to empty character strings
convertNAs = function(x) {
  x = sapply(x, as.character)
  x[is.na(x)] = ''
  x
}

eSetToMASV = function(eset, filename) {
  # Open connection
  con = file(filename, open='w')
  # Get data type
  dataType = getMASVDataType(eset)
  # Get sample names
  sampleNames = colnames(eset)
  # Get feature names
  featureNames = rownames(eset)
  # Get data
  data = Biobase::exprs(eset)
  # Get covariate names
  covariates = Biobase::pData(eset)
  covariateNames = colnames(covariates)
  # Get meta-feature names
  metaFeatures = Biobase::fData(eset)
  metaFeatureNames = colnames(metaFeatures)


  # Get version
  version = 1
  # Get top left
  topLeft = paste0('MASV:', version)
  # Get and write first row string
  firstRow = paste(topLeft, '', paste0(featureNames, collapse='\t'), paste0(covariateNames, collapse='\t'), sep='\t')
  writeLines(firstRow, con)

  # Get covariate types
  covariateTypes = sapply(covariates, function(x) getMASVType(class(x)))

  # Get and write second row string
  secondRow = paste('', dataType, paste(rep('', length(featureNames)), collapse='\t'), paste(covariateTypes, collapse='\t'), sep='\t')
  writeLines(secondRow, con)

  for ( i in 1:ncol(data) ) {
    # Get row string
    row = paste(sampleNames[i], '', paste0(convertNAs(data[,i]), collapse='\t'), paste0(convertNAs(covariates[i,]), collapse='\t'), sep='\t')
    # Write row string
    writeLines(row, con)
  }

  for ( i in 1:length(metaFeatureNames) ) {
    metaFeatureType = getMASVType(class(metaFeatures[,i]))

    # Get row string
    row = paste(metaFeatureNames[i], metaFeatureType, paste0(convertNAs(metaFeatures[,i]), collapse='\t'), paste(rep('', length(covariateNames)), collapse='\t'), sep='\t')
    # Write row string
    writeLines(row, con)
  }

  # Close connection
  close(con)
}

multi_data_set_to_masv = function(multi, filename) {
  # Open connection
  con = file(filename, open='w')
  
  data_sets = list()
  dataTypes = list()
  feature_sets = list()
  sets_metafeatures = list()
  
  for ( set in names(multi)) {
    # Get data
    data = assayData(multi[,set])
    data = data[[1]]
    data = data$exprs
    data_sets[[set]] = data
    
    # Get feature names
    feature_sets[[set]] = list(rownames(data))
    #feature_names = c(feature_names, rownames(data))
    
    # Get data type
    dataType = getMASVType(type(data))
    dataTypes[set] = dataType
    
    # Get meta-feature data
    f_data = fData(multi[,set])
    f_data = f_data[[1]]
    sets_metafeatures[[set]] = f_data
  }
  
  # Get sample names
  first_data_set = names(data_sets)[1]
  sampleNames = colnames(data_sets[[first_data_set]])
  
  # Get covariate names
  covariates = pData(multi[,first_data_set])[[1]]
  covariates[length(covariates)] = NULL
  covariateNames = colnames(covariates)
  
  # Get meta-feature names
  metaFeatureNames = colnames(sets_metafeatures[first_data_set])
  
  # Get version
  version = 1
  # Get top left
  topLeft = paste0('MASV:', version)
  
  # Get feature names
  feature_names = ''
  num_features = 0
  for (features in feature_sets) {
    num_features = num_features + length(features[[1]]
    features_str = paste('', paste(features[[1]], collapse = '\t'), sep='\t')
    if (nchar(feature_names) > 1){
      feature_names = paste(feature_names, features_str, sep='\t')
    } else {
      feature_names = features_str
    }
  }

  
  # Get and write first row string
  firstRow = paste(topLeft, feature_names, paste0(covariateNames, collapse='\t'), sep='\t')
  writeLines(firstRow, con)
  
  # Close connection
  close(con)
}

multi_data_set_to_masv(multi, './inst/output_test.tsv')
