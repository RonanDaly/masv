# Output eSet to masv file

#' Get MASV type of given R type
#'
#' @param rType A string.
#' @returns A string
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

#' Get data type of eSet
#'
#' @param eset A ExpressionSet.
#' @returns A string
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

#' Convert NAs to empty character strings
#'
#' @param x A string vector.
#' @returns A string vector.
convertNAs = function(x) {
  x = sapply(x, as.character)
  x[is.na(x)] = ''
  x
}

#' Write MASV file from ExpressionSet
#'
#' @param eset A ExpressionSet.
#' @param filename A string.
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

#' Write MASV file from MultiDataSet
#'
#' @param multi A MultiDataSet.
#' @param filename A string.
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
  metaFeatureNames = colnames(sets_metafeatures[[first_data_set]])
  
  # Get version
  version = 1
  # Get top left
  topLeft = paste0('MASV:', version)
  
  # Get feature names and number of columns for features
  feature_names = ''
  for (features in feature_sets) {
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
  
  # Get covariate types
  covariateTypes = sapply(covariates, function(x) getMASVType(class(x)))
  
  #firstRow = topLeft
  secondRow = ''
  thirdRow = ''
  for (set in names(data_sets)) {
    dataType = dataTypes[set][[1]]
    features = feature_sets[set][[1]][[1]]
    secondRow = paste(secondRow, dataType, paste(rep('', length(features)), collapse='\t'), sep='\t')
    thirdRow = paste(thirdRow, set, paste(rep('', length(features)), collapse='\t'), sep='\t')
  }
  
  secondRow = paste(secondRow, paste(covariateTypes, collapse='\t'), sep='\t')
  thirdRow = paste(thirdRow, paste(rep('', length(covariates)), collapse='\t'), sep='\t')
  
  writeLines(secondRow, con)
  writeLines(thirdRow, con)
  
  for (i in 1:length(sampleNames)) {
    row = sampleNames[i]
    
    for (set in names(data_sets)) {
      a_data = data_sets[[set]]
      row = paste(row, '', paste0(convertNAs(a_data[,i]), collapse='\t'), sep='\t')
    }
    # Get row string
    row = paste(row, paste0(convertNAs(covariates[i,]), collapse='\t'), sep='\t')
    # Write row string
    writeLines(row, con)
  }
  
  for ( i in 1:length(metaFeatureNames) ) {
    metaFeatureType = getMASVType(class(sets_metafeatures[[first_data_set]][,i]))
    
    # Get row string
    row = paste(metaFeatureNames[i], metaFeatureType,sep='\t')
    first_set = TRUE
    for (set in names(data_sets)) {
      meta_data = sets_metafeatures[[set]][,i]
      #t = paste0(convertNAs(meta_data[,1]), collapse='\t')
      #t = convertNAs(meta_data[,1])
      if (first_set == TRUE) {
        row = paste(row, paste0(convertNAs(meta_data), collapse='\t'), sep='\t')
      } else {
        row = paste(row, '', paste0(convertNAs(meta_data), collapse='\t'), sep='\t')
      }
      first_set = FALSE
    }
    
    row = paste(row, paste(rep('', length(covariateNames)), collapse='\t'), sep='\t')
    # Write row string
    writeLines(row, con)
  }
  
  # Close connection
  close(con)
}

#multi_data_set_to_masv(multi_test, './inst/output_test2.tsv')
