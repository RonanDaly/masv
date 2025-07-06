# MASV File Format
# ----------------
# A MASV file is a tab-seperated file that is used to store data and metadata.
# In a version 1 file, the data consists of multiple named features, all of the same type and
# a number of samples. The metadata consists of named features associated with the
# samples, such as covariates and experimental conditions and named meta-features associated
# with the features. Each covariate and meta-feature has it's own type.
# The data and metadata can have missing values which are given as an empty cell.
# The file can be thought of as a sequence of rows, each of which contains a number of cells,
# seperated by tabs
#
# Types in this file format consist of the following:
# * float: A floating point type
# * string: A string type
# * int: An integer type
# * factor: A factor type
# * date: A data, time or date-time in ISO 8601 format
#
# The first row consists of the following:
# * The first cell is the string "MASV:1", where "1" stands for the version of the standard
# * Next comes a blank cell
# * Next come the names of all the features in the data, one for each cell
# * Next come the names of all the covariates, one for each cell
#
# The second row consists of the following:
# * The first cell is blank
# * The next cell contains the type of the data
# * Next come blank cells, one for each for each data feature
# * Next come type cells, one for each covariate
#
# From then on, each row consists of:
# * The first cell is the name of a sample
# * Next comes a blank cell
# * Next come data value cells, one for each feature
# * Next come covariate value cells, one for each covariate
#
# After this, each row consists of:
# * The first cell is the name of a meta-feature
# * Next comes a type cell of that meta-feature,
# * Next come meta-feature values cells, one for each feature

checkMASVFile = function(topLeft) {
  split = strsplit(topLeft, ':', fixed=TRUE)[[1]]
  if ( split[1] != "MASV") {
    stop('MASV file magic not correct: magic must begin with MASV')
  }
  version = tryCatch({
      as.integer(split[2])
    },
    warning = function(e) {
      stop('MASV file magic not correct: version must be an integer')
    }
  )
}

getMASVVersion = function(topLeft) {
  split = strsplit(topLeft, ':', fixed=TRUE)[[1]]
  as.integer(split[2])
}

# Get R type of given data type
getRType = function(masvDataType) {
  if ( masvDataType == 'float' ) {
    'double'
  } else if ( masvDataType == 'string' ) {
    'character'
  } else if ( masvDataType == 'int' ) {
    'integer'
  } else if ( masvDataType == 'factor' ) {
    'factor'
  } else if ( masvDataType == 'date' ) {
    'POSIXct'
  } else {
    stop('Unknown data type: ', masvDataType)
  }
}

# Parse a row of data.
# cols: A vector of strings, each of which is a cell in the row
# dataType: The type of the data in the row. Either 'float', 'string', 'int', 'factor' or 'date',
parseData = function(cols, dataType) {
  if ( dataType == 'float' ) {
    as.numeric(cols)
  } else if ( dataType == 'string' ) {
    cols
  } else if ( dataType == 'int' ) {
    as.integer(cols)
  } else if ( dataType == 'factor' ) {
    factor(cols, exclude='')
  } else if ( dataType == 'date' ) {
    as.POSIXct(cols)
  } else {
    stop('Unknown MASV type: ', dataType)
  }
}

# Parse a row of 
#parseFeatureGroups

parseMASVFile = function(filename) {
  con = file(filename, open='r')
  firstLine = readLines(con, n=1, ok=FALSE)
  firstCols = strsplit(firstLine, '\t', fixed=TRUE)[[1]]
  topLeft = firstCols[1]
  checkMASVFile(topLeft)
  version = getMASVVersion(topLeft)
  if ( version != 1 ) {
    stop("Can only handle MASV version 1 files")
  }

  secondLine = readLines(con, n=1, ok=FALSE)
  secondCols = strsplit(secondLine, '\t', fixed=TRUE)[[1]]
  
  thirdLine = readLines(con, n=1, ok=FALSE)
  thirdCols = strsplit(thirdLine, '\t', fixed=TRUE)[[1]]

  # Get feature types
  #dataType = secondCols[2]
  
  
  feature_groups = list()
  group_names = c()
 
  featureNamesStart = 3
  featureNamesEnd = 3
  
  current_type = NULL
  group_start = 3
  group_end = 0
  for (i in 2:length(secondCols)){
    #print(i)
    if ( secondCols[i] != "" ){
      #print(secondCols[i])
      if (group_end > group_start) {
        #feature_groups = append(feature_groups,c(current_type, group_start, group_end))
        feature_groups[[length(feature_groups)+1]] = list(current_type, group_start, group_end)
        group_names = append(group_names,thirdCols[i])
        featureNamesEnd = group_end
      }
      
    current_type = secondCols[i]
    group_start = i+1
    
    } else {
        group_end = i
    }
  }
  
  # Get positions of feature and covariate columns
  covariateNamesStart = featureNamesEnd + 1
  covariateNamesEnd = length(secondCols)
  
  
  # Get feature names
  featureNames = as.character(unlist(firstCols[featureNamesStart:featureNamesEnd]))
  covariateNames = as.character(unlist(firstCols[covariateNamesStart:covariateNamesEnd]))

  #close(con)
  #return(feature_groups)
  
  # Create empty vector to store data
  data = list()
  iter = 1
  for (group in feature_groups) {
    #print(group)
    data[[iter]] = vector(getRType(group[1]), 0)
    iter = iter + 1
  }
  
  #close(con)
  #return(data)
  
  # Create dataframe to store covariates
  covariates = data.frame(matrix(nrow=0, ncol=length(covariateNames), dimnames=list(NULL, covariateNames)))
  sampleNames = c()
  metaFeatures = list()

  while (TRUE) {
    line = readLines(con, n=1, warn=FALSE)
    if ( length(line) == 0 ) {
      break
    }
    cols = strsplit(line, '\t', fixed=TRUE)[[1]]
    if ( length(cols) == length(firstCols) - 1 ) {
      # There is a trailing tab
      cols = c(cols, '')
    }

    # Get type
    rowType = cols[2]
    if ( rowType == '' ) {
      # This is a data row
      # Get sample name
      sampleName = cols[1]
      sampleNames = c(sampleNames, sampleName)
      
      # Parse the data for this sample
      for (i in 1:length(feature_groups)) {
        group = unlist(feature_groups[[i]])
        group_start = as.integer(group[2])
        group_end = as.integer(group[3])
        dataType = group[1]
        #print(group_end)
        parsedData = parseData(cols[group_start:group_end], dataType)
        data[[i]] = append(data[[i]],parsedData)
      }
      
      #parsedData = parseData(cols[featureNamesStart:featureNamesEnd], dataType)
      
      #data = append(data, parsedData)

      covariateRow = cols[covariateNamesStart:covariateNamesEnd]
      covariates[nrow(covariates) + 1,] = covariateRow
    } else {
      # This is a meta-feature row
      # Get meta-feature name
      metaFeatureName = cols[1]
      # Get meta-feature type
      metaFeatureType = cols[2]
      # Parse the meta-feature data
      parsedMetaFeatureData = parseData(cols[featureNamesStart:featureNamesEnd], metaFeatureType)
      # Add the meta-feature data to the data frame
      metaFeatures[[metaFeatureName]] = parsedMetaFeatureData
    }
  }

  # Close the file
  close(con)
  #return(data)
  #print(featureNames)
  for (i in 1:length(feature_groups)) {
      group = unlist(feature_groups[[i]])
      group_start = as.integer(group[2]) - 2 
      group_end = as.integer(group[3]) - 2
      #group_length = group_end - group_start
      group_feature_names = featureNames[group_start:group_end]
      # Convert data to a matrix
      dim(data[[i]]) = c(length(group_feature_names), nrow(covariates))
      dimnames(data[[i]]) = list(group_feature_names, sampleNames)
  }
  
    # Convert covariates to a data frame
    rownames(covariates) = sampleNames
    for ( i in 1:ncol(covariates) ) {
      covariates[,i] = parseData(covariates[,i], secondCols[covariateNamesStart + i - 1])
    }
  
    # Convert meta-features to a data frame
    metaFeatures = as.data.frame(metaFeatures)
    rownames(metaFeatures) = featureNames
  
  # Return the data
    return(list(data=data, covariates=covariates, metaFeatures=metaFeatures,feature_groups=feature_groups, group_names=group_names))
}

parseExpressionSets = function(filename) {
  parsedData = parseMASVFile(filename)
  data = parsedData$data
  covariates = parsedData$covariates
  metaFeatures = parsedData$metaFeatures
  feature_groups = parsedData$feature_groups
  group_names = parsedData$group_names
  
  e_sets = list()
  for (i in 1:length(data)) {
    group_data = data[[i]]
    group = unlist(feature_groups[[i]])
    group_start = as.integer(group[2]) - 2 
    group_end = as.integer(group[3]) - 2
    group_metafeatures = metaFeatures[group_start:group_end,]
    # Create an ExpressionSet
    e_set = Biobase::ExpressionSet(assayData=group_data,
                         phenoData=Biobase::AnnotatedDataFrame(covariates),
                         featureData=Biobase::AnnotatedDataFrame(group_metafeatures))
    e_sets[[i]] = e_set
  }
  return(list(e_sets=e_sets, group_names=group_names))
}

parseMultiDatSet = function(filename) {
  multi = createMultiDataSet()
  e_set_list = parseExpressionSets(filename)
  e_sets = e_set_list$e_sets
  group_names = e_set_list$group_names
  
  for (i in 1:length(e_sets)) {
    e_set = e_sets[[i]]
    multi = add_eset(multi, e_set, dataset.type = group_names[[i]], GRanges = NA)
  }
  
  return(multi)
}

#data = parseMASVFile('./inst/masv2test.tsv')
#meta_feats = data$metaFeatures[1:3,]
#class(meta_feats)
#print(feature_groups['factor'])
#test_data = data['data'][[1]]
#group = test_data[[2]]
e_sets = parseExpressionSets('./inst/masv2test.tsv')
e_set = e_sets[[2]]

multi = parseMultiDatSet('./inst/masv2test.tsv')
