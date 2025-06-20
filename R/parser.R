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
    
  # Get positions of feature and covariate columns
  covariateNamesStart = featureNamesEnd + 1
  covariateNamesEnd = length(secondCols)

  # Get feature types
  #dataType = secondCols[2]
  
  current_type = NULL
  group_length = 0
  feature_groups = c()
  group_names = c()
 
  featureNamesStart = 3
  featureNamesEnd = 3
  
  for (i in 3:length(secondCols)){
    print(i)
    if ( secondCols[i] != "" ){
      print(secondCols[i])
      if (group_length > 0) {
        feature_groups = append(feature_groups,c(current_type, group_length))
        group_names = append(group_names,thirdCols[i])
        featureNamesEnd = featureNamesEnd + group_length
      }
      
    current_type = secondCols[i]
    group_length = 0
    
    } else {
        group_length = group_length + 1
    }
  }
  
  # Get positions of feature and covariate columns
  covariateNamesStart = featureNamesEnd + 1
  covariateNamesEnd = length(secondCols)
  
  close(con)
  return(group_names)
  
  # Get feature names
  featureNames = as.character(unlist(firstCols[featureNamesStart:featureNamesEnd]))
  covariateNames = as.character(unlist(firstCols[covariateNamesStart:covariateNamesEnd]))

  # Create empty vector to store data
  data = vector(getRType(dataType), 0)

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
      parsedData = parseData(cols[featureNamesStart:featureNamesEnd], dataType)
      data = append(data, parsedData)

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

  # Convert data to a matrix
  dim(data) = c(length(featureNames), nrow(covariates))
  dimnames(data) = list(featureNames, sampleNames)

  # Convert covariates to a data frame
  rownames(covariates) = sampleNames
  for ( i in 1:ncol(covariates) ) {
    covariates[,i] = parseData(covariates[,i], secondCols[covariateNamesStart + i - 1])
  }

  # Convert meta-features to a data frame
  metaFeatures = as.data.frame(metaFeatures)
  rownames(metaFeatures) = featureNames

  # Return the data
  list(data=data, covariates=covariates, metaFeatures=metaFeatures)
}

parseExpressionSet = function(filename) {
  parsedData = parseMASVFile(filename)
  data = parsedData$data
  covariates = parsedData$covariates
  metaFeatures = parsedData$metaFeatures

  # Create an ExpressionSet
  eset = Biobase::ExpressionSet(assayData=data,
                       phenoData=Biobase::AnnotatedDataFrame(covariates),
                       featureData=Biobase::AnnotatedDataFrame(metaFeatures))
  eset
}

feature_groups = parseMASVFile('./inst/masv2test.tsv')
