################################################################################
# # wrapWS.R
# # R Versions: 2.12.1 2.14.0
# #
# # Author(s): Víctor H. Cervantes
# #
# # Process: Interaction with Winsteps from R
# # Description: Functions for reading Winsteps output files
# #              and for running some some basic setups of data
# #              from R using Winsteps
# #
# # Inputs: None
# #
# # Outputs: Functions
# #
# # File history:
# #   201007xx: Creation
# #   20111111: Modified to comply with R style
# #
# # TODO: Modify ReadDifTable function so that it may retrieve a Table 30 where
# #       more than two groups were used for DIF calculation
################################################################################

################################################################################
# # Functions for reading Winsteps output files
################################################################################
ReadIfile <- function (fileName, filePath = "./", sep = NULL) {
  # # This function reads the item parameters from a Winsteps IFILE
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #  sep: Null by default if the IFILE was produced as a fixed width format
  # #       else it speciies the separation character
  # #
  # # Ret:
  # #  iPar: The item difficulties and fit indices
  
  inFile <- paste(filePath, fileName, sep = "")
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }

  if (is.null(sep)) {
    iPar <- read.fwf(file = inFile, widths = c(1, 5, 8, 3, 8, 9, rep(7, 8), 6, 6, 7, 6, 2, 2, 30),
      skip = 2, header = FALSE)
  } else {
    iPar <- read.delim(file = inFile, sep = sep, skip = 2, header = FALSE)
  }

  estimatedItems <- iPar[, 4] != -3
  iPar <- iPar[estimatedItems, ]
  iPar <- data.frame(order = iPar[, 2], item = iPar[, 21], difficulty = iPar[, 3], se = iPar[, 7],
                     estimatedStatus = iPar[, 4],
                     infit = iPar[, 8], outfit = iPar[, 10], row.names = iPar[, 21])

  return(iPar)
}

ReadSfile <- function (fileName, filePath = "./", sep = NULL) {
  # # This function reads the Andreich threshold parameters from Winsteps SFILE
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #  sep: Null by default if the SFILE was produced as a fixed width format
  # #       else it speciies the separation character
  # #
  # # Ret:
  # #  iThres: The item Andrich thresholds

  inFile <- paste(filePath, fileName, sep = "")
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }

  inFile <- file(inFile, "r")

  titleLine <- readLines(inFile, n = 1)  # Read and drop the firs line from the sfile
  titleLine <- readLines(inFile, n = 1)  # Read the titles lines
  titles    <- unlist(strsplit(titleLine, sep))

  if (length(titles) == 2) {
    widths <- c(3, 7)
  } else {
    widths <- c(6, 3, 7)
  } 


  if (is.null(sep)) {
    iThres <- read.fwf(file = inFile, widths = widths,
      skip = 0, header = FALSE)
  } else {
    iThres <- read.delim(file = inFile, sep = sep, skip = 0, header = FALSE)
  } 

  close(inFile)

  if (length(titles) == 2) {
    iThres <- data.frame(category = iThres[, 1], threshold = iThres[, 2], 
                         row.names = paste(iThres[, 1], iThres[, 1], sep = "."))
  }  else {
    iThres <- data.frame(order = iThres[, 1], category = iThres[, 2], threshold = iThres[, 3], 
                         row.names = paste(iThres[, 1], iThres[, 2], sep = "."))
  } 

  return(iThres)
}

ReadPfile <- function (fileName, filePath = "./", sep = NULL) {
  # # This function reads person ability estimates from Winsteps PFILE
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #  sep: Null by default if the PFILE was produced as a fixed width format
  # #       else it speciies the separation character
  # #
  # # Ret:
  # #  pPar: The person abilities

  inFile <- paste(filePath, fileName, sep = "")
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }

  if (is.null(sep)) {
    pPar <- read.fwf(file = inFile, widths = c(1, 5, 8, 3, 8, 9, rep(7, 8), 6, 6, 30),
      skip = 2, header = FALSE)
  } else {
    pPar <- read.delim(file = inFile, sep = sep, skip = 2, header = FALSE)
  }
  pPar <- data.frame(personId = pPar[, 17], ability = pPar[, 3], se = pPar[, 7],
                     estimatedStatus = pPar[, 4], infit = pPar[, 8], outfit = pPar[, 10], row.names = pPar[, 17])

  return(pPar)
}

ReadSCfile <- function (fileName, filePath = "./", sep = NULL) {
  # # This function reads ability estimates from Winsteps SCOREFILE
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #  sep: Null by default if the SCOREFILE was produced as a fixed width format
  # #       else it speciies the separation character
  # #
  # # Ret:
  # #  scores: The abilities estimated for each score value

  inFile <- paste(filePath, fileName, sep = "")
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }

  if (is.null(sep)) {
    scores <- read.fwf(file = inFile, widths = c(6, 9, 8, 7, 6, 5, 8, 6, 8, 6, 10),
      skip = 3, header = FALSE)
  } else {
    scores <- read.delim(file = inFile, sep = sep, skip = 3, header = FALSE)
  }
  scores <- data.frame(score = scores[, 1], ability = scores[, 2], se = scores[, 3],
    frequencies = scores[, 7], percentage = scores[, 8], row.names = scores[, 1])

  return(scores)
}

ReadDifTable <- function (fileName, filePath = "./") {
  # # This function extracts table 30.1 from Winsteps log output
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #  sep: Null by default if the SCOREFILE was produced as a fixed width format
  # #       else it speciies the separation character
  # #
  # # Ret:
  # #  dif

  GetDif <- function (logLine) {
    # # This function extracts the relevant DIF information from the table in
    # # a line
    difInfo  <- as.numeric(strsplit(logLine, "\\s+")[[1]][c(8, 12)])
    itemName <- strsplit(logLine, "\\s+")[[1]][16]
    difInfo  <- list(difInfo = difInfo, itemName = itemName)
    return(difInfo)
  }

  isFound <- 0

  inFile <- paste(filePath, fileName, sep = "")
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }

  logFile <- file(inFile, open = "r")
  while (isFound != 1) {
    logLine <- readLines(logFile, n = 1)
    if (grepl("TABLE 30.1", logLine)) {
      nItems <- readLines(logFile, n = 1)
      nItems <- sub("([[:digit:]]) ITEMS.*", "\\1", nItems)
      nItems <- as.numeric(sub(".* ([[:digit:]])", "\\1", nItems))
      
      readLines(logFile, n = 8)

      difLines <- readLines(logFile, n = nItems * 2)
      difLines <- difLines[seq(2, nItems * 2, by = 2)]

      dif   <- matrix(nrow = nItems, ncol = 2)
      items <- character(length = nItems)
      for (ii in 1:nItems) {
        itemInfo  <- GetDif(difLines[ii])
        dif[ii, ] <- itemInfo$difInfo
        items[ii] <- itemInfo$itemName
      }
      dif <- data.frame(items, dif)
      names(dif) <- c("item", "diferencia", "pValue")
      
      isFound <- 1
    }

    if (length(logLine) == 0) {
      break
    }
  }
  close(logFile)

  if (isFound == 0) {
    stop("File", inFile, "in", filePath, "does not contain table 30.1\n")
  } else {
    return(dif)
  }
}


ReadContrast1Table <- function (fileName, filePath = "./") {
  # # This function extracts tables 23.0 and 23.6 from Winsteps log output
  # #
  # # Arg:
  # #  fileName: The file name
  # #  filePath: The file path
  # #  sep: Null by default if the SCOREFILE was produced as a fixed width format
  # #       else it speciies the separation character
  # #
  # # Ret:
  # #  dif

  # # Eigenvalues
  GetEigenValues <- function (logLine) {
    # # This function extracts the eigen values from the table
    values <- strsplit(logLine, "=")[[1]][2]
    eigenValue  <- as.numeric(strsplit(values, "\\s+")[[1]][2])
    return(eigenValue)
  }
  isFound <- 0

  inFile <- paste(filePath, fileName, sep = "")
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }

  logFile <- file(inFile, open = "r")
  while (isFound != 1) {
    logLine <- readLines(logFile, n = 1)
    if (grepl("TABLE 23.0", logLine)) {
      readLines(logFile, n = 6)

      eigenLines <- readLines(logFile, n = 8)

      eigenValues <- numeric(length = 8)
      for (ii in 1:8) {
        eigenValues[ii] <- GetEigenValues(eigenLines[ii])
      }

      eigenNames <- c("total", "explained", "unexplained", paste("contrast", 1:5, sep = "."))
      eigenValues <- data.frame(eigenNames, eigenValues)
      names(eigenValues) <- c("names", "eigenValues")
      
      isFound <- 1
    }

    if (length(logLine) == 0) {
      break
    }
  }
  close(logFile)

  if (isFound == 0) {
    stop("File", inFile, "in", filePath, "does not contain table 23.0\n")
  }


  # # Loadings
  GetContrast <- function (logLine) {
    # # This function extracts the relevant contrast information from the table in
    # # a line
    contrast  <- as.numeric(strsplit(logLine, "\\s+")[[1]][4])
    itemName <- strsplit(logLine, "\\s+")[[1]][11]
    contInfo  <- list(contrast = contrast, itemName = itemName)
    return(contInfo)
  }

  isFound <- 0

  inFile <- paste(filePath, fileName, sep = "")
  if (!file.exists(inFile)) {
    stop(fileName, " not found in ", filePath, "\n")
  }

  logFile <- file(inFile, open = "r")
  while (isFound != 1) {
    logLine <- readLines(logFile, n = 1)
    if (grepl("TABLE 23.6", logLine)) {
      nItems <- readLines(logFile, n = 1)
      nItems <- sub("([[:digit:]]) ITEMS.*", "\\1", nItems)
      nItems <- as.numeric(sub(".* ([[:digit:]])", "\\1", nItems))
      
      readLines(logFile, n = 15)

      conLines <- readLines(logFile, n = nItems)

      contrast1 <- numeric(length = nItems)
      items     <- character(length = nItems)
      for (ii in 1:nItems) {
        itemInfo      <- GetContrast(conLines[ii])
        contrast1[ii] <- itemInfo$contrast
        items[ii]     <- itemInfo$itemName
      }
      contrast1 <- data.frame(items, contrast1)
      names(contrast1) <- c("item", "contrast1")
      
      isFound <- 1
    }

    if (length(logLine) == 0) {
      break
    }
  }
  close(logFile)

  if (isFound == 0) {
    stop("File", inFile, "in", filePath, "does not contain table 23.6\n")
  }

  contrast1 <- list(eigenValues = eigenValues, loadings = contrast1)
}



################################################################################
# # Function to generate Winsteps control files and running the program
################################################################################
RunWinsteps <- function (responses, runName, outPath = "../output", runPath = "../output", 
                         group = NULL, weights = NULL, personIds = NULL, itemIds = NULL, 
                         keysItems = NULL, score = FALSE, stBias = TRUE, dif = FALSE, dimensionContrasts = FALSE,
                         rsm = NULL, tables = NULL, itemConstraints = NULL, personConstraints = NULL,
                         quotes = "Y", sep = "\t", verbose = FALSE, itSelect = NULL, psSelect = NULL, 
                         runProgram = TRUE) {
  # # This function generates a Winsteps control file given the options in its
  # # arguments, runs it and reads the item parameters
  # #
  # # Arg:
  # #  responses: numeric matrix or data.frame containing the (coded) responses to the items
  # #  group: vector with group numbers or single character codes of group membership
  # #  runName: name of the stem used for the files related with the analyses
  # #  outPath: path where the files will be stored
  # #  runPath: path that is put in the command file of winsteps
  # #  weights: vector with the weights to be asigned to each responses register
  # #  personIds: vector with the ids to be written for each responses register
  # #  itemIds: vector with the ids for each item
  # #  keysItems: vector with the keys for each item
  # #  score: logical indicating whether or not person abilities should be obtained and stored
  # #  stBias: logical indicating whether the standar bias correction implemented by Winsteps ought to be used
  # #  dif: logical indicating whether to request table 30 (DIF calculations) from Winsteps
  # #  dimensionContrasts: logical indicating whether or not to request table 23 (dimension analyses) from Wisnteps
  # #  tables: vector containing the numbers of the tables to be requested from Winsteps
  # #  itemConstraints: matrix containing the item positions in the first column and the difficulty values (in the
  # #                   second) to be constrained
  # #  personConstraints: matrix containing the person positions in the first column and the ability values (in the
  # #                     second) to be constrained
  # #  rsm: vector indicating the groups of items with the same rating scale model
  # #  quotes: character indicating if quotes (if equal to Y) sholud be placed or nor (if euqal to N) to non numeric
  # #          valued outpu
  # #  sep: character indicating the separation character to be used in the Winsteps output files. 
  # #  verbose: logical indicating whether to show or not function progres
  # #  itSelect: string to exclude items of analysis
  # #  psSelect: string to exclude persons of analysis
  # #  runProgram: logical indicating whether to execute winstep
  # #
  # # Ret:
  # #  iPar: returns the estimated item parameters

  ################################################################################
  # # Check arguments consistency
  ################################################################################
  if (!is.null(keysItems)){
    if (length(keysItems) != ncol(responses)){
      stop("keysItems length and responses number of columns should be equal")
    }
  }

  if (!is.null(group)) {
    if (length(group) != nrow(responses)) {
      stop("group length and responses number of rows should be equal")
    }
  }
  if (!is.null(weights)) {
    if (length(weights) != nrow(responses)) {
      stop("weights length and responses number of rows should be equal")
    }
  }
  if (!is.null(personIds)) {
    if (length(personIds) != nrow(responses)) {
      stop("personIds length and responses number of rows should be equal")
    }
  }
  if (!is.null(itemIds)) {
    if (length(itemIds) != ncol(responses)) {
      stop("itemIds length and responses number of columns should be equal")
    }
  }
  if (!is.null(rsm)) {
    if (length(rsm) != ncol(responses)) {
      stop("rsm length and responses number of columns should be equal")
    }
  }

  if (dif) {
    if (is.null(group)) {
      stop("Group must contain the group vector in order to run dif procedures\n")
    }
    tables <- unique(c(tables, 30))
  } else {
    if (!is.null(group)) {
      if (verbose) {
        cat("dif not requested.\ngroup will not be used\n")
      } 
    }
  }
  if (dimensionContrasts) {
    tables <- unique(c(tables, 23))
  }

  ################################################################################
  # # General variables and filenames
  ################################################################################
  # # Responses dimension
  nItems    <- ncol(responses)
  nPersons  <- nrow(responses)

  # # File names
  dataFileName  <- paste(runName, ".dat", sep = "")
  idFileNanem   <- paste(runName, ".IDFILE", sep = "")
  runNameII     <- file.path(outPath, runName)
  runName       <- file.path(runPath, runName)
  

  commandFile <- paste0(runName, ".con")
  logFile     <- paste0(runName, ".out")
  dataFile    <- paste0(runName, ".dat")
  idFile      <- paste0(runName, ".IDFILE")
  iFile       <- paste0(runNameII, ".IFILE")
  disFile     <- paste0(runNameII, ".DISFILE")
  sFile       <- paste0(runNameII, ".SFILE")
  pFile       <- paste0(runNameII, ".PFILE")
  scoreFile   <- paste0(runNameII, ".SCOREFILE")
  if (!is.null(weights)) {
    weightsFile  <- paste(runName,    ".wts", sep = "")
  }
    
  ################################################################################
  # # Preparing the data file
  ################################################################################
  if (!is.null(personIds)) {
    if (is.numeric(personIds)) {
      typeId <- "d" # # If numeric then add zeroes before the shorter ones
    } else {
      typeId <- "s" # # If not numeric then add spaces before the shortest ones
    }
  } else {
    personIds <- 1:nPersons
    typeId <- "d"
  }
  maxLengthId <- max(nchar(personIds))
  personIds   <- sprintf(paste0("\"%", maxLengthId, typeId, "\""), personIds)
  personIds   <- gsub("\"", "", personIds)

  if (!is.null(group)) {
    if (is.numeric(group)) {
      typeGroupId <- "d" # # If numeric then add zeroes before the shorter ones
    } else {
      typeGroupId <- "s" # # If not numeric then add spaces before the shortest ones
    }
  } else {
    group <- rep(1, nrow(responses))
    typeGroupId <- "d"
  }
  groupWidth <- max(nchar(group))
  group      <- sprintf(paste("\"%0", groupWidth, typeGroupId, "\"", sep = ""), group)
  group      <- gsub("\"", "", group)

  dataMatrix <- cbind(personIds, group, responses)
  write.table(dataMatrix, file = dataFile, append = FALSE, sep = "", row.names = FALSE, 
              col.names = FALSE, quote = FALSE)
  if (verbose) cat("Datafile prepared\n")

  ################################################################################
  # # Preparing the control file
  ################################################################################
  # # Title section

  cat(";Creado el ", format(Sys.time(), "%d/%m/%Y %H:%M"), "\n",  file = commandFile)
  cat("TITLE = Running Winsteps from R\n", file = commandFile, append = TRUE)
  
  # # Prelims section - formatting and such
  item1  <- maxLengthId + groupWidth + 1
  if (!is.null(itemIds)) {
    itLength <- max(nchar(itemIds))
  } else {
    itLength <- floor(log(nItems, 10)) + 3
  }
  
  if (sep == "\t") {
    cat("CSV = T\n",                 sep = " ", file = commandFile, append = TRUE)
  } else if (sep == ",") {
    cat("CSV = Y\n",                 sep = " ", file = commandFile, append = TRUE)
  } else {
    cat("CSV = N\n",                 sep = " ", file = commandFile, append = TRUE)
  }
  cat("DISCRIM = Y\n",               sep = " ", file = commandFile, append = TRUE)
  if (!is.null(rsm)) {
    cat("ISGROUPS = ", rsm, "\n",     sep = "", file = commandFile, append = TRUE)
  } else {
    cat("ISGROUPS = 0\n",             sep = "", file = commandFile, append = TRUE)
  }
  # # General output commands
  cat("HLINES = Y\n",                sep = " ", file = commandFile, append = TRUE)
  cat("ITEM = ITEM\n",               sep = " ", file = commandFile, append = TRUE)
  cat("KEYSCR = 1\n",               sep = " ", file = commandFile, append = TRUE)
  cat("LCONV = 0.001\n",             sep = " ", file = commandFile, append = TRUE)
  cat("MODELS = R\n",             sep = " ", file = commandFile, append = TRUE)
  cat("PERSON = ESTUDIANTE\n",             sep = " ", file = commandFile, append = TRUE)
  cat("PTBISERIAL = N\n",            sep = " ", file = commandFile, append = TRUE)
  cat("PVALUE = Y\n",                sep = " ", file = commandFile, append = TRUE)
  cat("RCONV = 0.01\n",              sep = " ", file = commandFile, append = TRUE)
  tabs <- "0000000000000000000000000000000000"
  if (!is.null(tables)) {
    for (ii in tables) {
      substring(tabs, ii, ii) <- "1"
    }
  }
  cat("TABLES =", tabs, "\n",        sep = " ", file = commandFile, append = TRUE)
  cat("UDECIM = 4\n",                sep = " ", file = commandFile, append = TRUE) 
  # # Response codes and polytomous model specification
  if (is.null(keysItems)) {
    responseCodes <- names(table(as.matrix(responses)))
    for (ii in seq(along = responseCodes[-1])) {
      responseCode <- paste(rep(responseCodes[ii + 1], nItems))
      cat("KEY", ii, " = ", responseCode, sep = "", "\n",
                                                file = commandFile, append = TRUE)
    }
  } else {
    responseCodes <- paste(sort(unique(keysItems)), collapse = "")
  }
  cat("CODES = ", responseCodes, "\n", sep = "", file = commandFile, append = TRUE)
  cat("DATA = ", dataFileName, "\n",    sep = "", file = commandFile, append = TRUE)
  if (is.null(itSelect)) {
    itSelect <- paste(rep("?", unique(nchar(itemIds))), collapse = "")
  } 

  cat("ISELECT = ", itSelect, "\n",    sep = "", file = commandFile, append = TRUE)
  cat("ITEM1 =", item1, "\n",        sep = " ", file = commandFile, append = TRUE)
  cat("ITLEN =", itLength, "\n",     sep = " ", file = commandFile, append = TRUE)
  
  if (!is.null(keysItems)){
    keyFin <- paste(keysItems, collapse = "")
    cat("KEY1 =", keyFin, "\n",     sep = " ", file = commandFile, append = TRUE)
  }
    
  cat("NAME1 = 1\n",                 sep = " ", file = commandFile, append = TRUE)
  cat("NAMLEN =", maxLengthId + groupWidth, "\n",
                                     sep = " ", file = commandFile, append = TRUE)
  cat("NI = ", nItems, "\n",         sep = " ", file = commandFile, append = TRUE)  
  
  if (is.null(psSelect)) {
    psSelect <- paste(rep("?", unique(nchar(personIds))), collapse = "")
  } 
  cat("PSELECT = ", psSelect, "\n",     sep = " ", file = commandFile, append = TRUE)
  cat("QUOTED = ", quotes, "\n",     sep = " ", file = commandFile, append = TRUE)
  
  # # Files section
  
  if (!is.null(weights)) {
    cat("PWEIGHT = ", weightsFile, "\n",
                                      sep = "", file = commandFile, append = TRUE)

    write.table(as.matrix(weights, ncol = 1), file = weightsFile, append = FALSE, sep = " ",
      row.names = TRUE, col.names = FALSE, na = " ", quote = FALSE)
  }
  cat("*\n*", file = idFile)
  cat("IDFILE = ", idFileNanem, "\n",      sep = "", file = commandFile, append = TRUE)
  cat("IFILE = ", iFile, "\n",      sep = "", file = commandFile, append = TRUE)
  cat("DISFILE = ", disFile, "\n",  sep = "", file = commandFile, append = TRUE)
  cat("SFILE = ", sFile, "\n",      sep = "", file = commandFile, append = TRUE)
  if (score) {
    cat("PFILE = ", pFile, "\n",    sep = "", file = commandFile, append = TRUE)
  }
  cat("SCOREFILE = ", scoreFile, "\n", sep = "", file = commandFile, append = TRUE)                                   
  # # JMLE bias correction of parameter estimates
  if (stBias) {
    cat("STBIAS = Y\n",              sep = " ", file = commandFile, append = TRUE)
  }

  # # Run dif procedure
  if (dif) {
    cat("DIF = $S", maxLengthId + 1, "W", groupWidth, "\n",
                                      sep = "", file = commandFile, append = TRUE)
  }
    
  # # Constraining diifficulty parameters
  if (!is.null(itemConstraints)) {
    cat("IAFILE = *\n",              sep = " ", file = commandFile, append = TRUE)
    nConstraints <- nrow(itemConstraints)
    for (ii in 1:nConstraints) {
      cat(itemConstraints[ii, 1], itemConstraints[ii, 2], "\n",
                                     sep = " ", file = commandFile, append = TRUE)
    }
    cat("*\n",                                  file = commandFile, append = TRUE)
    cat("\n\n",                                   file = commandFile, append = TRUE)
  }

  # # Constraining abilities
  if (!is.null(personConstraints)) {
    cat("PAFILE = *\n",              sep = " ", file = commandFile, append = TRUE)
    nConstraints <- nrow(personConstraints)
    for (ii in 1:nConstraints) {
      cat(personConstraints[ii, 1], personConstraints[ii, 2], "\n",
                                     sep = " ", file = commandFile, append = TRUE)
    }
    cat("*\n",                                  file = commandFile, append = TRUE)
  }

  # # Item names
  cat("&END\n", sep = " ", file = commandFile, append = TRUE)
  if (!is.null(itemIds)) {
    maxLengthId <- max(nchar(itemIds))
    if (is.numeric(itemIds)) {
      typeId <- "d" # # If numeric then add zeroes before the shorter ones
    } else {
      typeId <- "s" # # If not numeric then add spaces before the shortest ones
    }
    itemIds   <- sprintf(paste("%0", maxLengthId, typeId, sep = ""), itemIds)
  } else {
    itemIds     <- 1:nItems
    maxLengthId <- max(nchar(itemIds))
    typeId      <- "d"
    itemIds     <- paste("it", sprintf(paste("\"%0", maxLengthId, typeId, "\"", sep = ""), itemIds), sep = "")
    itemIds     <- gsub("\"", "", itemIds)
  }

  for (ii in 1:nItems) {
    cat(itemIds[ii], "\n", sep = "", file = commandFile, append = TRUE)
  }
  cat("END NAMES\n", sep = " ", file = commandFile, append = TRUE)
  if (verbose) cat("Control file prepared\n")

  # # Running program
  if (runProgram){
     if (verbose) {
       cat("Running program\n")
     }
     if (Sys.info()["sysname"] == "Linux") {
       if (verbose) {
         system(paste("tail -n 1 -f", logFile), wait = FALSE)
       }
       system( paste("wine", "~/.wine/drive_c/WINSTEPS/Winsteps.exe BATCH=YES", commandFile, logFile), wait = TRUE)
     } else {
       system(paste("C:\\\\WINSTEPS\\Winsteps.exe BATCH=YES", commandFile, logFile), wait = TRUE)
     }
     if (verbose) {
       cat("\n")
     }
     if (Sys.info()["sysname"] == "Linux") {
       if (verbose) {
         system("killall tail")
       }
     }
   
     # # Reading item parameters
     if (!(sep %in% c("\t", ",")) ) {
       sep = NULL
     }
     iPar <- ReadIfile(iFile, filePath = '', sep = sep)
   
     return(iPar)
   }
}
