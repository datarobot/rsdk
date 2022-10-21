#' Upload a data source.
#'
#' Takes either a file path or a dataframe and returns output for POST that specifies
#' the file object via form upload. This function is meant to facilitate uploading
#' CSV data sources into DataRobot, such as through \code{SetupProject}.
#'
#' @seealso SetupProject
#' @param dataSource character. The file to upload.
#' @return An httr object specifying the form upload content of the file path.
UploadData <- function(dataSource) {
  httr::upload_file(DataPathFromDataArg(dataSource))
}

#' Get the data path.
#'
#' Verifies that new data is either an existing datafile or a dataframe
#' If a dataframe, save as a CSV file
#' If neither an existing datafile nor a dataframe, halt with error
#' @param dataSource object. The dataframe or path to CSV to get data for.
#' @param saveFile character. Optional. A file name to write an autosaved dataframe to.
DataPathFromDataArg <- function(dataSource, saveFile = NULL) {
  # Can remove last two arguments after 2.3
  if (is.character(dataSource)) {
    if (file.exists(dataSource)) {
      dataPath <- dataSource
    } else {
      errorMsg <- paste("No file named", dataSource,
                        "exists in the working directory", getwd())
      stop(errorMsg)
    }
  } else {
    if (is.data.frame(dataSource)) {
      #
      #  If dataSource is a dataframe, save as a CSV file
      #
      if (is.null(saveFile)) {
        dataPath <- tempfile(fileext = "_autoSavedDF.csv")
      } else {
        dataPath <- saveFile
      }
      write.csv(dataSource, dataPath, row.names = FALSE)
    } else {
      errorMsg <- paste(deparse(substitute(dataSource)),
                        "is not a valid data file name or dataframe")
      stop(strwrap(errorMsg))
    }
  }
  return(dataPath)
}
