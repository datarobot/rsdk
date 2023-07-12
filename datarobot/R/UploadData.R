#' Upload a data source.
#'
#' Takes either a file path or a dataframe and returns output for POST that specifies
#' the file object via form upload. This function is meant to facilitate uploading
#' CSV data sources into DataRobot, such as through \code{SetupProject}.
#'
#' @seealso SetupProject
#' @param dataSource character. The file to upload.
#' @param fileName character. The name of the file after it is uploaded. If not set, defaults
#'  to the name of the uploaded file.
#' @return An httr object specifying the form upload content of the file path.
UploadData <- function(dataSource, fileName = NULL) {
  if (!is.null(fileName)) {
    stopifnot(is.character(fileName))
  }
  dataPath <- DataPathFromDataArg(dataSource)
  content <- httr::upload_file(dataPath)
  if ("name" %in% names(content) && is.null(content$name)) {
    # DSX-2862
    # httr::upload_file invokes curl::form_file.  In curl<5.0.1, this function
    # returns a length-2 list containing $path and $type. In curl==5.0.1, thanks to
    # https://github.com/jeroen/curl/pull/290 this function returns an extra $name.
    # To ensure this works properly with `datarobot:::TryingToBeNull()`, set $name
    # to the path.
    content$name <- ifelse(is.null(fileName), yes = basename(dataPath), no = fileName)
  }
  invisible(content)
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
