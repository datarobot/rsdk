#' Download the code of DataRobot Prime model and save it to a file.
#'
#' Training a model using a ruleset is a necessary prerequisite for being able to download the
#'   code for a ruleset.
#'
#' @inheritParams DeleteProject
#' @param primeFileId numeric. Prime file Id (can be acquired using ListPrimeFiles function)
#' @param filepath character. The location to save the file to.
#' @return NULL
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   primeFiles <- ListPrimeFiles(projectId)
#'   primeFile <- primeFiles[[1]]
#'   primeFileId <- primeFile$id
#'   file <- file.path(tempdir(), "primeCode.py")
#'   DownloadPrimeCode(projectId, primeFileId, file)
#' }
#' @export
DownloadPrimeCode <- function(project, primeFileId, filepath) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "primeFiles", primeFileId, "download")
  response <- DataRobotGET(routeString, as = "file", filename = filepath)
  invisible(NULL)
}
