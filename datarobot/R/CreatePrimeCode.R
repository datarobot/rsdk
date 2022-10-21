#' Create and validate the downloadable code for the ruleset associated with this model
#'
#' @inheritParams DeleteProject
#' @param primeModelId character. Id returned by GetPrimeModel(s) functions.
#' @param language character. Programming language to use for downloadable code (see PrimeLanguage).
#' @return job Id
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   CreatePrimeCode(projectId, modelId, "Python")
#' }
#' @export
CreatePrimeCode <- function(project, primeModelId, language) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "primeFiles")
  body <- list(modelId = primeModelId, language = language)
  postResponse <- DataRobotPOST(routeString, body =  body, returnRawResponse = TRUE)
  routeString <- UrlJoin("projects", projectId, "jobs", JobIdFromResponse(postResponse))
  jobsResponse <- DataRobotGET(routeString, simplifyDataFrame = FALSE)
  jobsResponse$id
}
