#' Request training for a DataRobot Prime model using a specified ruleset
#'
#' Training a model using a ruleset is a necessary prerequisite for being able to download the code
#' for a ruleset.
#'
#' @inheritParams DeleteProject
#' @param ruleset list. A list specifying ruleset parameters (see GetRulesets)
#' @return job Id
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   rulesets <- GetRulesets(projectId, modelId)
#'   ruleset <- rulesets[[1]]
#'   RequestPrimeModel(projectId, ruleset)
#' }
#' @export
RequestPrimeModel <- function(project, ruleset) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "primeModels")
  parentModelId <- ruleset$parentModelId
  rulesetId <- ruleset$rulesetId
  bodyFrame <- data.frame(parentModelId = parentModelId, rulesetId = rulesetId)
  body <- jsonlite::unbox(bodyFrame)
  postResponse <- DataRobotPOST(routeString, body = body,
                               returnRawResponse = TRUE, encode = "json")
  routeString <- UrlJoin("projects", projectId, "jobs", JobIdFromResponse(postResponse))
  jobsResponse <- DataRobotGET(routeString, simplifyDataFrame = FALSE)
  jobsResponse$id
}
