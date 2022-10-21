#' Pareto Front data for a Eureqa model
#'
#'  The Eureqa algorithm generates millions and millions of equations.
#'  Eureqa takes the best bits from the best initial models and splices them randomly
#'  into the next generation. After enough mixing, the models can achieve good accuracy.
#'  There are usually many equations at every complexity level, but they aren't exposed.
#'  The models that are displayed are the "Pareto-optimal" models.
#'  That means that for any given complexity score, it shows the model with the best
#'  error metric on the training data out of all the modes. After that,
#'  for each remaining model, if there a strictly better model, throw out the
#'  strictly-worse model. A Pareto Front are those "Pareto-optimal" models that
#'  are generated at various complexity scores.
#'
#' @param model An S3 object of class dataRobotModel like that returned by the function
#'   GetModel, or each element of the list returned by the function ListModels.
#'
#' @return data.frame with the following components:
#' \itemize{
#'   \item projectId character. the id of the project the model belongs to
#'   \item errorMetric character. Eureqa error-metric identifier used
#'      to compute error metrics for this search.
#'      Note that Eureqa error metrics do NOT correspond 1:1 with DataRobot
#'      error metrics -- the available metrics are not the same, and even
#'      equivalent metrics may be computed slightly differently.
#'   \item hyperparameters list. A list of the various hyperparameters that could be used.
#'      By default there are none.
#'   \item targetType character. Indicating what kind of modeling is being done in this project
#'      Options are: "Regression", "Binary" (Binary classification),
#'                   "Multiclass" (Multiclass classification)
#'   \item solutions list. List of Pareto points.
#'      Every Pareto point contains a dictionary with keys:
#'     \itemize{
#'       \item eureqaSolutionId character. ID of this solution
#'       \item complexity numeric. Complexity score for this solution.
#'          Complexity score is a function of the mathematical operators
#'          used in the current solution.
#'          The Complexity calculation can be tuned via model hyperparameters.
#'       \item error numeric. Error for the current solution,
#'          as computed by Eureqa using the "error_metric" error metric.
#'       \item expression character. String specifying the Eureqa model equation.
#'       \item expression_annotated character. Eureqa model equation string with
#'          variable names tagged for easy identification.
#'     }
#' }
#'
#' @examples
#' \dontrun{
#'   projectId <- "5b2827556523cd05bd1507a5"
#'   modelId <- "5b29406c6523cd0665685a8d"
#'   model <- GetModel(projectId, modelId)
#'   GetParetoFront(model)
#' }
#' @export
GetParetoFront <- function(model) {
  validModel <- ValidateModel(model)
  projectId <- validModel$projectId
  modelId <- validModel$modelId
  routeString <- UrlJoin("projects", projectId, "eureqaModels", modelId)
  response <- DataRobotGET(routeString, simplify = FALSE)
  as.dataRobotParetoFront(response)
}

as.dataRobotParetoFront <- function(inList) {
  outList <- as.list(inList)
  outList <- ApplySchema(outList, c("projectId",
                                    "errorMetric",
                                    "hyperparameters",
                                    "targetType",
                                    "solutions"))
  outList$hyperparameters <- as.list(outList$hyperparameters)
  outList$solutions <- as.list(outList$solutions)
  outList$solutions <- lapply(outList$solutions, ApplySchema, schema = c("eureqaSolutionId",
                                                                         "complexity",
                                                                         "error",
                                                                         "expression",
                                                                         "expressionAnnotated",
                                                                         "bestModel"))
  outList
}


#' Add a Eureqa solution to the list of models for the project.
#'
#' Each Eureqa model contains multiple possible solutions (see \code{GetParetoFront}).
#' However, only the best model is included in the leaderboard by default. To include
#' other models, you can get them via \code{GetParetoFront} and then add them.
#'
#' @inheritParams DeleteProject
#' @param eureqaSolutionId character. The solution ID of the Eureqa model to add.
#' @examples
#' \dontrun{
#'   projectId <- "5b2827556523cd05bd1507a5"
#'   modelId <- "5b29406c6523cd0665685a8d"
#'   eureqaModel <- GetModel(projectId, modelId)
#'   paretoFront <- GetParetoFront(eureqaModel)
#' }
#' @export
AddEureqaSolution <- function(project, eureqaSolutionId) {
  project <- ValidateProject(project)
  routeString <- UrlJoin("projects", project, "eureqaModels")
  body <- list(solutionId = eureqaSolutionId)
  DataRobotPOST(routeString, body = body)
  message("Solution added")
  invisible(NULL)
}
