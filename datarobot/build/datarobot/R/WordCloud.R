#' Retrieve word cloud data for a model.
#'
#' @inheritParams DeleteProject
#' @param modelId character. Unique alphanumeric identifier for the model of
#'   interest.
#' @param excludeStopWords logical. Optional. Set to TRUE if you want stopwords
#'   filtered out the response.
#' @return data.frame with the following components:
#' \describe{
#'   \item{ngram}{character. word or ngram value}
#'   \item{coefficient}{numeric. value from [-1.0, 1.0] range, describes effect
#'     of this ngram on the target. A large negative value means a strong effect
#'     toward the negative class in classification projects and a smaller
#'     predicted target value in regression projects. A large positive value
#'     means a strong effect toward the positive class and a larger predicted
#'     target value respectively}
#'   \item{frequency}{numeric. value from (0.0, 1.0] range, frequency of this
#'     ngram relative to the most frequent ngram}
#'   \item{count}{integer. number of rows in the training sample where this
#'     ngram appears}
#'   \item{isStopword}{logical. true for ngrams that DataRobot evaluates as
#'     stopwords}
#'   \item{variable}{character. Optional. Added in DataRobot API 2.19. String
#'     representation of the ngram source. Contains the column name and, for
#'     some models, preprocessing details. For example, `NGRAM_OCCUR_L2_cname`
#'     represents the ngram occurrences count using L2 normalization from the
#'     cname column}
#'   \item{class}{character. Optional. Added in DataRobot API 2.19. Values of
#'     the target class for the corresponding word or ngram. For regression, NA}
#' }
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   modelId <- "5996f820af07fc605e81ead4"
#'   GetWordCloud(projectId, modelId)
#' }
#' @export
GetWordCloud <- function(project, modelId, excludeStopWords = FALSE) {
  projectId <- ValidateProject(project)
  routeString <- UrlJoin("projects", projectId, "models", modelId, "wordCloud")
  query <- list("excludeStopWords" = tolower(as.character(excludeStopWords)))
  responseData <- DataRobotGET(routeString,
                               query = query)
  as.dataRobotWordCloud(responseData$ngrams)
}

as.dataRobotWordCloud <- function(indf) {
  # clean up DF, then sort
  outdf <- reorderColumns(indf, c("ngram" = 1, "frequency" = 2, "coefficient" = 3))
  outdf[with(outdf, order(outdf$frequency, decreasing = TRUE)), ]
}
