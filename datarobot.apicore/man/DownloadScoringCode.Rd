% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{DownloadScoringCode}
\alias{DownloadScoringCode}
\title{DownloadScoringCode}
\usage{
DownloadScoringCode(project, modelId, fileName, sourceCode = FALSE, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve Scoring Code
}
\details{
Retrieve Scoring Code for making new predictions from an existing model offline. You need the \"Scoring Code\" feature enabled to use this route.  By default, returns a compiled executable JAR that can be executed locally to calculate model predictions, or it can be used as a library for a Java application. Execute it with the '--help\\` parameters to learn how to use it as a command-line utility. See model API documentation (\url{https://javadoc.io/doc/com.datarobot/datarobot-prediction/latest/index.html}) to be able to use it inside an existing Java application.  With the sourceCode query parameter set to 'true', returns a source code archive that can be used to review internal calculations of the model. This JAR is NOT executable.  See \"\url{https://app.datarobot.com/docs/predictions/scoring-code/scorecode.html}\" in DataRobot application for more information.

This method invokes \verb{GET /projects/\{projectId\}/models/\{modelId\}/scoringCode/} in the DataRobot Public API.
}
\seealso{
The method ProjectsModelsScoringCodeList in \link{ModelsApi}, which invokes the same underlying API endpoint.
}
