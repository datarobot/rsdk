% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{GetRulesets}
\alias{GetRulesets}
\title{GetRulesets}
\usage{
GetRulesets(project, modelId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
List Rulesets
}
\details{
List all the rulesets approximating a model  When rulesets are created for the parent model, all of the rulesets are created at once, but not all rulesets have corresponding Prime models (until they are directly requested).

This method invokes \verb{GET /projects/\{projectId\}/models/\{modelId\}/primeRulesets/} in the DataRobot Public API.
}
\seealso{
The method ProjectsModelsPrimeRulesetsList in \link{ModelsApi}, which invokes the same underlying API endpoint.
}
