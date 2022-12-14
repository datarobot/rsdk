% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/insights_apiWrapper.R
\name{ListProjectsDatetimeModelsFeatureFit}
\alias{ListProjectsDatetimeModelsFeatureFit}
\title{ListProjectsDatetimeModelsFeatureFit}
\usage{
ListProjectsDatetimeModelsFeatureFit(
  backtestIndex,
  modelId,
  projectId,
  includeIcePlots,
  source = "training",
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve Feature Fit for a model backtest.
}
\details{
NOTE: Starting from API v2.29 this endpoint will be deprecated. Retrieve Feature Fit for a model backtest. Feature Fit provides partial dependence and predicted vs actual values for the top 500 features, ordered by feature importance score. The partial dependence shows marginal effect of a feature on the target variable after accounting for the average effects of all other predictive features. It indicates how, holding all other variables except the feature of interest as they were, the value of this feature affects your prediction. If a Feature Fit job was previously submitted for a given backtest, this endpoint will return a response structured as {\"message\": \\<message\\>, \"jobId\": \\<jobId\\>} where jobId is the ID of the job. Retrieve the job with :\url{http:get}:/api/v2/projects/(projectId)/jobs/(jobId)/

This method invokes \verb{GET /projects/\{projectId\}/datetimeModels/\{modelId\}/featureFit/} in the DataRobot Public API.
}
\seealso{
The method ProjectsDatetimeModelsFeatureFitList in \link{InsightsApi}, which invokes the same underlying API endpoint.
}
