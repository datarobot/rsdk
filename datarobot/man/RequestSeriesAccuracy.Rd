% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeriesAccuracy.R
\name{RequestSeriesAccuracy}
\alias{RequestSeriesAccuracy}
\title{Compute the series accuracy for a model.}
\usage{
RequestSeriesAccuracy(model)
}
\arguments{
\item{model}{character. The model for which you want to compute Feature Impact, e.g.
from the list of models returned by \code{ListModels(project)}.}
}
\value{
Job ID for the async job associated with the computation.
}
\description{
Note that you can call \code{GetSeriesAccuracy} without calling this function, and the
series accuracy will be requested automatically.
}
\examples{
\dontrun{
projectId <- "5984b4d7100d2b31c1166529"
modelId <- "5984b4d7100d2b31c1166529"
model <- GetModel(projectId, modelId)
jobId <- RequestSeriesAccuracy(projectId, modelId)
WaitForJobToComplete(projectId, jobId)
}
}
