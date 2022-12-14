% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SeriesAccuracy.R
\name{GetSeriesAccuracyForModel}
\alias{GetSeriesAccuracyForModel}
\title{Get the series accuracy associated with a particular model.}
\usage{
GetSeriesAccuracyForModel(model)
}
\arguments{
\item{model}{character. The model for which you want to compute Feature Impact, e.g.
from the list of models returned by \code{ListModels(project)}.}
}
\value{
data.frame with items:
  \itemize{
    \item multiseriesId character. The ID of the series.
    \item rowCount integer. The number of rows in the series.
    \item multiseriesValues character. The name of the series.
    \item duration character. The duration of the series.
    \item validationScore numeric. The validation score for the series.
    \item backtestingScore numeric. The score on backtests for the series. See
      \code{ScoreBacktests}.
    \item holdoutScore numeric. The score for the series on the holdout set.
  }
}
\description{
This will not work if you have not separately computed series accuracy via
\code{RequestSeriesAccuracy}. See \code{GetSeriesAccuracy} for a function that
will get series accuracy and also compute it automatically if it has not already been compute.
}
\examples{
\dontrun{
projectId <- "5984b4d7100d2b31c1166529"
modelId <- "5984b4d7100d2b31c1166529"
model <- GetModel(projectId, modelId)
jobId <- RequestSeriesAccuracy(projectId, modelId)
WaitForJobToComplete(projectId, jobId)
seriesAccuracy <- GetSeriesAccuracyForModel(model)
}
}
