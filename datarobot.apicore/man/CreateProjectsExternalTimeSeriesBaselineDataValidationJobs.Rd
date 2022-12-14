% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R
\name{CreateProjectsExternalTimeSeriesBaselineDataValidationJobs}
\alias{CreateProjectsExternalTimeSeriesBaselineDataValidationJobs}
\title{CreateProjectsExternalTimeSeriesBaselineDataValidationJobs}
\usage{
CreateProjectsExternalTimeSeriesBaselineDataValidationJobs(
  forecastWindowStart,
  forecastWindowEnd,
  catalogVersionId,
  holdoutEndDate,
  multiseriesIdColumns,
  holdoutStartDate,
  backtests,
  projectId,
  target,
  datetimePartitionColumn,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Validate baseline data
}
\details{
This route validates if a provided catalog version id can be used as baseline for calculating metrics. This functionality is available only for time series projects.For a baseline dataset to be valid, the number of unique date amd multiseries_id columnrows must match the unique number of date and multiseries_id column rows in the uploadedtraining dataset. This functionality is limited to one forecast distance. Additionally, the catalog must be a snapshot.

This method invokes \verb{POST /projects/\{projectId\}/externalTimeSeriesBaselineDataValidationJobs/} in the DataRobot Public API.
}
\seealso{
The method ExternalTimeSeriesBaselineDataValidationJobsCreate in \link{ProjectsApi}, which invokes the same underlying API endpoint.
}
