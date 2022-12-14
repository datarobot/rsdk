% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R
\name{RequestCrossSeriesDetection}
\alias{RequestCrossSeriesDetection}
\title{RequestCrossSeriesDetection}
\usage{
RequestCrossSeriesDetection(
  project,
  dateColumn,
  multiseriesIdColumns = NULL,
  crossSeriesGroupByColumns = NULL,
  maxWait = 600,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Validate columns for potential use as the group-by column for cross-series functionality.
}
\details{
Validate columns for potential use as the group-by column for cross-series functionality.  The group-by column is an optional setting that indicates how to further splitseries into related groups. For example, if each series represents sales of an individual product, the group-by column could be the product category, e.g., \"clothing\" or \"sports equipment\".

This method invokes \verb{POST /projects/\{projectId\}/crossSeriesProperties/} in the DataRobot Public API.
}
\seealso{
The method CrossSeriesPropertiesCreate in \link{ProjectsApi}, which invokes the same underlying API endpoint.
}
