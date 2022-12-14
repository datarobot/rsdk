% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{DownloadRatingTable}
\alias{DownloadRatingTable}
\title{DownloadRatingTable}
\usage{
DownloadRatingTable(project, ratingTableId, filename, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve Rating Table File
}
\details{
Retrieve the CSV file for the rating table

This method invokes \verb{GET /projects/\{projectId\}/ratingTables/\{ratingTableId\}/file/} in the DataRobot Public API.
}
\seealso{
The method ProjectsRatingTablesFileList in \link{ModelsApi}, which invokes the same underlying API endpoint.
}
