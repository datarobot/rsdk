% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R
\name{ListModelJobs}
\alias{ListModelJobs}
\title{ListModelJobs}
\usage{
ListModelJobs(project, status = NULL, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
List modeling jobs
}
\details{
List modeling jobs.

This method invokes \verb{GET /projects/\{projectId\}/modelJobs/} in the DataRobot Public API.
}
\seealso{
The method ProjectsModelJobsList in \link{ModelsApi}, which invokes the same underlying API endpoint.
}
