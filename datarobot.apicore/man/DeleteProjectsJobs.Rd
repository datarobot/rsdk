% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R
\name{DeleteProjectsJobs}
\alias{DeleteProjectsJobs}
\title{DeleteProjectsJobs}
\usage{
DeleteProjectsJobs(jobId, projectId, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Cancel Job
}
\details{
Cancel a pending job.

This method invokes \verb{DELETE /projects/\{projectId\}/jobs/\{jobId\}/} in the DataRobot Public API.
}
\seealso{
The method JobsDelete in \link{ProjectsApi}, which invokes the same underlying API endpoint.
}
