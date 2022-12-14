% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R
\name{CreateProjectCleanupJobs}
\alias{CreateProjectCleanupJobs}
\title{CreateProjectCleanupJobs}
\usage{
CreateProjectCleanupJobs(
  deletedAfter,
  creator,
  organization,
  deletedBefore,
  projectIds,
  searchFor,
  offset = 0,
  limit = 1000,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Schedule Project Permadelete Job
}
\details{
Add list of projects to permadelete and returns async status

This method invokes \verb{POST /projectCleanupJobs/} in the DataRobot Public API.
}
\seealso{
The method CleanupJobsCreate in \link{ProjectsApi}, which invokes the same underlying API endpoint.
}
