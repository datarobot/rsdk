% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R
\name{ListDeletedProjects}
\alias{ListDeletedProjects}
\title{ListDeletedProjects}
\usage{
ListDeletedProjects(
  deletedAfter,
  creator,
  organization,
  deletedBefore,
  projectId,
  searchFor,
  offset = 0,
  limit = 20,
  orderBy = "deletedOn",
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve a list of soft-deleted projects
}
\details{
Retrieve a list of soft-deleted projects matching search criteria

This method invokes \verb{GET /deletedProjects/} in the DataRobot Public API.
}
\seealso{
The method DeletedProjectsList in \link[datarobot.apicore:ProjectsApi]{datarobot.apicore::ProjectsApi}, which invokes the same underlying API endpoint.
}
