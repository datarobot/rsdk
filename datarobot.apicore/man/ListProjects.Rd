% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R
\name{ListProjects}
\alias{ListProjects}
\title{ListProjects}
\usage{
ListProjects(
  filter = NULL,
  projectName = NULL,
  projectId = NULL,
  orderBy = NULL,
  featureDiscovery = NULL,
  ...
)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
List projects
}
\details{
List all available projects.

This method invokes \verb{GET /projects/} in the DataRobot Public API.
}
\seealso{
The method List in \link{ProjectsApi}, which invokes the same underlying API endpoint.
}
