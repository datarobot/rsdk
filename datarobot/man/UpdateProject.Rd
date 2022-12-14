% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R, R/Projects.R
\name{UpdateProject}
\alias{UpdateProject}
\title{UpdateProject}
\usage{
UpdateProject(
  project,
  newProjectName = NULL,
  workerCount = NULL,
  holdoutUnlocked = NULL,
  ...
)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{newProjectName}{character. Updated value for the projectName parameter
associated with the project.}

\item{workerCount}{integer. The number of workers to run (default 2). Use \code{"max"} to set
to the maximum number of workers available.}

\item{holdoutUnlocked}{logical. Either NULL (default) or TRUE. If TRUE, this function
requests the DataRobot Autopilot to unlock the holdout data subset.}

\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Update project
}
\details{
Change project name, worker count, or unlock the holdout.   If any of the optional json arguments are not provided, that aspect of the project will not be altered.

This method invokes \verb{PATCH /projects/\{projectId\}/} in the DataRobot Public API.

Update parameters for an existing project

This function updates parameters for the project defined by project.
}
\examples{
\dontrun{
projectId <- "59a5af20c80891534e3c2bde"
UpdateProject(projectId, newProjectName = "cooler Project")
UpdateProject(projectId, workerCount = 20)
UpdateProject(projectId, holdoutUnlocked = TRUE)
}
}
\seealso{
The method Patch in \link[datarobot.apicore:ProjectsApi]{datarobot.apicore::ProjectsApi}, which invokes the same underlying API endpoint.
}
