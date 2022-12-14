% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projects_apiWrapper.R, R/CloneProject.R
\name{CloneProject}
\alias{CloneProject}
\title{CloneProject}
\usage{
CloneProject(project, newProjectName = NULL, maxWait = 600, ...)
}
\arguments{
\item{project}{dataRobotProject, or a character representing that project's ID.}

\item{newProjectName}{character. The name of the newly cloned project. If no
name is given, the API will default to 'Copy of \code{project$projectName}'.}

\item{maxWait}{integer. The maximum time to wait for each of two steps: (1) The initial
project creation request, and (2) data processing that occurs after receiving the response
to this initial request.}

\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\value{
A named list that contains:
\describe{
  \item{projectName}{character. The name assigned to the DataRobot project}
  \item{projectId}{character. The unique alphanumeric project identifier for this DataRobot
    project}
  \item{fileName}{character. The name of the CSV modeling file uploaded for this project}
  \item{created}{character. The time and date of project creation}
}
}
\description{
Clone a project
}
\details{
Create a clone of an existing project. > > The resultant project will begin the initial exploratory data analysis > and will be ready to set the target of the new project shortly.

This method invokes \verb{POST /projectClones/} in the DataRobot Public API.

Clone a project

This function clones a project, creating a fresh (postEDA1) copy that will
need a target and modeling options set.
}
\examples{
\dontrun{
project < GetProject("5c1303269300d900016b41a7")
CloneProject(project, newProjectName = "Project Restart")
}
}
\seealso{
The method ClonesCreate in \link[datarobot.apicore:ProjectsApi]{datarobot.apicore::ProjectsApi}, which invokes the same underlying API endpoint.
}
