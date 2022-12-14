% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SetupProject.R
\name{as.dataRobotProjectShort}
\alias{as.dataRobotProjectShort}
\title{Return value for SetupProject() and others}
\usage{
as.dataRobotProjectShort(inList)
}
\arguments{
\item{inList}{list. See return value below for expected elements.}
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
Return value for SetupProject() and others
}
