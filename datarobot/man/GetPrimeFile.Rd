% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models_apiWrapper.R, R/PrimeFiles.R
\name{GetPrimeFile}
\alias{GetPrimeFile}
\title{GetPrimeFile}
\usage{
GetPrimeFile(project, primeFileId, ...)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{primeFileId}{numeric. Unique alphanumeric identifier for the primeFile
to be retrieved.}

\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\value{
List with following elements:
\describe{
  \item{language}{Character string. Code programming language}
  \item{isValid}{logical flag indicating if code passed validation}
  \item{rulesetId}{Integer identifier for the ruleset}
  \item{parentModelId}{Unique alphanumeric identifier for the parent model}
  \item{projectId}{Unique alphanumeric identifier for the project}
  \item{id}{Unique alphanumeric identifier for the Prime file}
  \item{modelId}{Unique alphanumeric identifier for the model}
}
}
\description{
Retrieve metadata about a DataRobot Prime file
}
\details{
Retrieve metadata about a DataRobot Prime file available for download

This method invokes \verb{GET /projects/\{projectId\}/primeFiles/\{primeFileId\}/} in the DataRobot Public API.

Retrieve a specific Prime file from a DataRobot project

This function returns information about specified Prime file from a specified project.
}
\examples{
\dontrun{
projectId <- "59a5af20c80891534e3c2bde"
primeFiles <- ListPrimeFiles(projectId)
primeFile <- primeFiles[[1]]
primeFileId <- primeFile$id
GetPrimeFile(projectId, primeFileId)
}
}
\seealso{
The method ProjectsPrimeFilesRetrieve in \link[datarobot.apicore:ModelsApi]{datarobot.apicore::ModelsApi}, which invokes the same underlying API endpoint.
}
