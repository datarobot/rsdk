% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ViewWebModel.R
\name{ViewWebModel}
\alias{ViewWebModel}
\title{Retrieve a DataRobot web page that displays detailed model information}
\usage{
ViewWebModel(model)
}
\arguments{
\item{model}{An S3 object of class dataRobotModel like that returned by
the function GetModel, or each element of the list returned by
the function ListModels.}
}
\description{
This function brings up a web page that displays detailed model
information like that available from the standard DataRobot user
interface (e.g., graphical representations of model structures).
}
\examples{
\dontrun{
projectId <- "59a5af20c80891534e3c2bde"
modelId <- "5996f820af07fc605e81ead4"
model <- GetModel(projectId, modelId)
ViewWebModel(model)
}
}
