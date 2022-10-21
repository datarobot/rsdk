% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PredictionDatasets.R
\name{ListPredictionDatasets}
\alias{ListPredictionDatasets}
\title{Retrieve all prediction datasets associated with a project}
\usage{
ListPredictionDatasets(project)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}
}
\value{
An S3 object of class 'listDataRobotPredictionDataset', which is a
list of dataframes: each element of the list corresponds to one
prediction dataset associated with the project, and each dataframe has
one row and the following columns:
\itemize{
  \item id character. The unique alphanumeric identifier for the dataset.
  \item numColumns numeric. Number of columns in dataset.
  \item name character. Name of dataset file.
  \item created character. time of upload.
  \item projectId character. String giving the unique alphanumeric identifier for the project.
  \item numRows numeric. Number of rows in dataset.
  \item forecastPoint. The point relative to which predictions will be generated, based on the
    forecast window of the project. Only specified in time series projects, otherwise
    will be NULL.
}
}
\description{
This function returns an S3 object of class listDataRobotPredictionDataset that
describes all prediction datasets
available for the project specified by the project parameter.
This list may be converted to a dataframe with the as.data.frame
method for objects of class listDataRobotPredictionDataset.
}
\examples{
\dontrun{
  projectId <- "59a5af20c80891534e3c2bde"
  ListPredictionDatasets(projectId)
}
}