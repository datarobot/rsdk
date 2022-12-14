% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predictions_apiWrapper.R,
%   R/PredictionDatasets.R
\name{UploadPredictionDatasetFromDataSource}
\alias{UploadPredictionDatasetFromDataSource}
\title{UploadPredictionDatasetFromDataSource}
\usage{
UploadPredictionDatasetFromDataSource(
  project,
  dataSourceId,
  username,
  password,
  forecastPoint = NULL,
  maxWait = 600,
  relaxKIAFeaturesCheck = NULL
)

UploadPredictionDatasetFromDataSource(
  project,
  dataSourceId,
  username,
  password,
  forecastPoint = NULL,
  maxWait = 600,
  relaxKIAFeaturesCheck = NULL
)
}
\arguments{
\item{project}{character. Either (1) a character string giving the unique alphanumeric
identifier for the project, or (2) a list containing the element projectId with this
identifier.}

\item{dataSourceId}{character. The id of the data source}

\item{username}{character. The username to use for authentication to the database.}

\item{password}{character. The password to use for authentication to the database.
The password is encrypted at server side and never saved or stored.}

\item{forecastPoint}{character. Optional. The point relative to which predictions will be
generated, based on the forecast window of the project. Only specified in time series
projects.}

\item{maxWait}{integer. The maximum time (in seconds) to wait for each of two steps:
(1) The initial dataset upload request, and
(2) data processing that occurs after receiving the response to this initial request.}

\item{relaxKIAFeaturesCheck}{logical. For time series projects only. If True, missing values
in the known in advance features are allowed in the forecast window at the prediction time.
If omitted or FALSE, missing values are not allowed.}

\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Upload a dataset for predictions from a \code{DataSource}.
}
\details{
Upload a dataset for predictions from a \code{DataSource}.

This method invokes \verb{POST /projects/\{projectId\}/predictionDatasets/dataSourceUploads/} in the DataRobot Public API.
}
\examples{
\dontrun{
dataSourceId <- "5c1303269300d900016b41a7"
TestDataStore(dataSourceId, username = "myUser", password = "mySecurePass129")
}
}
\seealso{
The method ProjectsPredictionDatasetsDataSourceUploadsCreate in \link[datarobot.apicore:PredictionsApi]{datarobot.apicore::PredictionsApi}, which invokes the same underlying API endpoint.
}
