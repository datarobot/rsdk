% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datetime_partitioning_apiWrapper.R
\name{GetTimeSeriesFeatureDerivationLog}
\alias{GetTimeSeriesFeatureDerivationLog}
\title{GetTimeSeriesFeatureDerivationLog}
\usage{
GetTimeSeriesFeatureDerivationLog(project, offset = NULL, limit = NULL, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve the feature derivation log content and log length for a time series project as JSON.
}
\details{
Retrieve the feature derivation log content and log length for a time series project as JSON.  The Time Series Feature Log provides details about the feature generation process for a time series project. It includes information about which features are generated and their priority,as well as the detected properties of the time series data such as whether the series is stationary, and periodicities detected.  This route is only supported for time series projects that have finished partitioning.  The feature derivation log will include information about:  - Detected stationarity of the series, e.g.,   \verb{Series detected as non-stationary} - Detected presence of multiplicative trend in the series, e.g.,   \verb{Multiplicative trend detected} - Detected periodicities in the series, e.g.,   \verb{Detected periodicities: 7 day} - Maximum number of feature to be generated, e.g.,   \verb{Maximum number of feature to be generated is 1440} - Window sizes used in rolling statistics / lag extractors, e.g.,   \verb{The window sizes chosen to be: 2 months} - Features that are specified as known-in-advance, e.g.,   \verb{Variables treated as apriori: holiday} - Details about features generated as timeseries features, and their   priority, e.g.,   \verb{Generating feature \\"date (actual)\\" from \\"date\\" (priority: 1)} - Details about why certain variables are transformed in the input data,   e.g.,   \verb{Generating variable \\"y (log)\\" from \\"y\\" because multiplicative trend is detected}

This method invokes \verb{GET /projects/\{projectId\}/timeSeriesFeatureLog/} in the DataRobot Public API.
}
\seealso{
The method ProjectsTimeSeriesFeatureLogList in \link{DatetimePartitioningApi}, which invokes the same underlying API endpoint.
}
