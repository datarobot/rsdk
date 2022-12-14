% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DatetimeTrendPlots.R
\name{getMetadataStatus}
\alias{getMetadataStatus}
\title{A helper function to retrieve the status for specific backtest and source
for Datetime trend plots metadata.}
\usage{
getMetadataStatus(metadata, backtest, source)
}
\arguments{
\item{metadata}{list. The list retrieved from one of \code{GetAccuracyOverTimePlotsMetadata},
\code{GetForecastVsActualPlotsMetadata} or \code{GetAnomalyOverTimePlotsMetadata}.}

\item{backtest}{integer or character. Retrieve plots for a specific backtest.
Use the backtest index starting from zero.
To retrieve plots for holdout, use \code{DataSubset$Holdout}.}

\item{source}{character. The source of the data for the backtest/holdout.
Must be one of \code{SourceType}.}
}
\value{
character (Accuracy over Time, Anomaly over Time) or the list (Forecast vs Actual).
}
\description{
A helper function to retrieve the status for specific backtest and source
for Datetime trend plots metadata.
}
