% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataSources.R
\name{UpdateDataSource}
\alias{UpdateDataSource}
\title{Update a data store.}
\usage{
UpdateDataSource(
  dataSourceId,
  canonicalName = NULL,
  dataStoreId = NULL,
  query = NULL,
  table = NULL,
  schema = NULL,
  partitionColumn = NULL,
  fetchSize = NULL
)
}
\arguments{
\item{dataSourceId}{character. The ID of the data store to update.}

\item{canonicalName}{character. The user-friendly name of the data source.}

\item{dataStoreId}{character. The ID of the data store to connect to.}

\item{query}{character. A query to execute on the data store to get the data. Optional.}

\item{table}{character. The specified database table. Optional.}

\item{schema}{character. The specified database schema. Optional.}

\item{partitionColumn}{character. The name of the partition column. Optional.}

\item{fetchSize}{integer. a user specified fetch size in the range [1, 20000]. Optional.
By default a fetchSize will be assigned to balance throughput and memory usage}
}
\description{
Update a data store.
}
\examples{
\dontrun{
 dataSourceId <- "5c1303269300d900016b41a7"
 UpdateDataSource(dataSourceId, canonicalName = "Different Name")
}
}