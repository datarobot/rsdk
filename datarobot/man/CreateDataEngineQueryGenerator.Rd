% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ai_catalog_apiWrapper.R,
%   R/DataEngineQueryGenerator.R
\name{CreateDataEngineQueryGenerator}
\alias{CreateDataEngineQueryGenerator}
\title{CreateDataEngineQueryGenerator}
\usage{
CreateDataEngineQueryGenerator(generatorType, dataset, generatorSettings)

CreateDataEngineQueryGenerator(generatorType, dataset, generatorSettings)
}
\arguments{
\item{generatorType}{character. The type of query generator to create. "TimeSeries" is supported.}

\item{dataset}{object of class \code{queryGeneratorDataset}.}

\item{generatorSettings}{object of class \code{queryGeneratorSettings}.}

\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\value{
A query generator object:
\itemize{
\item Id. character. The ID of the created query generator.
\item datasets. list of objects containing elements
\itemize{
\item alias. character. The alias for the dataset used in the query.
\item datasetId. character. The ID of the dataset.
\item datasetVersionId. character. The ID of the dataset version.
}
\item generatorType. character. The type of the query generator.
\item generatorSettings. object containing elements
\itemize{
\item datetimePartitionColumn. character. The name of the date column.
\item timeUnit. character. The unit of the time step.
\item timeStep. integer. The number of units in the time step.
\item defaultNumericAggregationMethod. character. Aggregation for numeric columns.
\item defaultCategoricalAggregationMethod. character. Aggregation for categorical columns.
\item target. character. The name of the target column.
\item multiseriesIdColumns. list of character. Optional. The name of the series id column.
\item defaultTextAggregationMethod. character. Optional. Aggregation for text columns.
\item startFromSeriesMinDatetime. logical. Optional. Whether to use per series start date.
\item endToSeriesMaxDatetime. logical. Optional. Whether to use per series end date.
}
\item query. character. The text of the generated Spark SQL query.
}
}
\description{
Create a data engine query generator
}
\details{
Create a data engine query generator.

This method invokes \verb{POST /dataEngineQueryGenerators/} in the DataRobot Public API.

Create a DataEngineQueryGenerator for time series data prep.
}
\examples{
\dontrun{
generatorType <- "TimeSeries"
dataset <- list(alias = "my_dataset", datasetId = "59a5af20c80891534e3c2bde")
generatorSettings <- list(
  datetimePartitionColumn = "date",
  timeUnit = "DAY",
  timeStep = 1,
  defaultNumericAggregationMethod = "sum",
  defaultCategoricalAggregationMethod = "mostFrequent",
  target = "y"
)
queryGenerator <- CreateDataEngineQueryGenerator(generatorType, dataset, generatorSettings)
}
}
\seealso{
The method DataEngineQueryGeneratorsCreate in \link[datarobot.apicore:AiCatalogApi]{datarobot.apicore::AiCatalogApi}, which invokes the same underlying API endpoint.

Other Data Engine Query Generator functions: 
\code{\link{CreateDatasetFromDataEngineQueryGenerator}()},
\code{\link{GetDataEngineQueryGenerator}()}
}
\concept{Data Engine Query Generator functions}
