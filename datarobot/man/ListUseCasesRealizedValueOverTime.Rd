% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use_case_apiWrapper.R
\name{ListUseCasesRealizedValueOverTime}
\alias{ListUseCasesRealizedValueOverTime}
\title{ListUseCasesRealizedValueOverTime}
\usage{
ListUseCasesRealizedValueOverTime(useCaseId, start, end, bucketSize, ...)
}
\arguments{
\item{...}{Additional keyword arguments to be passed on to the \code{datarobot.apicore} and \code{httr} libraries.}
}
\description{
Retrieve realized value information for a given use case over a period of time
}
\details{
Retrieve realized value information for a given use case over a period of time

This method invokes \verb{GET /useCases/\{useCaseId\}/realizedValueOverTime/} in the DataRobot Public API.
}
\seealso{
The method UseCasesRealizedValueOverTimeList in \link[datarobot.apicore:UseCaseApi]{datarobot.apicore::UseCaseApi}, which invokes the same underlying API endpoint.
}
