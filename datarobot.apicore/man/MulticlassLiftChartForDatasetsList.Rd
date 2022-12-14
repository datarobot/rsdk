% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiclass_lift_chart_for_datasets_list.R
\docType{class}
\name{MulticlassLiftChartForDatasetsList}
\alias{MulticlassLiftChartForDatasetsList}
\title{MulticlassLiftChartForDatasetsList}
\format{
An \code{R6Class} generator object
}
\description{
MulticlassLiftChartForDatasetsList Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{count}}{integer Number of results returned on this page.}

\item{\code{data}}{list( \link{MulticlassLiftData} ) Array of multiclass lift chart data for dataset, as specified below.}

\item{\code{modelId}}{character The model id to which the chart data belongs.}

\item{\code{next_}}{character URL pointing to the next page (if null, there is no next page).}

\item{\code{previous}}{character URL pointing to the previous page (if null, there is no previous page).}

\item{\code{projectId}}{character The project id to which the chart data belongs.}

\item{\code{totalCount}}{integer Total count of multiclass lift charts matching to the query condition.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MulticlassLiftChartForDatasetsList-new}{\code{MulticlassLiftChartForDatasetsList$new()}}
\item \href{#method-MulticlassLiftChartForDatasetsList-validate}{\code{MulticlassLiftChartForDatasetsList$validate()}}
\item \href{#method-MulticlassLiftChartForDatasetsList-toJSON}{\code{MulticlassLiftChartForDatasetsList$toJSON()}}
\item \href{#method-MulticlassLiftChartForDatasetsList-fromJSON}{\code{MulticlassLiftChartForDatasetsList$fromJSON()}}
\item \href{#method-MulticlassLiftChartForDatasetsList-clone}{\code{MulticlassLiftChartForDatasetsList$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MulticlassLiftChartForDatasetsList-new"></a>}}
\if{latex}{\out{\hypertarget{method-MulticlassLiftChartForDatasetsList-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MulticlassLiftChartForDatasetsList$new(
  count = NULL,
  data = NULL,
  modelId = NULL,
  next_ = NULL,
  previous = NULL,
  projectId = NULL,
  totalCount = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{count}}{Number of results returned on this page.}

\item{\code{data}}{Array of multiclass lift chart data for dataset, as specified below.}

\item{\code{modelId}}{The model id to which the chart data belongs.}

\item{\code{next_}}{URL pointing to the next page (if null, there is no next page).}

\item{\code{previous}}{URL pointing to the previous page (if null, there is no previous page).}

\item{\code{projectId}}{The project id to which the chart data belongs.}

\item{\code{totalCount}}{Total count of multiclass lift charts matching to the query condition.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MulticlassLiftChartForDatasetsList-validate"></a>}}
\if{latex}{\out{\hypertarget{method-MulticlassLiftChartForDatasetsList-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MulticlassLiftChartForDatasetsList$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MulticlassLiftChartForDatasetsList-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-MulticlassLiftChartForDatasetsList-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MulticlassLiftChartForDatasetsList$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MulticlassLiftChartForDatasetsList-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-MulticlassLiftChartForDatasetsList-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MulticlassLiftChartForDatasetsList$fromJSON(
  MulticlassLiftChartForDatasetsListJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{MulticlassLiftChartForDatasetsListJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MulticlassLiftChartForDatasetsList-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MulticlassLiftChartForDatasetsList-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MulticlassLiftChartForDatasetsList$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
