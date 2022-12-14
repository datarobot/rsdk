% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confusion_chart_data.R
\docType{class}
\name{ConfusionChartData}
\alias{ConfusionChartData}
\title{ConfusionChartData}
\format{
An \code{R6Class} generator object
}
\description{
ConfusionChartData Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{classMetrics}}{list( \link{ConfusionChartClassMatrix} ) per-class information including one vs all scores in a format specified below}

\item{\code{classes}}{list( character ) class labels from the dataset, union of row classes & column classes. This field is deprecated as of v2.13. The rows and columns may have different class labels when using query parameters to retrieve a slice of the matrix; please use 'rowClasses' and 'colClasses' instead.}

\item{\code{colClasses}}{list( character ) class labels on columns of confusion matrix}

\item{\code{confusionMatrix}}{list( \link{array[integer]} ) 2d array of ints representing confusion matrix, aligned with `rowClasses` and 'colClasses'array. For confusionMatrix\link[=B]{A} we can get an integer that represents the number of times 'if class with index A was correct we have class with index B predicted' (if the orientation is 'actual').}

\item{\code{rowClasses}}{list( character ) class labels on rows of confusion matrix}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ConfusionChartData-new}{\code{ConfusionChartData$new()}}
\item \href{#method-ConfusionChartData-validate}{\code{ConfusionChartData$validate()}}
\item \href{#method-ConfusionChartData-toJSON}{\code{ConfusionChartData$toJSON()}}
\item \href{#method-ConfusionChartData-fromJSON}{\code{ConfusionChartData$fromJSON()}}
\item \href{#method-ConfusionChartData-clone}{\code{ConfusionChartData$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConfusionChartData-new"></a>}}
\if{latex}{\out{\hypertarget{method-ConfusionChartData-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConfusionChartData$new(
  classMetrics = NULL,
  classes = NULL,
  colClasses = NULL,
  confusionMatrix = NULL,
  rowClasses = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{classMetrics}}{per-class information including one vs all scores in a format specified below}

\item{\code{classes}}{class labels from the dataset, union of row classes & column classes. This field is deprecated as of v2.13. The rows and columns may have different class labels when using query parameters to retrieve a slice of the matrix; please use 'rowClasses' and 'colClasses' instead.}

\item{\code{colClasses}}{class labels on columns of confusion matrix}

\item{\code{confusionMatrix}}{2d array of ints representing confusion matrix, aligned with `rowClasses` and 'colClasses'array. For confusionMatrix\link[=B]{A} we can get an integer that represents the number of times 'if class with index A was correct we have class with index B predicted' (if the orientation is 'actual').}

\item{\code{rowClasses}}{class labels on rows of confusion matrix}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConfusionChartData-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ConfusionChartData-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConfusionChartData$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConfusionChartData-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ConfusionChartData-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConfusionChartData$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConfusionChartData-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ConfusionChartData-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConfusionChartData$fromJSON(ConfusionChartDataJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ConfusionChartDataJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConfusionChartData-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ConfusionChartData-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConfusionChartData$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
