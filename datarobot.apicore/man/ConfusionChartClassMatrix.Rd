% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/confusion_chart_class_matrix.R
\docType{class}
\name{ConfusionChartClassMatrix}
\alias{ConfusionChartClassMatrix}
\title{ConfusionChartClassMatrix}
\format{
An \code{R6Class} generator object
}
\description{
ConfusionChartClassMatrix Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{actualCount}}{integer number of times this class is seen in the validation data}

\item{\code{className}}{character name of the class}

\item{\code{confusionMatrixOneVsAll}}{list( \link{array[integer]} ) 2d array representing 2x2 one vs all matrix. This represents the True/False Negative/Positive rates as integer for each class. The data structure looks like: ``[ \link{ True Negative, False Positive }, \link{ False Negative, True Positive } ]``}

\item{\code{f1}}{numeric F1 score}

\item{\code{precision}}{numeric precision score}

\item{\code{predictedCount}}{integer number of times this class has been predicted for the validation data}

\item{\code{recall}}{numeric recall score}

\item{\code{wasActualPercentages}}{list( \link{ActualPercentages} ) one vs all actual percentages in a format specified below}

\item{\code{wasPredictedPercentages}}{list( \link{PredictedPercentages} ) one vs all predicted percentages in a format specified below}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ConfusionChartClassMatrix-new}{\code{ConfusionChartClassMatrix$new()}}
\item \href{#method-ConfusionChartClassMatrix-validate}{\code{ConfusionChartClassMatrix$validate()}}
\item \href{#method-ConfusionChartClassMatrix-toJSON}{\code{ConfusionChartClassMatrix$toJSON()}}
\item \href{#method-ConfusionChartClassMatrix-fromJSON}{\code{ConfusionChartClassMatrix$fromJSON()}}
\item \href{#method-ConfusionChartClassMatrix-clone}{\code{ConfusionChartClassMatrix$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConfusionChartClassMatrix-new"></a>}}
\if{latex}{\out{\hypertarget{method-ConfusionChartClassMatrix-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConfusionChartClassMatrix$new(
  actualCount = NULL,
  className = NULL,
  confusionMatrixOneVsAll = NULL,
  f1 = NULL,
  precision = NULL,
  predictedCount = NULL,
  recall = NULL,
  wasActualPercentages = NULL,
  wasPredictedPercentages = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{actualCount}}{number of times this class is seen in the validation data}

\item{\code{className}}{name of the class}

\item{\code{confusionMatrixOneVsAll}}{2d array representing 2x2 one vs all matrix. This represents the True/False Negative/Positive rates as integer for each class. The data structure looks like: ``[ \link{ True Negative, False Positive }, \link{ False Negative, True Positive } ]``}

\item{\code{f1}}{F1 score}

\item{\code{precision}}{precision score}

\item{\code{predictedCount}}{number of times this class has been predicted for the validation data}

\item{\code{recall}}{recall score}

\item{\code{wasActualPercentages}}{one vs all actual percentages in a format specified below}

\item{\code{wasPredictedPercentages}}{one vs all predicted percentages in a format specified below}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConfusionChartClassMatrix-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ConfusionChartClassMatrix-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConfusionChartClassMatrix$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConfusionChartClassMatrix-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ConfusionChartClassMatrix-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConfusionChartClassMatrix$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConfusionChartClassMatrix-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ConfusionChartClassMatrix-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConfusionChartClassMatrix$fromJSON(
  ConfusionChartClassMatrixJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ConfusionChartClassMatrixJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ConfusionChartClassMatrix-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ConfusionChartClassMatrix-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ConfusionChartClassMatrix$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
