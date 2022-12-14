% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/forecast_vs_actual_plots_forecast_distances_status.R
\docType{class}
\name{ForecastVsActualPlotsForecastDistancesStatus}
\alias{ForecastVsActualPlotsForecastDistancesStatus}
\title{ForecastVsActualPlotsForecastDistancesStatus}
\format{
An \code{R6Class} generator object
}
\description{
ForecastVsActualPlotsForecastDistancesStatus Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{completed}}{list( integer ) \link{optional} An array of available forecast distances for the `completed` status. If there are no forecast distances for this status, it will not appear in the response.}

\item{\code{errored}}{list( integer ) \link{optional} An array of available forecast distances for the `errored` status. If there are no forecast distances for this status, it will not appear in the response.}

\item{\code{inProgress}}{list( integer ) \link{optional} An array of available forecast distances for the `inProgress` status. If there are no forecast distances for this status, it will not appear in the response.}

\item{\code{insufficientData}}{list( integer ) \link{optional} An array of available forecast distances for the `insufficientData` status. If there are no forecast distances for this status, it will not appear in the response.}

\item{\code{notCompleted}}{list( integer ) \link{optional} An array of available forecast distances for the `notCompleted` status. If there are no forecast distances for this status, it will not appear in the response.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ForecastVsActualPlotsForecastDistancesStatus-new}{\code{ForecastVsActualPlotsForecastDistancesStatus$new()}}
\item \href{#method-ForecastVsActualPlotsForecastDistancesStatus-validate}{\code{ForecastVsActualPlotsForecastDistancesStatus$validate()}}
\item \href{#method-ForecastVsActualPlotsForecastDistancesStatus-toJSON}{\code{ForecastVsActualPlotsForecastDistancesStatus$toJSON()}}
\item \href{#method-ForecastVsActualPlotsForecastDistancesStatus-fromJSON}{\code{ForecastVsActualPlotsForecastDistancesStatus$fromJSON()}}
\item \href{#method-ForecastVsActualPlotsForecastDistancesStatus-clone}{\code{ForecastVsActualPlotsForecastDistancesStatus$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ForecastVsActualPlotsForecastDistancesStatus-new"></a>}}
\if{latex}{\out{\hypertarget{method-ForecastVsActualPlotsForecastDistancesStatus-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ForecastVsActualPlotsForecastDistancesStatus$new(
  completed = NULL,
  errored = NULL,
  inProgress = NULL,
  insufficientData = NULL,
  notCompleted = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{completed}}{An array of available forecast distances for the `completed` status. If there are no forecast distances for this status, it will not appear in the response.}

\item{\code{errored}}{An array of available forecast distances for the `errored` status. If there are no forecast distances for this status, it will not appear in the response.}

\item{\code{inProgress}}{An array of available forecast distances for the `inProgress` status. If there are no forecast distances for this status, it will not appear in the response.}

\item{\code{insufficientData}}{An array of available forecast distances for the `insufficientData` status. If there are no forecast distances for this status, it will not appear in the response.}

\item{\code{notCompleted}}{An array of available forecast distances for the `notCompleted` status. If there are no forecast distances for this status, it will not appear in the response.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ForecastVsActualPlotsForecastDistancesStatus-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ForecastVsActualPlotsForecastDistancesStatus-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ForecastVsActualPlotsForecastDistancesStatus$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ForecastVsActualPlotsForecastDistancesStatus-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ForecastVsActualPlotsForecastDistancesStatus-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ForecastVsActualPlotsForecastDistancesStatus$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ForecastVsActualPlotsForecastDistancesStatus-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ForecastVsActualPlotsForecastDistancesStatus-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ForecastVsActualPlotsForecastDistancesStatus$fromJSON(
  ForecastVsActualPlotsForecastDistancesStatusJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ForecastVsActualPlotsForecastDistancesStatusJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ForecastVsActualPlotsForecastDistancesStatus-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ForecastVsActualPlotsForecastDistancesStatus-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ForecastVsActualPlotsForecastDistancesStatus$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
