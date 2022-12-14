% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calendar_events_response_query.R
\docType{class}
\name{CalendarEventsResponseQuery}
\alias{CalendarEventsResponseQuery}
\title{CalendarEventsResponseQuery}
\format{
An \code{R6Class} generator object
}
\description{
CalendarEventsResponseQuery Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{count}}{integer The number of items returned on this page.}

\item{\code{data}}{list( \link{CalendarEvent} ) An array of calendar events}

\item{\code{next_}}{character A URL pointing to the next page (if `null`, there is no next page).}

\item{\code{previous}}{character A URL pointing to the previous page (if `null`, there is no previous page).}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CalendarEventsResponseQuery-new}{\code{CalendarEventsResponseQuery$new()}}
\item \href{#method-CalendarEventsResponseQuery-validate}{\code{CalendarEventsResponseQuery$validate()}}
\item \href{#method-CalendarEventsResponseQuery-toJSON}{\code{CalendarEventsResponseQuery$toJSON()}}
\item \href{#method-CalendarEventsResponseQuery-fromJSON}{\code{CalendarEventsResponseQuery$fromJSON()}}
\item \href{#method-CalendarEventsResponseQuery-clone}{\code{CalendarEventsResponseQuery$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CalendarEventsResponseQuery-new"></a>}}
\if{latex}{\out{\hypertarget{method-CalendarEventsResponseQuery-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CalendarEventsResponseQuery$new(
  count = NULL,
  data = NULL,
  next_ = NULL,
  previous = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{count}}{The number of items returned on this page.}

\item{\code{data}}{An array of calendar events}

\item{\code{next_}}{A URL pointing to the next page (if `null`, there is no next page).}

\item{\code{previous}}{A URL pointing to the previous page (if `null`, there is no previous page).}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CalendarEventsResponseQuery-validate"></a>}}
\if{latex}{\out{\hypertarget{method-CalendarEventsResponseQuery-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CalendarEventsResponseQuery$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CalendarEventsResponseQuery-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-CalendarEventsResponseQuery-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CalendarEventsResponseQuery$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CalendarEventsResponseQuery-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-CalendarEventsResponseQuery-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CalendarEventsResponseQuery$fromJSON(
  CalendarEventsResponseQueryJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{CalendarEventsResponseQueryJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CalendarEventsResponseQuery-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CalendarEventsResponseQuery-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CalendarEventsResponseQuery$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
