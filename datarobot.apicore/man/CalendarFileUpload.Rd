% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calendar_file_upload.R
\docType{class}
\name{CalendarFileUpload}
\alias{CalendarFileUpload}
\title{CalendarFileUpload}
\format{
An \code{R6Class} generator object
}
\description{
CalendarFileUpload Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{file}}{data.frame The calendar file used to create a calendar. The calendar file expect to meet the following criteria:  Must be in a csv or xlsx format.  Must have a header row. The names themselves in the header row can be anything.  Must have a single date column, in YYYY-MM-DD format.  May optionally have a name column as the second column.  May optionally have one series ID column that states what series each event is applicable for. If present, the name of this column must be specified in the `multiseriesIdColumns` parameter.}

\item{\code{multiseriesIdColumns}}{character \link{optional} An array of multiseries ID column names for the calendar file. Currently only one multiseries ID column is supported. If not specified, the calendar is considered to be single series.}

\item{\code{name}}{character \link{optional} The name of the calendar file. If not provided, this will be set to the name of the provided file.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CalendarFileUpload-new}{\code{CalendarFileUpload$new()}}
\item \href{#method-CalendarFileUpload-validate}{\code{CalendarFileUpload$validate()}}
\item \href{#method-CalendarFileUpload-toJSON}{\code{CalendarFileUpload$toJSON()}}
\item \href{#method-CalendarFileUpload-fromJSON}{\code{CalendarFileUpload$fromJSON()}}
\item \href{#method-CalendarFileUpload-clone}{\code{CalendarFileUpload$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CalendarFileUpload-new"></a>}}
\if{latex}{\out{\hypertarget{method-CalendarFileUpload-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CalendarFileUpload$new(
  file = NULL,
  multiseriesIdColumns = NULL,
  name = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{file}}{The calendar file used to create a calendar. The calendar file expect to meet the following criteria:  Must be in a csv or xlsx format.  Must have a header row. The names themselves in the header row can be anything.  Must have a single date column, in YYYY-MM-DD format.  May optionally have a name column as the second column.  May optionally have one series ID column that states what series each event is applicable for. If present, the name of this column must be specified in the `multiseriesIdColumns` parameter.}

\item{\code{multiseriesIdColumns}}{An array of multiseries ID column names for the calendar file. Currently only one multiseries ID column is supported. If not specified, the calendar is considered to be single series.}

\item{\code{name}}{The name of the calendar file. If not provided, this will be set to the name of the provided file.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CalendarFileUpload-validate"></a>}}
\if{latex}{\out{\hypertarget{method-CalendarFileUpload-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CalendarFileUpload$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CalendarFileUpload-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-CalendarFileUpload-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CalendarFileUpload$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CalendarFileUpload-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-CalendarFileUpload-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CalendarFileUpload$fromJSON(CalendarFileUploadJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{CalendarFileUploadJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CalendarFileUpload-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CalendarFileUpload-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CalendarFileUpload$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
