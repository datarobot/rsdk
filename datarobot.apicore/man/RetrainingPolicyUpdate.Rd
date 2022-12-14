% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retraining_policy_update.R
\docType{class}
\name{RetrainingPolicyUpdate}
\alias{RetrainingPolicyUpdate}
\title{RetrainingPolicyUpdate}
\format{
An \code{R6Class} generator object
}
\description{
RetrainingPolicyUpdate Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{action}}{character \link{optional} Configure the action to take on the resultant new model.}

\item{\code{autopilotOptions}}{\link{AutopilotOptions} \link{optional}}

\item{\code{description}}{character \link{optional} Description of the retraining policy.}

\item{\code{featureListStrategy}}{character \link{optional} Configure the feature list strategy used for modeling.}

\item{\code{modelSelectionStrategy}}{character \link{optional} Configure how new model is selected when the retraining policy runs.}

\item{\code{name}}{character \link{optional} Name of the retraining policy.}

\item{\code{projectOptions}}{\link{ProjectOptions} \link{optional}}

\item{\code{projectOptionsStrategy}}{character \link{optional} Configure the project option strategy used for modeling.}

\item{\code{timeSeriesOptions}}{\link{TimeSeriesOptions} \link{optional}}

\item{\code{trigger}}{\link{Trigger} \link{optional}}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-RetrainingPolicyUpdate-new}{\code{RetrainingPolicyUpdate$new()}}
\item \href{#method-RetrainingPolicyUpdate-validate}{\code{RetrainingPolicyUpdate$validate()}}
\item \href{#method-RetrainingPolicyUpdate-toJSON}{\code{RetrainingPolicyUpdate$toJSON()}}
\item \href{#method-RetrainingPolicyUpdate-fromJSON}{\code{RetrainingPolicyUpdate$fromJSON()}}
\item \href{#method-RetrainingPolicyUpdate-clone}{\code{RetrainingPolicyUpdate$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RetrainingPolicyUpdate-new"></a>}}
\if{latex}{\out{\hypertarget{method-RetrainingPolicyUpdate-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RetrainingPolicyUpdate$new(
  action = NULL,
  autopilotOptions = NULL,
  description = NULL,
  featureListStrategy = NULL,
  modelSelectionStrategy = NULL,
  name = NULL,
  projectOptions = NULL,
  projectOptionsStrategy = NULL,
  timeSeriesOptions = NULL,
  trigger = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{action}}{Configure the action to take on the resultant new model.}

\item{\code{description}}{Description of the retraining policy.}

\item{\code{featureListStrategy}}{Configure the feature list strategy used for modeling.}

\item{\code{modelSelectionStrategy}}{Configure how new model is selected when the retraining policy runs.}

\item{\code{name}}{Name of the retraining policy.}

\item{\code{projectOptionsStrategy}}{Configure the project option strategy used for modeling.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RetrainingPolicyUpdate-validate"></a>}}
\if{latex}{\out{\hypertarget{method-RetrainingPolicyUpdate-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RetrainingPolicyUpdate$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RetrainingPolicyUpdate-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-RetrainingPolicyUpdate-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RetrainingPolicyUpdate$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RetrainingPolicyUpdate-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-RetrainingPolicyUpdate-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RetrainingPolicyUpdate$fromJSON(
  RetrainingPolicyUpdateJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{RetrainingPolicyUpdateJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RetrainingPolicyUpdate-clone"></a>}}
\if{latex}{\out{\hypertarget{method-RetrainingPolicyUpdate-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RetrainingPolicyUpdate$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
