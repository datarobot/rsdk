% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_package_bias_and_fairness.R
\docType{class}
\name{ModelPackageBiasAndFairness}
\alias{ModelPackageBiasAndFairness}
\title{ModelPackageBiasAndFairness}
\format{
An \code{R6Class} generator object
}
\description{
ModelPackageBiasAndFairness Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{fairnessMetricsSet}}{character A set of fairness metrics to use for calculating fairness.}

\item{\code{fairnessThreshold}}{numeric Threshold value of the fairness metric. Can be in a range of ``\link{0:1}``.}

\item{\code{preferableTargetValue}}{\link{AnyOfbooleanintegerstring} A target value that should be treated as a positive outcome for the prediction. For example if we want to check gender discrimination for giving a loan and our target named ``is_bad``, then the positive outcome for the prediction would be ``No``, meaning that the loan is good and that's what we treat as a preferable result for the loaner.}

\item{\code{protectedFeatures}}{list( character ) A list of feature to mark as protected for Bias and Fairness measurement. A common example of those would `race`, `gender`, `age_group`. All protected features should be tracked as segment attributes. Only categorical features can be marked as protected.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ModelPackageBiasAndFairness-new}{\code{ModelPackageBiasAndFairness$new()}}
\item \href{#method-ModelPackageBiasAndFairness-validate}{\code{ModelPackageBiasAndFairness$validate()}}
\item \href{#method-ModelPackageBiasAndFairness-toJSON}{\code{ModelPackageBiasAndFairness$toJSON()}}
\item \href{#method-ModelPackageBiasAndFairness-fromJSON}{\code{ModelPackageBiasAndFairness$fromJSON()}}
\item \href{#method-ModelPackageBiasAndFairness-clone}{\code{ModelPackageBiasAndFairness$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageBiasAndFairness-new"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageBiasAndFairness-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageBiasAndFairness$new(
  fairnessMetricsSet = NULL,
  fairnessThreshold = NULL,
  preferableTargetValue = NULL,
  protectedFeatures = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{fairnessMetricsSet}}{A set of fairness metrics to use for calculating fairness.}

\item{\code{fairnessThreshold}}{Threshold value of the fairness metric. Can be in a range of ``\link{0:1}``.}

\item{\code{preferableTargetValue}}{A target value that should be treated as a positive outcome for the prediction. For example if we want to check gender discrimination for giving a loan and our target named ``is_bad``, then the positive outcome for the prediction would be ``No``, meaning that the loan is good and that's what we treat as a preferable result for the loaner.}

\item{\code{protectedFeatures}}{A list of feature to mark as protected for Bias and Fairness measurement. A common example of those would `race`, `gender`, `age_group`. All protected features should be tracked as segment attributes. Only categorical features can be marked as protected.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageBiasAndFairness-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageBiasAndFairness-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageBiasAndFairness$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageBiasAndFairness-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageBiasAndFairness-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageBiasAndFairness$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageBiasAndFairness-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageBiasAndFairness-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageBiasAndFairness$fromJSON(
  ModelPackageBiasAndFairnessJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ModelPackageBiasAndFairnessJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageBiasAndFairness-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageBiasAndFairness-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageBiasAndFairness$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
