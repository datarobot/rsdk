% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_package_import_meta.R
\docType{class}
\name{ModelPackageImportMeta}
\alias{ModelPackageImportMeta}
\title{ModelPackageImportMeta}
\format{
An \code{R6Class} generator object
}
\description{
ModelPackageImportMeta Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{containsFearPipeline}}{character \link{optional} Exists for imported models only, indicates thatmodel package contains file with fear pipeline.}

\item{\code{containsFeaturelists}}{character \link{optional} Exists for imported models only, indicates thatmodel package contains file with featurelists.}

\item{\code{containsLeaderboardMeta}}{character \link{optional} Exists for imported models only, indicates thatmodel package contains file with leaderboard meta.}

\item{\code{containsProjectMeta}}{character \link{optional} Exists for imported models only, indicates thatmodel package contains file with project meta.}

\item{\code{creatorId}}{character User ID of the person who created this Model Package}

\item{\code{creatorUsername}}{character Username of the person who created this model package}

\item{\code{dateCreated}}{character When this Model Package was created}

\item{\code{originalFileName}}{character Exists for imported models only, the original file name that was uploaded}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ModelPackageImportMeta-new}{\code{ModelPackageImportMeta$new()}}
\item \href{#method-ModelPackageImportMeta-validate}{\code{ModelPackageImportMeta$validate()}}
\item \href{#method-ModelPackageImportMeta-toJSON}{\code{ModelPackageImportMeta$toJSON()}}
\item \href{#method-ModelPackageImportMeta-fromJSON}{\code{ModelPackageImportMeta$fromJSON()}}
\item \href{#method-ModelPackageImportMeta-clone}{\code{ModelPackageImportMeta$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageImportMeta-new"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageImportMeta-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageImportMeta$new(
  creatorId = NULL,
  creatorUsername = NULL,
  dateCreated = NULL,
  originalFileName = NULL,
  containsFearPipeline = NULL,
  containsFeaturelists = NULL,
  containsLeaderboardMeta = NULL,
  containsProjectMeta = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{creatorId}}{User ID of the person who created this Model Package}

\item{\code{creatorUsername}}{Username of the person who created this model package}

\item{\code{dateCreated}}{When this Model Package was created}

\item{\code{originalFileName}}{Exists for imported models only, the original file name that was uploaded}

\item{\code{containsFearPipeline}}{Exists for imported models only, indicates thatmodel package contains file with fear pipeline.}

\item{\code{containsFeaturelists}}{Exists for imported models only, indicates thatmodel package contains file with featurelists.}

\item{\code{containsLeaderboardMeta}}{Exists for imported models only, indicates thatmodel package contains file with leaderboard meta.}

\item{\code{containsProjectMeta}}{Exists for imported models only, indicates thatmodel package contains file with project meta.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageImportMeta-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageImportMeta-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageImportMeta$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageImportMeta-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageImportMeta-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageImportMeta$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageImportMeta-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageImportMeta-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageImportMeta$fromJSON(
  ModelPackageImportMetaJson,
  validateParams = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ModelPackageImportMetaJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ModelPackageImportMeta-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ModelPackageImportMeta-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ModelPackageImportMeta$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
