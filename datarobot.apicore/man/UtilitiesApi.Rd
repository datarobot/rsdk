% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities_api.R
\docType{class}
\name{UtilitiesApi}
\alias{UtilitiesApi}
\title{Utilities operations}
\format{
An \code{R6Class} generator object
}
\description{
datarobot.apicore.Utilities
}
\examples{

## ------------------------------------------------
## Method `UtilitiesApi$StringEncryptionsCreate`
## ------------------------------------------------

\dontrun{
library(datarobot.apicore)
stringEncryption <- StringEncryption$new() # StringEncryption | 

api.instance <- UtilitiesApi$new()
result <- api.instance$StringEncryptionsCreate(stringEncryption=stringEncryption)
}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{apiClient}}{Handles the client-server communication.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-UtilitiesApi-new}{\code{UtilitiesApi$new()}}
\item \href{#method-UtilitiesApi-StringEncryptionsCreate}{\code{UtilitiesApi$StringEncryptionsCreate()}}
\item \href{#method-UtilitiesApi-clone}{\code{UtilitiesApi$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UtilitiesApi-new"></a>}}
\if{latex}{\out{\hypertarget{method-UtilitiesApi-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UtilitiesApi$new(apiClient)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{apiClient}}{A configurable \code{ApiClient} instance. If none provided, a new client with default configuration will be created.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UtilitiesApi-StringEncryptionsCreate"></a>}}
\if{latex}{\out{\hypertarget{method-UtilitiesApi-StringEncryptionsCreate}{}}}
\subsection{Method \code{StringEncryptionsCreate()}}{
Encrypt a string which DataRobot can decrypt.
Produces: "application/json"
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UtilitiesApi$StringEncryptionsCreate(stringEncryption = NULL, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{stringEncryption}}{\link{StringEncryption}.}

\item{\code{...}}{Optional. Additional named parameters to be passed downward.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
DataRobot does not store your credentials when accessing external data stores. Instead you first encrypt your sensitive information with DataRobot and then, when accessing those data stores, supply the encrypted credentials. DataRobot will decrypt your credentials and use them to establish a connection to your data store.


This method invokes \verb{POST /stringEncryptions/} in the DataRobot Public API.


Response status codes, messages, and headers:
\itemize{
\item \strong{\code{200}} Encryted string.
\itemize{
}
}
}

\subsection{Returns}{
\link{StringEncryptionResponse}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
library(datarobot.apicore)
stringEncryption <- StringEncryption$new() # StringEncryption | 

api.instance <- UtilitiesApi$new()
result <- api.instance$StringEncryptionsCreate(stringEncryption=stringEncryption)
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-UtilitiesApi-clone"></a>}}
\if{latex}{\out{\hypertarget{method-UtilitiesApi-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{UtilitiesApi$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
