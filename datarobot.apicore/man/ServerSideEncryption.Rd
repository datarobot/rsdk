% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server_side_encryption.R
\docType{class}
\name{ServerSideEncryption}
\alias{ServerSideEncryption}
\title{ServerSideEncryption}
\format{
An \code{R6Class} generator object
}
\description{
ServerSideEncryption Class
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{algorithm}}{character \link{optional} The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).}

\item{\code{customerAlgorithm}}{character \link{optional} Specifies the algorithm to use to when encrypting the object (for example, AES256).}

\item{\code{customerKey}}{character \link{optional} Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in customerAlgorithm. The key must be sent as an base64 encoded string.}

\item{\code{kmsEncryptionContext}}{character \link{optional} Specifies the Amazon Web Services KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.}

\item{\code{kmsKeyId}}{character \link{optional} Specifies the ID of the symmetric customer managed key to use for object encryption.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ServerSideEncryption-new}{\code{ServerSideEncryption$new()}}
\item \href{#method-ServerSideEncryption-validate}{\code{ServerSideEncryption$validate()}}
\item \href{#method-ServerSideEncryption-toJSON}{\code{ServerSideEncryption$toJSON()}}
\item \href{#method-ServerSideEncryption-fromJSON}{\code{ServerSideEncryption$fromJSON()}}
\item \href{#method-ServerSideEncryption-clone}{\code{ServerSideEncryption$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ServerSideEncryption-new"></a>}}
\if{latex}{\out{\hypertarget{method-ServerSideEncryption-new}{}}}
\subsection{Method \code{new()}}{
A function used to initialize an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ServerSideEncryption$new(
  algorithm = NULL,
  customerAlgorithm = NULL,
  customerKey = NULL,
  kmsEncryptionContext = NULL,
  kmsKeyId = NULL,
  validateParams = FALSE,
  ...
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{algorithm}}{The server-side encryption algorithm used when storing this object in Amazon S3 (for example, AES256, aws:kms).}

\item{\code{customerAlgorithm}}{Specifies the algorithm to use to when encrypting the object (for example, AES256).}

\item{\code{customerKey}}{Specifies the customer-provided encryption key for Amazon S3 to use in encrypting data. This value is used to store the object and then it is discarded; Amazon S3 does not store the encryption key. The key must be appropriate for use with the algorithm specified in customerAlgorithm. The key must be sent as an base64 encoded string.}

\item{\code{kmsEncryptionContext}}{Specifies the Amazon Web Services KMS Encryption Context to use for object encryption. The value of this header is a base64-encoded UTF-8 string holding JSON with the encryption context key-value pairs.}

\item{\code{kmsKeyId}}{Specifies the ID of the symmetric customer managed key to use for object encryption.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters before initialization. Default FALSE.}

\item{\code{...}}{Any additional keyword arguments to be passed into this object for initialization.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ServerSideEncryption-validate"></a>}}
\if{latex}{\out{\hypertarget{method-ServerSideEncryption-validate}{}}}
\subsection{Method \code{validate()}}{
A helper function that provides public access to the private validateProps function. This allows users the ability
to programmatically validate objects before sending them to DataRobot.
checking this objects set properties.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ServerSideEncryption$validate()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ServerSideEncryption-toJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ServerSideEncryption-toJSON}{}}}
\subsection{Method \code{toJSON()}}{
A helper function that serializes this object into a JSON encoded string.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ServerSideEncryption$toJSON()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ServerSideEncryption-fromJSON"></a>}}
\if{latex}{\out{\hypertarget{method-ServerSideEncryption-fromJSON}{}}}
\subsection{Method \code{fromJSON()}}{
A helper function that deserializes a JSON string into an instance of this class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ServerSideEncryption$fromJSON(ServerSideEncryptionJson, validateParams = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ServerSideEncryptionJson}}{A JSON encoded string representation of a class instance.}

\item{\code{validateParams}}{An optional param for auto validating this object's parameters after deserialization. Default FALSE.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ServerSideEncryption-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ServerSideEncryption-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ServerSideEncryption$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
