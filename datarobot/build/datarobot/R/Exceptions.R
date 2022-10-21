# See http://adv-r.had.co.nz/Exceptions-Debugging.html

Condition <- function(subclass, message, ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call),
    ...
 )
}


Exception <- function(subclass, message = "", ...) {
  # This should be called only via specific exception type constructors, not directly.
  # Since this is a helper function to be called by other exception-generators,
  # `call` needs to look two calls up the stack.
  message <- paste0(subclass, ": ", message)
  return(Condition(c(subclass, "error"), message, call = call, ...))
}


Raise <- function(condition, call = sys.call(-1), ...) {
  # This wrapper around `stop` is necessary to get the `call` correct.
  condition$call <- call
  stop(condition)
}


Exceptions <- list(
  PendingJobFinished = function(message = "") Exception("PendingJobFinished", message = message),
  PendingJobNotFinished = function(message = "") Exception("PendingJobFinished", message = message),
  PendingJobFailed = function(message = "") Exception("PendingJobFailed", message = message),
  AsyncTimeout = function(message = "") Exception("AsyncTimeout", message = message)
)
