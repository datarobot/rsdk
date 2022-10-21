Deprecated <- function(message, deprecatedInVersion, removedInVersion) {
  .Deprecated(msg = sprintf("%s has been deprecated in %s, will be removed in %s.",
                            message, deprecatedInVersion, removedInVersion))
}
