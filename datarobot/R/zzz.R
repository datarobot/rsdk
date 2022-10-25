# Copyright 2021 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
# The `onload` function is usually defined in `zzz.R` in order to make sure that all of the
# rest of the package is loaded before we try to run it (because R loads the files in
# alphabetical order)

.onAttach <- function(libname, pkgname) {
  configPath <- GetDefaultConfigPath()
  if (file.exists(configPath)) {
    packageStartupMessage(paste("Authenticating with config at:", configPath))
    tryCatch(ConnectWithConfigFile(configPath),
      error = function(e) packageStartupMessage(e$message)
    )
  } else {
    packageStartupMessage(paste(
      "Did not connect to DataRobot on package startup.",
      "Use `ConnectToDataRobot`."
    ))
    packageStartupMessage(paste(
      "To connect by default on startup, you can put a config file at:",
      configPath
    ))
  }

  if (requireNamespace("datarobot.apicore", quietly = TRUE)) {
    assign("dr",
      .createApiConvenienceWrapper(),
      pos = "package:datarobot"
    )
  }
}

#' Find all generators related to Api classes, e.g. `ProjectsApi`, and
#' instantiate them in a list in the `datarobot` namespace for convenience.
#' @keywords internal
.createApiConvenienceWrapper <- function() {
  # Is there a better way to retrieve all of the classes...?
  # 1. find all objects in apicore ns with names that end with 'Api'
  # 2. exclude anything that ends in 'OpenApi' bc those are actual schemas in the OpenAPI
  #    spec, not Api classes, i.e. `DatetimePartitioningDataForOpenApi`
  # 3. retrieve the class generators and instantiate a new object, add it to a list
  apiClasses <- grep(".*OpenApi$",
    ls("package:datarobot.apicore", pattern = ".*Api$"),
    invert = TRUE, value = TRUE
  )
  return(sapply(apiClasses,
    function(x) {
      getFromNamespace(x, ns = "datarobot.apicore")$new()
    },
    USE.NAMES = TRUE
  ))
}
