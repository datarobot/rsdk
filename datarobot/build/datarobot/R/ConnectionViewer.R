RStudioConnectionOpened <- function(endpoint, token) {
  observer <- getOption("connectionObserver")
  if (!is.null(observer)) {
    observer$connectionOpened(type = "DataRobot",
                              displayName = "DataRobot",
                              icon = file.path(system.file(file.path("icons"),
                                               package = "datarobot"), "datarobot.png"),
                              host = endpoint,
                              listObjectTypes = function() { list(table = NULL) },
                              connectCode = "datarobot::ConnectToDataRobot",
                              disconnect = function() { NULL },
                              listObjects = datarobot::ListProjects,
                              listColumns = datarobot::ListProjects,
                              previewObject = datarobot::ListProjects,
                              connectionObject = NULL)
  }
}
