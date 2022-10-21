# The `onload` function is usually defined in `zzz.R` in order to make sure that all of the
# rest of the package is loaded before we try to run it (because R loads the files in
# alphabetical order)

.onAttach <- function(libname, pkgname) {
  configPath <- GetDefaultConfigPath()
  if (file.exists(configPath)) {
    packageStartupMessage(paste("Authenticating with config at:", configPath))
    tryCatch(ConnectWithConfigFile(configPath),
             error = function(e) packageStartupMessage(e$message))
  } else {
    packageStartupMessage(paste("Did not connect to DataRobot on package startup.",
                                "Use `ConnectToDataRobot`."))
    packageStartupMessage(paste("To connect by default on startup, you can put a config file at:",
                                configPath))
  }
}
