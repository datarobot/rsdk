GetSharingPath <- function(object) {
  if (is(object, "dataRobotProject")) {
    path <- "projects"
    id <- object$projectId
  } else if (is(object, "dataRobotDataSource")) {
    path <- "externalDataSources"
    id <- object$id
  } else if (is(object, "dataRobotDataStore")) {
    path <- "externalDataStores"
    id <- object$id
  } else if (is(object, "dataRobotCalendar")) {
    path <- "calendars"
    id <- object$id
  } else {
    klass <- paste(class(object), collapse = " ")
    stop("Objects of class ", klass, " cannot be shared.")
  }
  if (is.null(id)) {
    stop("Object ID could not be found.")
  }
  UrlJoin(path, id, "accessControl")
}


GetDefaultSharingRole <- function(object) {
  if (is(object, "dataRobotProject")) {
    role <- SharingRole$User
  } else if (is(object, "dataRobotDataSource") || is(object, "dataRobotDataStore")) {
    role <- SharingRole$Consumer
  } else if (is(object, "dataRobotCalendar")) {
    role <- SharingRole$ReadOnly
  } else {
    klass <- paste(class(object), collapse = " ")
    stop("Objects of class ", klass, " cannot be shared.")
  }
  role
}


#' List information about which users have what kinds of access to a shared object.
#'
#' Note that currently only data sources and data stores can be shared with this API.
#'
#' @param object object. The shared object to inspect access for.
#' @return A list specifying information on access:
#'   \itemize{
#'      \item username character. The name of the user with access.
#'      \item userId character. The ID of the user with access.
#'      \item role character. The type of access granted. See \code{SharingRole} for options.
#'      \item canShare logical. Whether the user can further share access.
#'   }
#' @inheritParams GetServerDataInRows
#' @examples
#' \dontrun{
#'  dataStoreId <- "5c1303269300d900016b41a7"
#'  dataStore <- GetDataStore(dataStoreId)
#'  ListSharingAccess(dataStore)
#' }
#' @export
ListSharingAccess <- function(object, batchSize = NULL) {
  access <- DataRobotGET(GetSharingPath(object), simplifyDataFrame = FALSE)
  access <- GetServerDataInRows(access, batchSize = batchSize)
  as.dataRobotAccessList(access)
}

as.dataRobotAccessList <- function(access) {
  lapply(access, ApplySchema, schema = c("username", "userId", "role", "canShare"))
}


ValidateAccessEntry <- function(entry) {
  if (!("username" %in% names(entry))) {
    stop("Access list is malformed: Does not contain `username`.")
  }
  if (!("role" %in% names(entry))) {
    stop("Access list is malformed: Does not contain `role`.")
  }
  if (!isTRUE(IsParameterIn(entry$role, SharingRole))) {
    stop(entry$role, " is not a valid role. See `SharingRole` for options.")
  }
  TRUE
}

ValidateAccessList <- function(access) {
  if (!is(access, "list")) {
    stop("Must specify access via an access list (see `ListSharingAccess`).")
  }
  lapply(access, ValidateAccessEntry)
  TRUE
}


FormatAccessList <- function(object, access) {
  if ("username" %in% names(access)) { # if access is a single list...
    access <- list(access)             # ...it needs to be coerced to list-of-lists
  }

  ValidateAccessList(access)

  # Projects and calendars don't respect canShare
  if (is(object, "dataRobotProject") || is(object, "dataRobotCalendar")) {
    schema <- c("username", "role")
  } else {
    schema <- c("username", "role", "canShare")
  }
  access <- lapply(access, ApplySchema, schema = schema)
  access <- lapply(access, function(a) lapply(a, jsonlite::unbox))

  # Why this is necessary I have no idea. :'(
  if (is(object, "dataRobotCalendar")) {
    list(users = access)
  } else {
    list(data = access)
  }
}


#' Update access to a particular object.
#'
#' @inheritParams ListSharingAccess
#' @param access dataRobotAccessList. A list specifying access given to all users. See
#'   \code{ListSharingAccess}.
#' @examples
#' \dontrun{
#'  dataStoreId <- "5c1303269300d900016b41a7"
#'  dataStore <- GetDataStore(dataStoreId)
#'  access <- ListSharingAccess(dataStore)
#'  # Remove access from the first user and grant it to foo@foo.com instead.
#'  access[[1]]$username <- "foo@foo.com"
#'  UpdateAccess(dataStore, access)
#'  # Change access to a Read Only role.
#'  access[[1]]$role <- SharingRole$ReadOnly
#'  UpdateAccess(dataStore, access)
#' }
#' @export
UpdateAccess <- function(object, access) {
  DataRobotPATCH(GetSharingPath(object), encode = "json",
                 body = FormatAccessList(object, access))
  message("Access updated.")
  invisible(NULL)
}


#' Share a shareable object with a particular user.
#'
#' See \code{SharingRole} for more details on available access levels that can be granted
#' to a user. Set \code{role} to \code{NULL} to revoke access to a particular user.
#'
#' @inheritParams ListSharingAccess
#' @param username character. The name of the user to share the object with.
#' @param role character. The role (access level) to give that user. See \code{SharingRole}.
#' @param canShare logical. Is the user allowed to further reshare?
#' @examples
#' \dontrun{
#'  dataStoreId <- "5c1303269300d900016b41a7"
#'  dataStore <- GetDataStore(dataStoreId)
#'  # Grant access to a particular user.
#'  Share(dataStore, "foo@foo.com")
#'  # Grant access in a Read Only role.
#'  Share(dataStore, "foo@foo.com", role = SharingRole$ReadOnly)
#'  # Revoke access
#'  Share(dataStore, "foo@foo.com", role = NULL)
#' }
#' @export
Share <- function(object, username, role = "default", canShare = NULL) {
  if (length(username) > 1) {
    stop("`Share` only supports sharing with one user at a time. Use `UpdateAccessList` or ",
         "call `Share` iteratively.")
  }
  if (identical(role, "default")) { role <- GetDefaultSharingRole(object) }
  access <- ListSharingAccess(object)
  if (username %in% lapply(access, `[[`, "username")) {
    subAccess <- list(username = username, role = role)
    subAccess$canShare <- canShare
    access[[which(lapply(access, `[[`, "username") == username)]] <- subAccess
  } else {
    subAccess <- list(username = username, role = role)
    subAccess$canShare <- canShare
    access <- c(access, list(subAccess))
  }
  access <- as.dataRobotAccessList(access)
  tryCatch(UpdateAccess(object, access),
           error = function(e) {
             if (grepl("Multiple changes were specified for a single user", as.character(e))) {
               stop("User ", username, " is already shared on this ", class(object), ". Use ",
                    "`UpdateAccess` to change access for this user instead.")
             } else if (grepl("The following users were not found", as.character(e))) {
               stop("User ", username, " was not found.")
             } else { stop(e) }
           })
  invisible(NULL)
}
