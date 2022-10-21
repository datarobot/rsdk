#' Create a calendar from an uploaded CSV.
#'
#' @param dataSource object. Either (a) the name of a CSV file, or (b) a dataframe.
#'   This parameter identifies the source of the calendar data.
#' @param name character. Optional. The name of the calendar.
#' @param multiSeriesIdColumn character. Optional. Added in 2.19. The column in
#'   the calendar that defines which series an event belongs to. Only one
#'   column is supported.
#' @param maxWait integer. The maximum time (in seconds) to wait for the retrieve to complete.
#' @return An S3 object of class "dataRobotCalendar"
#' @examples
#' \dontrun{
#'    CreateCalendar("inst/extdata/calendar.csv", name = "intlHolidayCalendar")
#' }
#' \dontrun{
#'    holidayCalendarDF <- as.data.frame(myCalendar)
#'    CreateCalendar(holidayCalendarDF, name = "intlHolidayCalendar")
#' }
#' \dontrun{
#'    CreateCalendar("inst/extdata/calendar.csv",
#'                   name = "intlHolidayCalendar",
#'                   multiSeriesIdColumn = "Country")
#' }
#' @export
CreateCalendar <- function(dataSource,
                           name = NULL,
                           multiSeriesIdColumn = NULL,
                           maxWait = 600) {
  if (length(multiSeriesIdColumn) > 1) {
    stop("Only a single column can be used to define events.")
  }
  if (is.list(multiSeriesIdColumn)) {
    # take the first
    multiSeriesIdColumn <- multiSeriesIdColumn[[1]]
  }

  if (is.null(name)) { name <- dataSource }
  routeString <- UrlJoin("calendars", "fileUpload")
  body <- list(name = name, file = UploadData(dataSource))
  if (!is.null(multiSeriesIdColumn)) {
    # API expects a JSON array string like '["series_id"]' or nothing at all
    body$multiseriesIdColumns <- jsonlite::toJSON(multiSeriesIdColumn)
  }
  postResponse <- DataRobotPOST(routeString, body = body, returnRawResponse = TRUE)
  calendar <- WaitForAsyncReturn(GetRedirectFromResponse(postResponse),
                                 maxWait = maxWait,
                                 addUrl = FALSE,
                                 # TODO make the failureStatus check case-insensitive DSX-1228
                                 failureStatuses = c(JobFailureStatuses, "ERROR"))
  as.dataRobotCalendar(calendar)
}

as.dataRobotCalendar <- function(inList) {
  outList <- inList
  # /calendars/{id} returns 'Id' instead of 'id' like normal; let's fix that
  # TODO Refactor into a rename() function in utils.R
  outList$id <- outList$Id
  outList$Id <- NULL
  class(outList) <- "dataRobotCalendar"
  outList
}


#' Retrieve a calendar
#'
#' @param calendarId character. The ID of the calendar to retrieve.
#' @return An S3 object of class "dataRobotCalendar"
#' @examples
#' \dontrun{
#'    calendarId <- "5da75da31fb4a45b8a815a53"
#'    GetCalendar(calendarId)
#' }
#' @export
GetCalendar <- function(calendarId) {
  routeString <- UrlJoin("calendars", calendarId)
  as.dataRobotCalendar(DataRobotGET(routeString))
}


#' Retrieve the calendar for a particular project.
#'
#' @inheritParams GetProject
#' @return An S3 object of class "dataRobotCalendar"
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'    GetCalendar(projectId)
#' }
#' @export
GetCalendarFromProject <- function(project) {
  GetCalendar(GetDatetimePartition(project)$calendarId)
}


#' List all available calendars.
#'
#' @return A list of S3 objects of class "dataRobotCalendar"
#' @examples
#' \dontrun{
#'    ListCalendars()
#' }
#' @export
ListCalendars <- function() {
  calendars <- DataRobotGET("calendars", simplifyDataFrame = FALSE)
  calendars <- GetServerDataInRows(calendars)
  calendars <- lapply(calendars, as.dataRobotCalendar)
  class(calendars) <- c("listOfCalendars", "listSubclass")
  calendars
}


#' Delete a calendar
#'
#' @inheritParams GetCalendar
#' @return NULL
#' @examples
#' \dontrun{
#'    calendarId <- "5da75da31fb4a45b8a815a53"
#'    DeleteCalendar(calendarId)
#' }
#' @export
DeleteCalendar <- function(calendarId) {
  calendarId <- ValidateCalendar(calendarId)
  routeString <- UrlJoin("calendars", calendarId)
  DataRobotDELETE(routeString)
  invisible(NULL)
}


#' Update a calendar
#'
#' Currently supports changing the name of a calendar.
#'
#' @inheritParams GetCalendar
#' @param name character. The new name to name the calendar.
#' @return An S3 object of class "dataRobotCalendar"
#' @examples
#' \dontrun{
#'    calendarId <- "5da75da31fb4a45b8a815a53"
#'    UpdateCalendar(calendarId, name = "New name for calendar")
#' }
#' @export
UpdateCalendar <- function(calendarId, name = NULL) {
  calendarId <- ValidateCalendar(calendarId)
  routeString <- UrlJoin("calendars", calendarId)
  DataRobotPATCH(routeString, body = list(name = name))
  GetCalendar(calendarId)
}
