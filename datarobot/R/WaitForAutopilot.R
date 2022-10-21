#' This function periodically checks whether Autopilot is finished and returns only after it is.
#'
#' @param project character. The project for which you want to wait until autopilot is finished.
#' @param checkInterval numeric. Optional. Maximum wait (in seconds) between checks that Autopilot
#'   is finished. Defaults to 20.
#' @param timeout numeric. Optional. Time (in seconds) after which to give up (Default is no
#'   timeout). There is an error if Autopilot is not finished before timing out.
#' @param verbosity numeric. Optional. 0 is silent, 1 or more displays information about progress.
#'   Default is 1.
#' @examples
#' \dontrun{
#'   projectId <- "59a5af20c80891534e3c2bde"
#'   WaitForAutopilot(projectId)
#' }
#' @export
WaitForAutopilot <- function(project, checkInterval = 20.0, timeout = NULL, verbosity = 1) {
  GetWaitStatus <- StartRetryWaiter(timeout = timeout, maxdelay = checkInterval)
  stillTrying <- TRUE
  while (stillTrying) {
   waitStatus <- GetWaitStatus()
   stillTrying <- waitStatus$stillTrying
   projectStatus <- GetProjectStatus(project)
   if (verbosity > 0) {
     inprogress <- ListModelJobs(project, status = 'inprogress')
     queued <- ListModelJobs(project, status = 'queue')
     message(sprintf("In progress: %d, queued: %d (waited: %.0fs)",
             nrow(inprogress), nrow(queued), waitStatus$secondsWaited))
   }
   if (projectStatus$autopilotDone) {
     return(invisible())
   }
  }
  stop("Autopilot did not finish in the time allotted within the time specified by `timeout`")
}
