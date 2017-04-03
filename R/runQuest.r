#' Run quest app function
#' @keywords quest shiny
#' @export
#' @examples
#' This function runs the quest application.
#' quest()

runQuest <- function() {
  #qwd<<-getwd()
  appDir <- system.file("quest", package = "quest")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `quest`.", call. = FALSE)
  }
  
  shiny::runApp(appDir)
}