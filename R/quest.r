#' Run quest app function
#'
#' This function runs the quest application.
#' quest()

quest <- function() {
  appDir <- system.file("quest", package = "quest")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `quest`.", call. = FALSE)
  }
  
  shiny::runApp(appDir)
}