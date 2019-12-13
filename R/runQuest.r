#' runQuest function
#'
#' This function builds a ggplot2 function to plot data
#' @param t Table of data
#' @param c Selected column to test
#' @param lim Limit to number of levels
#' @keywords factor limits
#' runQuest()

runQuest<-function(){
  appDir<-system.file("quest", package = "quest")
  if(appDir==""){
    stop("Could not find directory. Try re-installing `quest`.", call. = FALSE)
  }
  shiny::runApp(appDir)
}
