
#' factor_limit function
#'
#' This function builds a ggplot2 function to plot data
#' @param t Table of data
#' @param c Selected column to test
#' @param lim Limit to number of levels
#' @keywords factor limits
#' @export
#' @examples
#' factor_limit()


factor_limit<-function(t,c,lim){
  if(!is.na(c)){
    for(i in 1:length(c)){
      if(is.factor(t[,c[1]])&length(levels(t[,c[1]]))>lim){
        stop(paste0("Cannot use ",c[1],": factor variable with >",lim," levels"))
      }
    }
  }
}