
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
    if(is.factor(t[,c])&length(levels(t[,c]))>lim){
      stop(paste0("Cannot use ",c,": factor variable with >",lim," levels"))
    }
  }
}