#' rollPlot rolling plotting function
#'
#' This function orders and bins a variable in to groups defined by a window 
#' and step size then computes rolling averages for multipe variables across each bin.
#' @param t Table of data.
#' @param x Variable to be binned on x-axis.
#' @param y Vector of variables to plot on y-axis.
#' @param ylab Label for Y-axis. Defaults to "variable".
#' @param xlab Label for X-axis.
#' @param w Window size (number of values per bin). Defaults to 400.
#' @param s Step size (number of values to move along for each bin). Defaults to 80.
#' @param f Function to apply to y values for each bin ("mean","median","sum"). Defaults to "mean".
#' @param scale Scale the x-axis ("linear"=linear spacing of points based value of bin or "bins"=equal spacing between all bins). Defaults to "linear".
#' @param feature Name of features being binned. Defaults to "data points".
#' @param data Returns a table in long form if set to True.
#' @param samples Set the legend name. Defaults to "Samples".
#' @param labels Set the labels for samples.
#' @param split Plot samples separately.
#' @param splitscale Scales for split plots "fixed"(default),"free",free_x","free_y".
#' @param facet Variable to facet by.
#' @param sd Plot standard deviation as ribbon when plotting mean.
#' @param points Plot points at each data position
#' @keywords bin average plots.
#' @export
#' @examples
#' rollPlot()


rollPlot<-function (t, x, y, ylab = "", xlab = "", w = 400, s = 80, f = "mean", 
                    scale = "linear", feature = "data points", data = F, samples = "Samples", 
                    split = F, sd = F, cols = vector(), facet = NA, splitscale = "fixed", 
                    points = F, labels = vector()) 
{
  library(zoo)
  library(ggplot2)
  library(egg)
  library(reshape2)
  library(viridis)
  library(plyr)
  
  ##Set labels for samples
  if (length(labels) == 0) {
    labels = y
  }
  rep = F
  ##If X value also appears in Y
  if (x %in% y) {
    rep = T
    tt <- t[,y]
  }
  else{
    tt <- t[, c(x, y)]
  }
  l <- dim(tt)[1]
  
  ##Order by X value and apply rolling function
  ts <- tt[order(tt[, x]), ]
  tr <- as.data.frame(rollapply(ts, FUN = f, width = w, by = s, 
                                align = "center", na.rm = T))
  ##Set indexes for bin plots or xvalues for linear plots then melt
  tr$idx <- row.names(tr)
  if (rep) {
    tr$xval <- tr[, x]
    m <- melt(tr, id.vars = c("xval", "idx"))
  }
  else{
    m <- melt(tr, id.vars = c(x, "idx"))
  }
  ##set labels
  m$variable <- factor(m$variable, levels = y)
  m$variable <- mapvalues(m$variable, from = y, to = labels)
  m$idx <- as.numeric(as.character(m$idx))
  names(m)[1] <- "xval"
  
  ##repeat for SD calculation if set
  if (f == "mean" & sd) {
    sr <- as.data.frame(rollapply(ts, FUN = sd, width = w, 
                                  by = s, align = "center", na.rm = T))
    sr$idx <- row.names(sr)
    if (rep) {
      sr$xval <- sr[, x]
      ms <- melt(sr, id.vars = c("xval", "idx"))
    }
    else {
      ms <- melt(sr, id.vars = c(x, "idx"))
    }
    ms$variable <- factor(ms$variable, levels = y)
    ms$variable <- mapvalues(ms$variable, from = y, to = labels)
    ms$idx <- as.numeric(as.character(ms$idx))
    names(ms)[1] <- "xval"
  }
  ##return melted dfs
  if (data) {
    if (sd & f == "mean") {
      return(list(a = m, sd = ms))
    }
    else {
      return(m)
    }
  }
  ##linear plot
  if (scale != "bins") {
    p <- ggplot(m, aes_string("xval", "value", colour = "variable"))
    if (sd & f == "mean") {
      p <- p + geom_ribbon(aes(ymin = value - ms$value, 
                               ymax = value + ms$value, fill = variable), alpha = 0.5, 
                           colour = NA)
      if (length(cols) == 0) {
        p <- p + scale_fill_viridis(discrete = T, guide = F)
      }
      else {
        p <- p + scale_fill_manual(values = cols)
      }
    }
    if (points) {
      p <- p + geom_point(size = 2)
    }
    else {
      p <- p + geom_line(size = 2)
    }
    if (!feature=="NULL"){
        p <- p + ggtitle(paste(l, feature))
    }
    p <- p + theme_bw()
    if (scale == "log") {
      p <- p + scale_x_log10()
    }
    if (xlab != "") {
      p <- p + xlab(xlab)
    }
    else {
      p <- p + xlab(x)
    }
  }
  ##binned plot
  else {
    p <- ggplot(m, aes(x = idx, y = value, colour = variable, 
                       group = variable))
    if (sd & f == "mean") {
      p <- p + geom_ribbon(aes(ymin = value - ms$value, 
                               ymax = value + ms$value, fill = variable), alpha = 0.5, 
                           colour = NA)
      if (length(cols) == 0) {
        p <- p + scale_fill_viridis(discrete = T, guide = F)
      }
      else {
        p <- p + scale_fill_manual(values = cols)
      }
    }
    if (points) {
      p <- p + geom_point(size = 2)
    }
    else {
      p <- p + geom_line(size = 2)
    }
    if(!feature=="NULL"){
        p <- p + ggtitle(paste(l, feature)) 
    }
    p <- p + theme_bw()
  }

  if (ylab != "") {
    p <- p + ylab(ylab)
  }
  if (xlab != "") {
      p <- p + xlab(xlab)
  }
  else {
      p <- p + xlab(paste(x,"index"))
  }
  if (length(cols) == 0) {
    p <- p + scale_color_viridis(discrete = T, name = samples)
  }
  else {
     p <- p + scale_color_manual(values = cols, name = samples)
  }
  if (split) {
    p <- p + facet_wrap(~variable, scales = splitscale)
  }
  p
}
