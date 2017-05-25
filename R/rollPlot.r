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
#' @keywords bin average plots.
#' @export
#' @examples
#' rollPlot()


rollPlot<-function(t,x,y,ylab="",xlab="",w=400,s=80,f="mean",
                   scale="linear",feature="data points",data=F,samples="Samples",facet=F,sd=F){
  
  library(zoo) #for rollapply function
  library(ggplot2)
  library(egg)
  library(reshape2)
  library(viridis)
  
  tt<-t[,c(x,y)]
  l<-dim(tt)[1]
  ts<-tt[order(tt[,x]),]
  tr<-as.data.frame(rollapply(ts,FUN=f,width=w,by=s,align="center",na.rm=T))
  tr$idx<-row.names(tr)
  m<-melt(tr,id.vars=c(x,"idx"))
  m$variable<-factor(m$variable,levels=y)
  m$idx<-as.numeric(as.character(m$idx))
  if(f=="mean" & sd){
    sr<-as.data.frame(rollapply(ts,FUN=sd,width=w,by=s,align="center",na.rm=T))
    sr$idx<-row.names(sr)
    ms<-melt(sr,id.vars=c(x,"idx"))
    ms$variable<-factor(ms$variable,levels=y)
    ms$idx<-as.numeric(as.character(ms$idx))
  }
  
  if(data){
    if(sd & f=="mean"){
      return(list(a=m,sd=ms))
    }
    else{
      return(m)
    }
  }
  if(scale!="bins"){
    p<-ggplot(m,aes_string(x,"value",colour="variable"))
    if(sd & f=="mean"){
      p<-p+geom_ribbon(aes(ymin = value - ms$value , ymax = value + ms$value, fill = variable),alpha=0.5,colour=NA)+
      scale_fill_viridis(discrete=T,guide=F)
    }
    p<-p+geom_line(size=2)+ggtitle(paste(l,feature))+theme_bw()
    if(scale=="log"){
      p<-p+scale_x_log10()
    }
    if(xlab!=""){
      p<-p+xlab(xlab)
    }
  }
  else{
    p<-ggplot(m,aes(x=idx,y=value,colour=variable,group=variable))+geom_line(size=2)+ggtitle(paste(l,feature))+theme_bw()
  }
  if(ylab!=""){
    p<-p+ylab(ylab)
  }
  p<-p+scale_color_viridis(discrete=T,name=samples)
  if(facet){
    p<-p+facet_wrap(~variable)
  }
  p
}

