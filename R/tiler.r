#' tiler function
#'
#' This function plots 3 variables on an x,y and coloured z scale. Values on the x and y scale are tiled
#' and the colour of the tile is determined by the function applied to z (mean,median,count etc.).
#' @param t Table of data
#' @param x,y,z Variables for each dimension
#' @param xl,yl,zl Log the variable first. Defaults to F.
#' @param xs,ys,zs Scale the variable first. Defaults to 1.
#' @param bin Size of each tile
#' @param zrange Range of the colour scale. 
#' @param xrange Range displayed on x-axis. 
#' @param yrange Range displayed on y-axis.
#' @param cols Vector of colours for colour range. Defaults to colorRamps::matlab.like2(10)
#' @param func Function applied to tiled z variables ("median","mean","sum","count"). Defaults to median.
#' @param smooth Add a smoothing line, options are lm,glm,gam,loess,rlm. Off by default.
#' @keywords tile plots
#' @export
#' @examples
#' tiler()

tiler<-function(t,x,xl=F,xs=1,y,yl=F,ys=1,z,zl=F,zs=1,bin=1,zrange=NA,xrange=NA,yrange=NA,func="median",col=NA,smooth=NA){
  library(bigvis)
  if(is.na(col)){
    library(colorRamps)
    col<-matlab.like2(10)
  }
  
  if(xl){t[,x]<-log(t[,x])}
  if(yl){t[,y]<-log(t[,y])}
  if(zl & func !="count"){t[,z]<-log(t[,z])}
  tab<-condense(bin(t[,x]*xs,bin),bin(t[,y]*ys,bin),z = t[,z]*zs,summary = func)
  if(func=="mean"){
    names(tab)<-c(paste(x,".x",sep=""),paste(y,".y",sep=""),"count",paste(z,".",func,sep=""))
  }
  else{
    names(tab)<-c(paste(x,".x",sep=""),paste(y,".y",sep=""),paste(z,".",func,sep=""))
  }
  if(func=="count" & zl){tab[,paste(z,".",func,sep="")]<-log(tab[,paste(z,".",func,sep="")])}
  p<-ggplot(tab,aes_string(paste(x,".x",sep=""),paste(y,".y",sep="")))
  ss<-paste(z,".",func,sep="")
  pp<-NULL
  pp<-p + geom_tile(aes_string(fill=ss))
  if(!is.na(zrange)){
    pp<-pp+ scale_fill_gradientn(space = "Lab",limits = zrange,oob = scales::squish,colours=col)
  }
  else{
    pp<-pp+ scale_fill_gradientn(space = "Lab",oob = scales::squish,colours=col)
  }
  if(!is.na(xrange)){
    pp<-pp+xlim(xrange)
  }
  if(!is.na(yrange)){
    pp<-pp+ylim(yrange)
  }
  if(!is.na(smooth)){
    pp<-pp + stat_smooth(method=smooth, se=T, colour="black",size=1)
  }
  return(pp)
}  