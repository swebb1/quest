#' ggplot_builder function
#'
#' This function builds a ggplot2 function to plot data
#' @param d Table of data
#' @param x,y,z Variables for each dimension
#' @param logx,logy Log the variable first. Defaults to F.
#' @param geom Select a ggplot2 geometry (currently point,line,histogram,bar,boxplot,violin)
#' @param facet Facet plot by values in a column
#' @param smooth Add a smooth line to point plots (gam,lm,loess,rlm,glm,auto)
#' @param xlim Range displayed on x-axis
#' @param ylim Range displayed on y-axis
#' @param xrotate Angle to rotate x-axis labels (90=vertical)
#' @param colour A variable to colour by
#' @param fill A variable to fill by
#' @param bar.position Position of bars in a bar plot (stack,dodge,fill)
#' @param bins Number of bins to use in a binned plot (histogram)
#' @param outliers Set outliers=F to remove outliers from boxplot
#' @param enable.plotly convert to interactive Plotly plot
#' @param theme Set ggplot theme (grey,bw,dark,light,void,linedraw,minimal,classsic)
#' @keywords tile plots
#' @export
#' @examples
#' ggplot_builder()


ggplot_builder<-function(d,x,y,z,geom="point",facet=NA,smooth=NA,xlim=NA,ylim=NA,xrotate=0,colour=NA,
                         fill=NA,bar.position="stack",bins=0,outliers=T,enable.plotly=F,
                         theme="grey",logx=F,logy=F){
library(plotly)  
if(geom=="point"){
  if(is.na(colour)){
    p<-ggplot(d,aes_string(x=x,y=y)) + geom_point()
  }
  else{
    p<-ggplot(d,aes_string(x=x,y=y)) + geom_point(aes_string(colour=colour))
  }
}
if(geom=="line"){
  if(is.na(colour)){
    p<-ggplot(d,aes_string(x=x,y=y)) + geom_line()
  }
  else{
    p<-ggplot(d,aes_string(x=x,y=y)) + geom_line(aes_string(colour=colour))
  }
}
else if(geom=="bar"){
  if(is.na(fill)){
    p<-ggplot(d,aes_string(x=x))+geom_bar(position = bar.position)
  }
  else{
    p<-ggplot(d,aes_string(x=x,fill=fill))+geom_bar(position=bar.position)
  }
}
else if(geom=="histogram"){
  if(is.na(fill)){
    p<-ggplot(d,aes_string(x=x))+geom_histogram()
  }
  else{
    p<-ggplot(d,aes_string(x=x,fill=fill))+geom_histogram()
  }
  if(bins>0){
    p<-p+stat_bin(bins=bins)
  }
}
else if(geom=="boxplot"){
    if(is.na(fill)){
      p<-ggplot(d,aes_string(x=x,y=y))
    }
    else{
      p<-ggplot(d,aes_string(x=x,y=y,fill=fill))
    }
    if(outliers==T){
      p<-p+geom_boxplot()
    }
    else{
      p<-p+geom_boxplot(outlier.shape=NA)
    }
}
else if(geom=="violin"){
  if(is.na(fill)){
    p<-ggplot(d,aes_string(x=x,y=y))+geom_violin()
  }
  else{
    p<-ggplot(d,aes_string(x=x,y=y,fill=fill))+geom_violin()
  }
}
if(!is.na(facet)){
  p<-p+facet_wrap(c(facet))
}
if(!is.na(smooth) & geom %in% c("point")){
  if(!is.na(facet)){
    p<-p+geom_smooth(method = smooth,aes_string(colour=facet,fill=facet)) 
  }
  else{
    p<-p+geom_smooth(method = smooth)
  }
}
if(!is.na(xlim)){
  p<-p+xlim(xlim)
}
if(!is.na(ylim)){
  p<-p+ylim(ylim)
}
p<-switch(theme,grey=p+theme_grey(),dark=p+theme_dark(),light=p+theme_light(),linedraw=p+theme_linedraw(),bw=p+theme_bw(),minimal=p+theme_minimal(),classic=p+theme_classic(),void=p+theme_void(),p+theme_grey())
if(xrotate!=0){
  p<-p+theme(axis.text.x=element_text(angle=xrotate,hjust=1,vjust=0.5))
}
if(logy){
  p<-p+ scale_y_log10()
}
if(logx){
  p<-p+ scale_x_log10()
}
#p<-p + scale_colour_brewer(palette="Set1") + scale_fill_brewer(palette="Set1")
if(enable.plotly){
  return(ggplotly(p))
}
p
}

#+  scale_fill_brewer(palette="Set1") + scale_colour_brewer(palette="Set1")
#scale_colour_gradientn(colours=rainbow(4))
#matlab_like........

#ggplot_builder(t,"biotype","CpGdensity",geom="",xvert=T,bar.position="stack",
#               theme="dark",bins=20,fill="biotype",logy=F,outliers=F) 


#ggplot(t,aes_string("biotype","length",colour="biotype",fill="OE_direction"))+ geom_boxplot()


#bar.position = stack, dodge or fill


#ggplot_builder(d=t,x="GC",y="length",z="length",logx=F,logy=F,facet="NA",
#               geom="histogram",smooth="NA",xrotate=0,colour="NA",
#               fill=NA,bar.position = "stack",theme = "light",
#               enable.plotly = F,outliers=T,bins = 0,
#               xlim="NA",ylim="NA")
