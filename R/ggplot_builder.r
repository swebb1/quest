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
#' @param man_colour Select a solid colour
#' @param man_fill Select a solid fill colour
#' @param bar.position Position of bars in a bar plot (stack,dodge,fill)
#' @param bins Add a stat_bin with this number of bins
#' @param binwidth Size of binwidth in binned plots (histogram)
#' @param outliers Set outliers=F to remove outliers from boxplot
#' @param enable.plotly convert to interactive Plotly plot
#' @param theme Set ggplot theme (grey,bw,dark,light,void,linedraw,minimal,classsic)
#' @keywords tile plots
#' @export
#' @examples
#' ggplot_builder()


ggplot_builder<-function(d,x,y=NA,z=NA,geom="point",facet=NA,smooth=NA,xlim=NA,ylim=NA,xrotate=0,colour=NA,
                         fill=NA,bar.position="stack",binwidth=0,bins=0,outliers=T,enable.plotly=F,
                         theme="grey",logx=F,logy=F,man_colour=NA,man_fill=NA){
library(plotly)  
a<-list()
g<-list()
if(geom=="point"){
  a$x<-x
  a$y<-y
  if(!is.na(colour)){
    a$colour<-colour
  }
  if(!is.na(man_colour)){
    g$colour<-man_colour
  }
  as<-do.call(aes_string,a)
  geo<-do.call(geom_point,g)
}
if(geom=="line"){
  a$x<-x
  a$y<-y
  if(!is.na(colour)){
    a$colour<-colour
  }
  if(!is.na(man_colour)){
    g$colour<-man_colour
  }
  as<-do.call(aes_string,a)
  geo<-do.call(geom_line,g)
}
else if(geom=="bar"){
  a$x<-x
  if(!is.na(fill)){
    a$fill<-fill
  }
  g$position<-bar.position
  if(!is.na(man_fill)){
    g$fill<-man_fill
  }
  as<-do.call(aes_string,a)
  geo<-do.call(geom_bar,g)
}
else if(geom=="histogram"){
  a$x<-x
  if(!is.na(fill)){
    a$fill<-fill
  }
  if(!is.na(man_fill)){
    g$fill<-man_fill
  }
  if(binwidth>0){
    g$binwidth<-binwidth
  }
  as<-do.call(aes_string,a)
  geo<-do.call(geom_histogram,g)
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
p<-ggplot(d,as)+geo
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
if(bins>0 & geom %in% c("histogram","bar") & is.numeric(d[,x])) {
  p<-p+stat_bin(bins=bins)
}
if(!is.na(xlim) & is.numeric(d[,x])){
  p<-p+xlim(xlim)
}
if(!is.na(ylim)){
  if(!is.null(a$y)){#if y aesthetic exists
    if(is.numeric(d[,y])){ #if y aestheitc is numeric
      p<-p+ylim(ylim)
    }
  }
  else{
    p<-p+ylim(ylim)
  }
}
if(logx & is.numeric(x)){
  p<-p+ scale_x_log10()
}
if(logy){
  if(!is.null(a$y)){ #if y aesthetic exists
    if(is.numeric(d[,y])){ #if y aestheitc is numeric
      p<-p+ scale_y_log10()
    }
  }
  else{
    p<-p+ scale_y_log10()
  }
}
p<-switch(theme,grey=p+theme_grey(),dark=p+theme_dark(),light=p+theme_light(),linedraw=p+theme_linedraw(),bw=p+theme_bw(),minimal=p+theme_minimal(),classic=p+theme_classic(),void=p+theme_void(),p+theme_grey())
if(xrotate!=0){
  p<-p+theme(axis.text.x=element_text(angle=xrotate,hjust=1,vjust=0.5))
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
