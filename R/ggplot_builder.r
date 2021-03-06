#' ggplot_builder function
#'
#' This function builds a ggplot2 function to plot data
#' @param d Table of data
#' @param x,y,z Variables for each dimension
#' @param logx,logy Log the variable first. Defaults to F.
#' @param geom Select a ggplot2 geometry (currently point,line,histogram,bar,boxplot,violin)
#' @param facet Facet plot by values in a column
#' @param facet_drop Drop faceted panels without values (T)
#' @param facet_col Number of columns (0)
#' @param facet_row Number of rows (0)
#' @param smooth Add a smooth line to point plots (gam,lm,loess,rlm,glm,auto)
#' @param xlim Range displayed on x-axis
#' @param ylim Range displayed on y-axis
#' @param xrotate Angle to rotate x-axis labels (90=vertical)
#' @param colour A variable to colour by
#' @param fill A variable to fill by
#' @param alpha A variable to alpha by
#' @param text Variable to show in plotly when hover over points
#' @param labels Variable to label points by
#' @param label_display vector of labels to display
#' @param nudge_x shift label on x-axis
#' @param nudge_y shift label on y-axis
#' @param man_colour Select a solid colour
#' @param man_fill Select a solid fill colour
#' @param man_alpha Select a static alpha value
#' @param bar.position Position of bars in a bar plot (stack,dodge,fill)
#' @param bins Add a stat_bin with this number of bins
#' @param binwidth Size of binwidth in binned plots (histogram)
#' @param outliers Set outliers=F to remove outliers from boxplot
#' @param varwidth Set varwidth=T to plot boxplots with variable width based on dataset size
#' @param gradient Select gradient colour scheme (default,Matlab)
#' @param gradient.steps Set number of shades in gradient
#' @param gradient.range Set range of values covered by gradient
#' @param gradient.trans Transform gradient scale (identity,log,log10,log2,sqrt)
#' @param colourset Select colour scheme (default,Set1,Set2,Set3,Spectral)
#' @param cut_method Select method for binning continuous X axis in boxplots (number,interval,width see cut_interval etc.)
#' @param cut.n Binning number applied to cut_method
#' @param enable.plotly convert to interactive Plotly plot
#' @param theme Set ggplot theme (grey,bw,dark,light,void,linedraw,minimal,classsic)
#' @param factorlim Set maximum levels allowed to use factors for plotting (default=50)
#' @param stat.method Set stat method for barplots (count,identity,summary) (default=bin)
#' @param stat.func Set summary function for stat.method="summary" (default=mean)
#' @param coord_flip Flip the x and y axes (default=F)
#' @param tile_height Set height of tile for geom_tile
#' @param tile_width Set width of tile for geom_tile
#' @keywords ggplot wrapper builder
#' @export
#' @examples
#' ggplot_builder()


ggplot_builder<-function(d,x=NA,y=NA,geom="point",facet=NA,facet_drop=T,facet_col=T,facet_row=T,smooth=NA,smooth.se=T,xlim=NA,ylim=NA,xrotate=0,colour=NA,
                         fill=NA,alpha=NA,text=NA,labels=NA,label_display=NA,nudge_y=0,nudge_x=0,bar.position="stack",binwidth=0,bins=0,outliers=T,varwidth=F,enable.plotly=F,
                         theme="grey",logx=F,logy=F,man_colour=NA,man_fill=NA,man_alpha=NA,tile_height=NA,tile_width=NA,
                         gradient="default",gradient.trans="identity",gradient.steps=10,gradient.range=NA,colourset="default",coord_flip=F,
                         cut_method="number",cut.n=10,factorlim=50,stat.method="count",stat.func="mean"){
library(plotly)
library(colorRamps)
library(viridis)
library(ggplot2)
library(scales)
library(munsell)
library(ggthemes)
library(ggExtra)
library(RColorBrewer)

###Avoid plotting with large factors
for(i in c(facet,colour,fill,x,alpha)){
  factor_limit(d,i,factorlim)
}
if(!geom %in% c("histogram","bar") | (geom=="bar" & stat.method!="count")){ ##Check Y variable if applicable
  factor_limit(d,y,factorlim)
}
if(is.na(x)){
  x<-"1"
}
if(is.na(y)){
  y<-"1"
}  
#color palettes
vir<-viridis(gradient.steps)
blues<-brewer.pal(n=9,"Blues")
greens<-brewer.pal(n=9,"Greens")
ml<-matlab.like2(gradient.steps)
default<-mnsl(c("2.5PB 2/4", "2.5PB 7/10"))

###build plot
a<-list()
g<-list()
if(geom=="point"){
  a$x<-x
  a$y<-y
  if(!is.na(colour)){
    a$colour<-colour
  }
  if(!is.na(alpha)){
    a$alpha<-alpha
  }
  if(!is.na(text)){
    a$text<-text
  }
  if(!is.na(man_colour)){
    g$colour<-man_colour
  }
  if(!is.na(man_alpha)){
    g$alpha<-man_alpha
  }
  as<-do.call(aes_string,a)
  geo<-do.call(geom_point,g)
}
if(geom=="tile"){
  a$x<-x
  a$y<-y
  if(!is.na(fill)){
    a$fill<-fill
  }
  if(!is.na(alpha)){
    a$alpha<-alpha
  }
  if(!is.na(man_fill)){
    g$fill<-man_fill
  }
  if(!is.na(man_alpha)){
    g$alpha<-man_alpha
  }
  if(!is.na(tile_width)){
    g$width<-tile_width
  }
  if(!is.na(tile_height)){
    g$height<-tile_height
  }
  as<-do.call(aes_string,a)
  geo<-do.call(geom_tile,g)
}
if(geom=="line"){
  a$x<-x
  a$y<-y
  if(!is.na(colour)){
    a$colour<-colour
  }
  if(!is.na(alpha)){
    a$alpha<-alpha
  }
  if(!is.na(man_colour)){
    g$colour<-man_colour
  }
  if(!is.na(man_alpha)){
    g$alpha<-man_alpha
  }
  as<-do.call(aes_string,a)
  geo<-do.call(geom_line,g)
}
else if(geom=="bar"){
  if(!is.factor(d[,x])){
    stop("bar requires discrete x variable")
  }  
  a$x<-x
  if(stat.method!="count"){
    a$y<-y ##map a y aesthetic if using stat identity or summary
  }
  if(!is.na(fill)){
    a$fill<-fill
  }
  g$position<-bar.position
  if(!is.na(man_fill)){
    g$fill<-man_fill
  }
  if(!is.na(man_alpha)){
    g$alpha<-man_alpha
  }
  if(!is.na(stat.method)){
    g$stat<-stat.method
  }
  if(stat.method=="summary"){
    g$fun.y<-stat.func
  }
  as<-do.call(aes_string,a)
  geo<-do.call(geom_bar,g)
}
else if(geom=="histogram"){
  if(is.factor(d[,x])){
    stop("Histogram requires continuous x variable")
  }
  a$x<-x
  if(!is.na(fill)){
    a$fill<-fill
  }
  if(!is.na(man_fill)){
    g$fill<-man_fill
  }
  if(!is.na(man_alpha)){
    g$alpha<-man_alpha
  }
  if(binwidth>0){
    g$binwidth<-binwidth
  }
  if(bins>0){
    g$bins<-bins
  }
  as<-do.call(aes_string,a)
  geo<-do.call(geom_histogram,g)
}
else if(geom=="boxplot"){
  if(!is.numeric(d[,y])){
    stop("Boxplot requires continuous y variable")
  }
  a$x<-x
  a$y<-y
  if(!is.na(fill)){
    a$fill<-fill
  }
  if(!is.na(alpha)){
    a$alpha<-alpha
  }
  if(!is.na(man_fill)){
    g$fill<-man_fill
  }
  if(!is.na(man_alpha)){
    g$alpha<-man_alpha
  }
  if(!is.na(colour)){
    a$colour<-colour
  }
  if(!is.na(man_colour)){
    g$colour<-man_colour
  }
  if(x!="1"){ ##if x is set and numeric apply a group function
    if(is.numeric(d[,x])){
      cut<-switch(cut_method,interval=cut_interval(d[,x],n = cut.n),width=cut_width(d[,x],width=cut.n),number=cut_number(d[,x],n = cut.n))
      a$group<-cut
    }
  }
  if(outliers==F){
    g$outlier.shape<-NA
  }
  if(varwidth==T){
    g$varwidth<-T
  }
  as<-do.call(aes_string,a)
  geo<-do.call(geom_boxplot,g)
}
else if(geom=="violin"){
  if(!is.numeric(d[,y])){
    stop("Violin requires continuous y variable")
  }
  a$x<-x
  a$y<-y
  if(!is.na(fill)){
    a$fill<-fill
  }
  if(!is.na(man_fill)){
    g$fill<-man_fill
  }
  if(!is.na(colour)){
    a$colour<-colour
  }
  if(!is.na(man_colour)){
    g$colour<-man_colour
  }
  if(!is.na(alpha)){
    a$alpha<-alpha
  }
  if(!is.na(man_alpha)){
    g$alpha<-man_alpha
  }
  if(x!="1"){ ##if x is set and numeric apply a group function
    if(is.numeric(d[,x])){
      cut<-switch(cut_method,interval=cut_interval(d[,x],n = cut.n),width=cut_width(d[,x],width=cut.n),number=cut_number(d[,x],n = cut.n))
      a$group<-cut
    }
  }
  as<-do.call(aes_string,a)
  geo<-do.call(geom_violin,g)
}
p<-ggplot(d,as)+geo
if(!is.na(facet)){
  #if(is.factor(d[,facet])&length(levels(d[,facet]))<=factorlim){
    p<-p+facet_wrap(facet,drop=facet_drop,nrow = facet_row,ncol = facet_col)
  #}
  #else{
  #  stop(paste("You must facet by a factor variable with <=",factorlim,"levels"))
  #}  
}
if(!is.na(smooth) & geom %in% c("point")){
  s<-list()
  if(!is.na(fill)){
    s$fill<-fill
  }
  if(!is.na(colour)){
    s$colour<-colour
  }
  if(smooth.se==F){
    s$se<-F
  }
  statas<-do.call(aes_string,s)
  p<-p+stat_smooth(method=smooth,statas)
}
if(x!="1"){
  if(!is.na(xlim) & is.numeric(d[,x])){
    p<-p+xlim(xlim)
  }
}
if(!is.na(ylim) & y!="1"){
  if(!is.null(a$y)){#if y aesthetic exists
    if(is.numeric(d[,y])){ #if y aestheitc is numeric
      p<-p+ylim(ylim)
    }
  }
  else{
    p<-p+ylim(ylim)
  }
}
if(x!="1"){
  if(logx & is.numeric(d[,x])){ #####!!!!!!!!!!
    p<-p+scale_x_log10()
  }
}
if(logy & y!="1"){
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

if(!is.na(labels)){
  if(is.na(label_display)){
    label_display<-d[,labels]
  }
  labs=ifelse(d[,labels] %in% label_display,as.character(d[,labels]),'')
  p<-p+geom_text(aes(label=labs),nudge_y=nudge_y,nudge_x=nudge_x)
}

##set colour scales
if(!is.na(colour)){
  if(is.factor(d[,colour])){
    p<-switch(colourset,default=p,Set1=p+scale_colour_brewer(palette="Set1"),
              Set2=p+scale_colour_brewer(palette="Set2"),
              Set3=p+scale_colour_brewer(palette="Set3"),
              Spectral=p+scale_colour_brewer(palette="Spectral"),
              Viridis=p+scale_colour_viridis(discrete=T))
  }
  else{
    if(!is.na(gradient.range)){
      p<-switch(gradient,default=p+scale_colour_gradientn(colours=default,limits=gradient.range,oob = scales::squish,space="Lab",trans=gradient.trans),
                Matlab=p+scale_colour_gradientn(space = "Lab",limits = gradient.range,oob = scales::squish,colours=ml,trans=gradient.trans),
                Blues=p+scale_colour_gradientn(space = "Lab",limits = gradient.range,oob = scales::squish,colours=blues,trans=gradient.trans),
                Greens=p+scale_colour_gradientn(space = "Lab",limits = gradient.range,oob = scales::squish,colours=greens,trans=gradient.trans),
                Viridis=p+scale_colour_viridis(space = "Lab",limits = gradient.range,oob = scales::squish,trans=gradient.trans)
                )
    }
    else{
      p<-switch(gradient,default=p+scale_colour_gradientn(colours=default,trans=gradient.trans),
                Matlab=p+scale_colour_gradientn(colours=ml,trans=gradient.trans),
                Blues=p+scale_colour_gradientn(colours=blues,trans=gradient.trans),
                Greens=p+scale_colour_gradientn(colours=greens,trans=gradient.trans),
                Viridis=p+scale_colour_viridis(trans=gradient.trans))
    }
  }
}
if(!is.na(fill)){
  if(is.factor(d[,fill])){
    p<-switch(colourset,default=p,Set1=p+scale_fill_brewer(palette="Set1"),
              Set2=p+scale_fill_brewer(palette="Set2"),
              Set3=p+scale_fill_brewer(palette="Set3"),
              Spectral=p+scale_fill_brewer(palette="Spectral"),
              Viridis=p+scale_fill_viridis(discrete=T))
  }
  else{
    if(!is.na(gradient.range)){
      p<-switch(gradient,default=p+scale_fill_gradientn(colours=default,limits=gradient.range,oob = scales::squish,space="Lab",trans=gradient.trans),
                Matlab=p+scale_fill_gradientn(space = "Lab",limits = gradient.range,oob = scales::squish,colours=ml,trans=gradient.trans),
                Blues=p+scale_fill_gradientn(space = "Lab",limits = gradient.range,oob = scales::squish,colours=blues,trans=gradient.trans),
                Greens=p+scale_fill_gradientn(space = "Lab",limits = gradient.range,oob = scales::squish,colours=greens,trans=gradient.trans),
                Viridis=p+scale_fill_viridis(space = "Lab",limits = gradient.range,oob = scales::squish,trans=gradient.trans)
                )
    }
    else{
      p<-switch(gradient,default=p+scale_colour_gradientn(colours=default,trans=gradient.trans),
                Matlab=p+scale_fill_gradientn(colours=ml,trans=gradient.trans),
                Blues=p+scale_fill_gradientn(colours=blues,trans=gradient.trans),
                Greens=p+scale_fill_gradientn(colours=greens,trans=gradient.trans),
                Viridis=p+scale_fill_viridis(trans=gradient.trans)
                )
    }  }
}
if(coord_flip){
  p<-p+coord_flip()
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
