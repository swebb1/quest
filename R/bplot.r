#' bplot rolling average plotting function
#'
#' This function orders and bins a variable in to groups defined by a window 
#' and step size then computes rolling averages for multipe variables across each bin.
#' @param t Table of data.
#' @param x Variable to be binned on x-axis.
#' @param y Vector of variables to plot on y-axis.
#' @param ys Scale the y-axis variables first. Defaults to 1.
#' @param ystep Division of steps printed on y-axis scale. Defaults to 0.2.
#' @param ylab Label for Y-axis. Defaults to blank.
#' @param axis3 Column of length variable to be plotted on 3rd axis. Defaults to "NA" (not shown).
#' @param w Window size (number of values per bin). Defaults to 400.
#' @param s Step size (number of values to move along for each bin). Defaults to 80.
#' @param f Function to apply to y values for each bin ("mean","median","sum"). Defaults to "mean".
#' @param scale Scale the x-axis ("linear"=linear spacing of points based on mean vaue of bin or "bins"=equal spacing between all bins). Defaults to "linear".
#' @param leg Postion of legend. Defaults to "topleft".
#' @param col Vector of colours to use in plots. Defaults to 1:100.
#' @param max,min Minimum and Maximum range of y-axis.
#' @param feature Name of features being binned. Defaults to "data points".
#' @keywords bin average plots.
#' @export
#' @examples
#' bplot()


bplot<-function(t,x,y,ys=1,ystep=0,ylab="",axis3="NA",w=400,s=80,f="mean",
                scale="linear",leg="topleft",col=1:100,max="default",min="default",feature="data points"){
  library(zoo) #for rollapply function
  
  t<-t[order(t[,x]),] #order x-axis
  l<-length(y) #number of lines to plot
  #remove NAs
  for(i in 1:l){
    t<-t[!is.na(t[,y[i]]),] 
    #scale the y-axes
    if(ys!=1){
      t<-t*ys
    }
  }
  xb<-t[,x] #get binning column
  if(scale=="log"){ #log binninng values if requested
    xb<-log(xb)
    x<-paste0("log(",x,")")
  }
  xbins<-rollapply(xb, width=w, by=s,FUN="mean", align="left") #bin and mean
  ybins<-NULL
  boxlist<-NULL
  ##for y variables bin and calculate avg/boxes
  for(i in 1:l){
    yb<-t[,y[i]]
    if(f=="boxes"){
      box<-rollapply(yb, width=w, by=s,FUN=function(b){b}, align="left")
      bins<-rollapply(yb, width=w, by=s,FUN="median", align="left")
      box<-t(box)
      ybins<-cbind(ybins,bins)
      boxlist[[i]]<-box
    }
    else{
      bins<-rollapply(yb, width=w, by=s,FUN=f, align="left") 
      ybins<-cbind(ybins,bins)
    }
  }
  #set yscale
  if(min=="default"){
    min<-min(ybins)
  }  
  if(max=="default"){
    max<-max(ybins)
  }
  #plot boxplots
  if(f=="boxes"){
    for(i in 1:l){
      for(j in 1:dim(boxlist[[i]])[2]){
        min<-min(min,quantile(boxlist[[i]][,j],0.25))
        max<-max(max,quantile(boxlist[[i]][,j],0.75))    
      }
    }  
    par(mar=c(5,5,5,5))
    sc<-xbins
    xlim<-c(min(sc),max(sc))
    #plot equal spaces between boxes
    if(scale=="bins"){
      sc<-1:length(xbins)
      xlim<-c(1,length(xbins))
    }
    boxplot(boxlist[[1]],outline=F,col="forest green",staplelty=0,whisklty=0,boxcol="forest green",medlty=0,axes=F,
            ylab="",mar=20,ylim=c(min,max),names="",xlim=xlim,main=paste(length(xb),feature),xlab=x,at=sc,boxwex=0.001)
    lines(sc,ybins[,1],col="dark grey",lwd=2)    
    #insert color pallete
    if(l>1){
      for(i in 2:l){
        boxplot(boxlist[[i]],add=T,outline=F,col=i,staplelty=0,whisklty=0,boxcol=i,medlty=0,axes=F,xlab="",
                ylab="",mar=20,ylim=c(min,max),names="",main="",at=sc,boxwex=0.001)
        lines(sc,ybins[,i],col="dark grey",lwd=2)
      }
    }
  }
  #if line plots
  else{
    par(mar=c(5,5,5,5))
    sc<-xbins
    #plot equal spacing between bins
    if(scale=="bins"){
      sc<-1:length(xbins)
    }
    #plot line 1
    plot(sc,ybins[,1],col=col[1],axes=F,ylab=ylab,type="l",xlab=x,
         main=paste(length(xb),feature),ylim=c(min,max),lwd=2)
    #plot grid
    grid(nx = NULL, ny = NULL, col = "grey", lty = 1,lwd = 1, equilogs = TRUE)
    #plot other lines
    if(l>1){
      for(i in 2:l){
        lines(sc,ybins[,i],col=col[i],lwd=2)
      }
    }
  }
  ##adjust axes
  if(ystep==0){
    axis(2)
  }
  else{
    axis(2,at=seq(0,max,ystep),labels=seq(0,max,ystep),las=1)
  }
  if(scale=="linear" | scale=="log"){
    axis(1,at=xbins,labels=format(round(xbins, 2), nsmall = 2),las=1)
  }
  else{
    axis(1,at=c(seq(1,length(xbins),(length(xbins)/10)),length(xbins)),labels=format(round(c(xbins[seq(1,length(xbins),(length(xbins)/10),)],xbins[length(xbins)]), 2), nsmall = 2),las=2)
  }
  if(axis3!="NA"){
    lbins<-rollapply(t[,axis3], width=w, by=s,FUN="mean", align="left")
    par(new=T)
    plot(sc,lbins,col="grey",axes=F,ylab="",type="l",lty=2,lwd=2,xlab="")
    axis(4)  
    if(leg != "off"){
      legend(x=leg,legend=c(y,axis3),lty=1,lwd=2,col=c(col[1:l],"grey"),box.lwd=0,bty="n")
    }
    mtext(axis3,side=4,line=2)
  }
  else{
    if(leg != "off"){
      legend(x=leg,legend=c(y),lty=1,lwd=2,col=c(col),box.lwd=0,bty="n")}
  }
}



