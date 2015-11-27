library(shiny)
library(zoo)
library(ggplot2)
library(bigvis)
library(corrplot)
library(colorRamps)

#set up the directory to extract datafiles  from
path <- "/homes/swebb/data/tables"
#read files in directory 
myfiles <- list.files(path=path)
#myfiles <- grep ##Work out how to only select certain files
#reconstruct full path name for each file 
temp = paste(path,myfiles, sep='/')
##read the contents of each file, and assign too a data.frame with the same name as the filename
for (i in 1:length(myfiles)) assign(myfiles[i], read.table(temp[i],header=T))

#df<-read.table("/homes/swebb/data/published//PNAS_2015_Zhogbi_GSE66870//deseq/Zhogbi_master_table_deseq_results.tab",header=T)

shinyServer(function(input, output,session) {

  ######plot functions######
  ##binned plot function
  bplot<-function(t,x,y,w=400,s=80,f="mean",scale="linear"){
    withProgress(message="Plotting...",value=0,{
    t<-t[order(t[,x]),] #order x-axis
    l<-length(y) #number of lines to plot
    #remove NAs
    for(i in 1:l){
      t<-t[!is.na(t[,y[i]]),]
    }
    xb<-t[,x]
    if(scale=="log"){
      xb<-log(xb)
    }
    xbins<-rollapply(xb, width=w, by=s,FUN="mean", align="left")
    ybins<-NULL
    boxlist<-NULL
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
    min<-min(ybins)
    max<-max(ybins)
    if(f=="boxes"){
      for(i in 1:l){
        for(j in 1:dim(boxlist[[i]])[2]){
          min<-min(min,quantile(boxlist[[i]][,j],0.25))
          max<-max(max,quantile(boxlist[[i]][,j],0.75))    
        }
      }  
      par(mar=c(5,5,5,5))
      sc<-xbins
      if(scale=="bins"){
        sc<-1:length(xbins)
      }
      boxplot(boxlist[[1]],outline=F,col="forest green",staplelty=0,whisklty=0,boxcol="forest green",medlty=0,axes=F,xlab=x,
              ylab="",mar=20,ylim=c(min,max),names="",main=paste(length(xb),"points"),at=sc,boxwex=0.001)
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
    else{
      par(mar=c(5,5,5,5))
      sc<-xbins
      if(scale=="bins"){
        sc<-1:length(xbins)
      }
      plot(sc,ybins[,1],col="forest green",axes=F,ylab="",type="l",xlab=x,
           main=paste(length(xb),"points"),ylim=c(min,max),lwd=2)
      #insert color pallete
      if(l>1){
        for(i in 2:l){
          lines(sc,ybins[,i],col=i,lwd=2)
        }
      }
    }
    axis(2)
    if(scale=="linear" | scale=="log"){
      axis(1,at=xbins,labels=format(round(xbins, 1), nsmall = 2))
    }
    else{
      axis(1,at=c(seq(1,length(xbins),(length(xbins)/10)),length(xbins)),labels=format(round(c(xbins[seq(1,length(xbins),(length(xbins)/10),)],xbins[length(xbins)]), 2), nsmall = 2),las=2)
    }
    legend(xbins[2],max,legend=y,lty=rep(1,l),lwd=rep(1,l),col=c("forest green",2:l))
    })
  }
  
  ##Tile plot function
  tiler<-function(t,x,xl=F,xs=1,y,yl=F,ys=1,z,zl=F,zs=1,bin=1,min=-5,max=5,xrange=c(-100,100),yrange=c(-100,100),scale="zero",func="median"){
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
    pp<-NULL;
    if(scale=="zero"){
      pp<-p + geom_tile(aes_string(fill=ss)) + scale_fill_gradient2(space = "Lab",limits=c(min,max),oob = scales::squish,low="red",mid="white",high="dodger blue") + xlim(xrange) + ylim(yrange)
    }
    else if(scale=="rainbow"){ 
        hmcol<-matlab.like2(10)
        pp<-p + geom_tile(aes_string(fill=ss))+ scale_fill_gradientn(space = "Lab",limits = c(min,max),oob = scales::squish,colours=hmcol)+xlim(xrange)+ylim(yrange)
    }
#    pp<-pp + stat_smooth(se=T, colour="black",size=1)
    return(pp)
  }  
  
  ######Code######
  
  ##select file
  output$fileUI <- renderUI({
    tagList(
      selectInput("file",  "Select File:", myfiles )
    )
  })
    
  ##table data
  filter<-function(fstring,ignore=FALSE){
#    if(input$add != ""){ ##add columns if adds exist
#      a<-strsplit(input$add,",")
#      for(i in 1:length(a[[1]])){
#        a2<-strsplit(a[[1]][i],":")
#        a3<-paste(a2[[1]][1],"<-",input$file,"[,'",a2[[1]][2],"']",a2[[1]][3],sep="")
#        if(length(a2[[1]])==4){
#          a3<-paste(a3,input$file,"[,'",a2[[1]][4],"']",sep="")
#        }
#        eval(parse(text=a3))
#        a4<-paste(input$file,"<-cbind(",input$file,",",a2[[1]][1],")",sep="")
#        eval(parse(text=a4))
#      }
#    }
    fdf<-get(input$file) ##get the object input file
    withProgress(message="Filtering...",value=0,{
    if(input$addFilt=="Apply Filters" | ignore){ ##filter table if filters exist
        f<-strsplit(fstring,",")
        for(i in 1:length(f[[1]])){
          f2<-strsplit(f[[1]][i],":")
          f3<-paste("fdf<-subset(fdf,fdf[,'",f2[[1]][1],"']",f2[[1]][2],")",sep="")
          eval(parse(text=f3))
        }
    }
    })
    fdf
  }
  
  ##output table
  output$table = renderDataTable({
    fdf<-filter(input$filts)
    fdfc<-fdf[, input$show_vars, drop = FALSE]  ##show selected columns
    if(dim(fdfc)[2]==0){ ##If no columns selected show full table. Needs Fixing!!!!
      return(fdf)
    }
    else{
      return(fdfc)
    }
  },options = list(bSortClasses = TRUE,aLengthMenu = c(5,10,20,50,100), iDisplayLength = 10) )
  
  ##table controls
  output$show_cols<-renderUI({
    if(is.null(input$file)){return(NULL)}
    #   fdf<-filter(input$filts)
    fdf<-get(input$file)
    items=names(fdf)
    tagList(
      checkboxGroupInput('show_vars', 'Columns to show:', items, selected = items)
    )
  })
  #outputOptions(output, 'show_cols', suspendWhenHidden=FALSE)
  
  ##Observe check box input for select/deselect all columns
  observe({
    if(is.null(input$file)){return(NULL)}
    fdf<-get(input$file)
    if (input$show_all){
        updateCheckboxGroupInput(
          session, 'show_vars','Columns to show:', choices = names(fdf),
          selected = names(fdf)
        )
    }
    else{
        updateCheckboxGroupInput(
          session, 'show_vars','Columns to show:', choices = names(fdf),
          selected = NULL
        )
    }  
  })

 ##1d plot controls
 output$plot_cols <- renderUI({
   if(is.null(input$file)){return(NULL)}
#   fdf<-filter(input$filts)
   fdf<-get(input$file)
   items=names(fdf)
   tagList(
     selectInput("x", "Column to plot",items,multiple=T)
   )
 })
  
 ##1d plot
 output$plot <- renderPlot({ 
   par(mar=c(10,5,5,5))
   withProgress(message="Plotting...",value=0,{
   fdf<-filter(input$filts)
   if(is.numeric(fdf[,input$x[1]]) & input$off){ ##Only plot if numeric column selected
     if(input$type=="boxplot"){
       ##plot rival boxplots based on a filter
       if(input$bversus!=""){
         bfilts<-input$bversus
         bfdf<-filter(bfilts,TRUE)
         bfdf<-cbind("filter2",bfdf)
         colnames(bfdf)[1]<-"filter"
         fdf<-cbind("filter1",fdf)
         colnames(fdf)[1]<-"filter"
         bfdf<-rbind(fdf,bfdf)
         ggplot(data = melt(bfdf[,c("filter",input$x)]), aes(x=variable, y=value)) + geom_boxplot(aes(fill=factor(filter)))
       }
       else{
         boxplot(fdf[,input$x],outline = F,col="dodger blue",names = input$x,main="",las=2)
       }
    }
     else if(input$type=="histogram"){
       hx<-fdf[,input$x]
       if(input$hlogx){
         hx<-log(hx) ##log x axis
       }
       if(input$breaks==0){
         hist(hx,xlab = input$x,col="firebrick",main="") ##default breaks
       }
       else{
         hist(hx,xlab = input$x,breaks=input$breaks,col="firebrick",main="") ##custom breaks
       }
     }
   }
   })
 },height=600,width=800)
  
 ##2d plot cols
 output$dplot_cols <- renderUI({
   if(is.null(input$file)){return(NULL)}
   #   fdf<-filter(input$filts)
   fdf<-get(input$file)
   items=names(fdf)
   tagList(
     selectInput("dx", "Column to plot",items),
     selectInput("dy", "Column to plot",items)
   )
 })
 
   ##2d plot
   output$dplot <- renderPlot({ 
     withProgress(message="Plotting...",value=0,{
     fdf<-filter(input$filts)
     if(is.numeric(fdf[,input$dx]) & is.numeric(fdf[,input$dy]) & input$off){
       par(pch=".")
       x<-fdf[,input$dx]
       y<-fdf[,input$dy]
       if(input$logx){
         x<-log(x) ##log x axis
       }
       if(input$logy){
         y<-log(y) ##log y axis
       }
       if(input$dtype=="scatter"){
        plot(x,y,main="",xlab=input$dx,ylab=input$dy,col="dodger blue")
        #abline(lm(y~x),col="red")
       }
       else if(input$dtype=="smoothScatter"){
        smoothScatter(x,y,main="",xlab=input$dx,ylab=input$dy)
        #abline(lm(y~x),col="red") ##need a better line here than lm, also doesn't work with NA/INF etc
       }
       ##highlight points in plot based on a filter
       if(input$hilite!=""){
         hfilts<-input$hilite
         #if(input$filts!="" & input$addFilt=="Apply Filters"){ ##don't combine with main filter for now
           #hfilts<-paste(input$filts,input$hilite,sep=",")
         #}
         hfdf<-filter(hfilts,TRUE)
         hx<-hfdf[,input$dx]
         hy<-hfdf[,input$dy]
         if(input$logx){
           hx<-log(hx)
         }
         if(input$logy){
           hy<-log(hy)
         }
         points(hx,hy,col="green")
       }
     }
     })
   },height=600,width=800)
  
  
  ##bin plot controls
  output$bin_cols <- renderUI({
    if(is.null(input$file)){return(NULL)}
    #   fdf<-filter(input$filts)
    fdf<-get(input$file)
    items=names(fdf)
    tagList(
      selectInput("bx", "Column to bin X-axis by",items), 
      selectInput("by", "Columns to plot",items,multiple=T)
    )
  })
  
  ##bin plot
  output$bplot <- renderPlot({ 
    fdf<-filter(input$filts)
    if(is.numeric(fdf[,input$bx])& input$off){
      bplot(fdf,input$bx,input$by,w=input$win,s=input$step,f=input$func,scale=input$scale)
      #bplot(fdf,input$x2,input$y2,w=input$win,s=input$step,f=input$func,scale=input$scale)
    }  
  },height=600,width=800)
  
  ##tile controls
  output$t_cols <- renderUI({
    if(is.null(input$file)){return(NULL)}
    #   fdf<-filter(input$filts)
    fdf<-get(input$file)
    items=names(fdf)
    tagList(
      selectInput("tx", "X-axis",items), 
      numericInput("txs","Scale",1), ##Allows rescaling of value as tile plots auto round to integers
      checkboxInput("tlogx","Log",value = F),
      selectInput("ty", "Y-axis",items),
      numericInput("tys","Scale",1),
      checkboxInput("tlogy","Log",value = F),
      selectInput("tz", "Colour by",items),
      numericInput("tzs","Scale",1),
      checkboxInput("tlogz","Log",value = F)
      )
  })
  
  ##tile plots
  output$tplot <- renderPlot({ 
    fdf<-filter(input$filts)
    if(is.numeric(fdf[,input$tx]) & is.numeric(fdf[,input$ty]) & is.numeric(fdf[,input$tz]) & input$off){
      tiler(fdf,input$tx,input$tlogx,input$txs,input$ty,input$tlogy,input$tys,input$tz,input$tlogz,input$tzs,bin=input$bins,min=input$tmin,max=input$tmax,xrange=c(input$txmin,input$txmax),yrange=c(input$tymin,input$tymax),scale=input$tscale,func=input$tsummary)  
    }  
  },height=600,width=800)
  
  # Download
  output$downloadData <- downloadHandler(
    filename = function() {input$tableName},
    content = function(file) {
      fdf<-filter(input$filts)
      fdf<-fdf[, input$show_vars, drop = FALSE]
      write.table(fdf, file,sep="\t",quote=F,row.names=F)
    }
  )
 
}
)


