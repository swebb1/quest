library(shiny)
library(ggplot2)
library(reshape2)
library(corrplot)
library(questplots)
library(RColorBrewer)

#setup a color pallete choice on main dashboard or per tool?
cols<-brewer.pal(9,"Set1")

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
  },options = list(bSortClasses = TRUE,aLengthMenu = c(5,10,20,50,100), iDisplayLength = 10)
  )
  
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
     selectInput("x", "Columns to plot",items,multiple=T)    
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
 })
  
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
   })
  
  
  ##bin plot controls
  output$bin_cols <- renderUI({
    if(is.null(input$file)){return(NULL)}
    #   fdf<-filter(input$filts)
    fdf<-get(input$file)
    items=names(fdf)
    tagList(
      selectInput("bx", "Column to bin X-axis by",items), 
      selectInput("by", "Columns to plot",items,multiple=T),
      selectInput("baxis3", "Column to plot on separate axis (e.g. length of regions)",c("NA",items))
    )
  })
  
  ##bin plot
  output$bplot <- renderPlot({ 
    fdf<-filter(input$filts)
    if(is.numeric(fdf[,input$bx])& input$off){
      bmin<-"default"
      bmax<-"default"
      if(input$bmin!="default"){bmin<-as.numeric(input$bmin)}
      if(input$bmax!="default"){bmax<-as.numeric(input$bmax)}
      bplot(t=fdf,x=input$bx,y=input$by,ys=input$bys,ystep=input$bystep,ylab=input$bylab,axis3=input$baxis3,w=input$bw,s=input$bs
              ,f=input$bf,scale=input$bscale,leg=input$bleg,col=cols,max=bmax,min=bmin,feature=input$bfeature)
      #bplot(fdf,input$bx,input$by,w=input$win,s=input$step,f=input$func,scale=input$scale,leg=input$leg,col=cols)
      #bplot(fdf,input$x2,input$y2,w=input$win,s=input$step,f=input$func,scale=input$scale)
    }  
  })
  
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
  })
  
  # Download
  output$downloadData <- downloadHandler(
    filename = function() {input$tableName},
    content = function(file) {
      fdf<-filter(input$filts)
      fdf<-fdf[, input$show_vars, drop = FALSE]
      write.table(fdf, file,sep="\t",quote=F,row.names=F)
    }
  )

##Add plot save button on each panel for pdf, different for ggplot and others,
##May need to convert all plots to ggplot
#  output$downloadPlot <- downloadHandler(
#    filename = function() {input$tableName},
#    content = function(file) {
#      fdf<-filter(input$filts)
#      fdf<-fdf[, input$show_vars, drop = FALSE]
#      write.table(fdf, file,sep="\t",quote=F,row.names=F)
#    }
#  )

 
}
)


