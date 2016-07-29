
shinyServer(function(input, output,session) {

  filter<-function(d,fstring){
    df<-d
    if(fstring != ""){ ##add columns if adds exist
      withProgress(message="Applying operation...",value=0,{
        a<-strsplit(fstring,"@")
        for(i in 1:length(a[[1]])){
          eval(parse(text=a[[1]][i]))
        }
      })
    }
    if(nrow(df) == 0){
      return(NULL)
    }
    df
  } 
  
  observeEvent(input$list_dir, {
    withProgress(message="Listing files...",value=0,{
      output$inFiles <- renderUI({
        tagList(
          radioButtons('files', 'Select file:',list.files(input$dir,include.dirs = F,recursive=input$recursive,full.names = T,pattern=input$pattern))
        )
      })
    })
  })
  

  Data<-reactive({
    if(is.null(input$file) & is.null(input$files)){
      return(df)
    }
    if(input$inputType=="Upload"){
      inFile<-input$file
      if(is.null(inFile)){
        return(NULL)
      }
      df<-read.table(inFile$datapath,header=input$header)
    }
    else if(input$inputType=="Server"){
        inFile<-input$files
        if(is.null(inFile)){
          return(NULL)
        }
        df<-read.table(inFile,header=input$header)
    }
    if(input$execute){
      try({
        df<-filter(df,input$add)
        return(df)
      })
      return<-NULL
    }
    else{
      return(df)
    }
  })
  
  output$downloadFiles<-renderUI({
    downloadName<-"Table.tab"
    if(input$inputType=="Server" & !is.null(input$files)){
      downloadName<-basename(input$files)
    }
    else if(input$inputType=="Upload" & !is.null(input$file)){
      inFile<-input$file
      downloadName<-inFile$name
    }
    tagList(
      textInput("tableName","Table Name:",value = downloadName),
      downloadButton('downloadData', 'Download Table')
    )
  })
  
  ##output table
  output$table = renderDataTable({
    fdf<-Data()
    #fdf<-filter(input$filts)
    fdfc<-fdf[, input$show_vars, drop = FALSE]  ##show selected columns
    if(dim(fdfc)[2]==0){ ##If no columns selected show full table. Needs Fixing!!!!
      return(fdf)
    }
    else{
      return(fdfc)
    }
  },options = list(bSortClasses = TRUE,aLengthMenu = c(5,10,20,50,100), iDisplayLength = 5)
  )
  
  ##table controls
  #output$show_cols<-renderUI({
  #  if(is.null(input$file)){return(NULL)}
  #  fdf<-filter(input$add)
  #  fdf<-Data()
  #  items=names(fdf)
  #  tagList(
  #    checkboxGroupInput('show_vars', 'Columns to show:', items, selected = items)
  #  )
  #})
  #outputOptions(output, 'show_cols', suspendWhenHidden=FALSE)
  
  ##Observe check box input for select/deselect all columns
  observe({
    if(is.null(input$file)){return(NULL)}
    fdf<-Data()
   # if (input$show_all){
    #    updateCheckboxGroupInput(
    #      session, 'show_vars','Columns to show:', choices = names(fdf),
    #      selected = names(fdf)
    #    )
  #  }
  #  else{
  #      updateCheckboxGroupInput(
  #        session, 'show_vars','Columns to show:', choices = names(fdf),
  #        selected = NULL
  #      )
  #  }  
  })

 ##1d plot controls
 output$plot_cols <- renderUI({
   if(is.null(input$file) & is.null(input$files)){return(NULL)}
   #fdf<-filter(input$filts)
   if(!input$freeze){
    fdf<-Data()
    items=names(fdf[,sapply(fdf,is.numeric),drop=F]) #get numeric columns only
    tagList(
      selectInput("x", "Columns to plot",items,multiple=T,selected = items[1])    
    )
   }
 })
  
 ##1d plot
 output$plot <- renderPlot({ 
   par(mar=c(10,5,5,5))
   withProgress(message="Plotting...",value=0,{
   fdf<-Data()
   #fdf<-filter(input$filts)
   if(input$auto){ ##Only plot if plotting turned on
     if(input$type=="boxplot"){
       ##plot rival boxplots based on a filter
       if(input$bversus!=""){
         bfilts<-input$bversus
         bfdf<-filter(fdf,bfilts)
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
       hx<-fdf[,input$x[1]]
       if(input$hlogx){
         hx<-log(hx) ##log x axis
       }
       if(input$breaks==0){
         hist(hx,xlab = input$x[1],col="firebrick",main="") ##default breaks
       }
       else{
         hist(hx,xlab = input$x[1],breaks=input$breaks,col="firebrick",main="") ##custom breaks
       }
     }
   }
   })
 })
  
 ##2d plot cols
 output$dplot_cols <- renderUI({
   if(is.null(input$file) & is.null(input$files)){return(NULL)}
   #fdf<-filter(input$filts)
   if(!input$freeze){
     fdf<-Data()
     items=names(fdf[,sapply(fdf,is.numeric),drop=F]) #get numeric columns only
     tagList(
       selectInput("dx", "Column to plot",items),
       selectInput("dy", "Column to plot",items)
     )
   }
 })
 
   ##2d plot
   output$dplot <- renderPlot({ 
     withProgress(message="Plotting...",value=0,{
     fdf<-Data()
     #fdf<-filter(input$filts)
     if(input$auto){
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
         hfdf<-filter(fdf,hfilts)
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
    if(is.null(input$file) & is.null(input$files)){return(NULL)}
    #fdf<-filter(input$filts)
    if(!input$freeze){
      fdf<-Data()
      items=names(fdf[,sapply(fdf,is.numeric),drop=F]) #get numeric columns only
      tagList(
        selectInput("bx", "Column to bin X-axis by",items), 
        selectInput("by", "Columns to plot",items,multiple=T,selected = items[1]),
        selectInput("baxis3", "Column to plot on separate axis (e.g. length of regions)",c("NA",items))
      )
    }
  })
  
  ##bin plot
  output$bplot <- renderPlot({ 
    fdf<-Data()
    #fdf<-filter(input$filts)
    if(input$auto){
      bmin<-"default"
      bmax<-"default"
      if(input$bmin!="default"){bmin<-as.numeric(input$bmin)}
      if(input$bmax!="default"){bmax<-as.numeric(input$bmax)}
      bplot(t=fdf,x=input$bx,y=input$by,ys=input$bys,ystep=input$bystep,ylab=input$bylab,axis3=input$baxis3,w=input$bw,s=input$bs
              ,f=input$bf,scale=input$bscale,leg=input$bleg,col=cols,max=bmax,min=bmin,feature=input$bfeature)
    }  
  })
  
  ##tile controls
  output$t_cols <- renderUI({
    if(is.null(input$file) & is.null(input$files)){return(NULL)}
    #fdf<-filter(input$filts)
    if(!input$freeze){
      fdf<-Data()
      items=names(fdf[,sapply(fdf,is.numeric),drop=F]) #get numeric columns only
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
    }
  })
  
  ##tile plots
  output$tplot <- renderPlot({ 
    if(length(input$tx)==0 | length(input$ty)==0 | length(input$tz)==0){
      return(NULL)
    }
    fdf<-Data()
    #fdf<-filter(input$filts)
    if(input$auto){
      tiler(t=fdf,x=input$tx,xl=input$tlogx,xs=input$txs,y=input$ty,yl=input$tlogy,ys=input$tys,
            z=input$tz,zl=input$tlogz,zs=input$tzs,bin=input$bins,min=input$tmin,max=input$tmax,
            xrange=c(input$txmin,input$txmax),yrange=c(input$tymin,input$tymax),
            func=input$tsummary)  
    }  
  })
  
  ##heatmap
  ##bin plot controls
  output$h_cols <- renderUI({
    if(is.null(input$file) & is.null(input$files)){return(NULL)}
    #fdf<-filter(input$filts)
    if(!input$freeze){
      fdf<-Data()
      items=names(fdf[,sapply(fdf,is.numeric),drop=F]) #get numeric columns only
      rnames=names(fdf)
      tagList(
        selectInput("hrows", "Column to use for row names:",rnames), 
        selectInput("hcols", "Columns to include in heatmap",items,multiple=T,selected = items[1:2])      )
    }
  })
  output$hmap<-renderD3heatmap({
    fdf<-Data()
    m<-as.matrix(fdf[1:input$hnrow,c(input$hcols)])
    rownames(m)<-as.character(fdf[1:input$hnrow,input$hrows])
    colnames(m)<-c(input$hcols)
    colors <- colorRampPalette( rev(brewer.pal(9, "Blues")) )(255)
    d3heatmap(m, scale = "none",k_row=input$hkrow,cexRow = 0.7,cexCol=0.7)
  })
  
  # Download
  output$downloadData <- downloadHandler(
    filename = function() {input$tableName},
    content = function(file) {
      fdf<-Data()
      #fdf<-filter(input$filts)
      #fdf<-fdf[, input$show_vars, drop = FALSE]
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


