shinyServer(function(input, output,session) {

  ##close app button
  observeEvent(input$close, {
    stopApp()
  })
  
  ##filter function based on R code
  filter<-function(d,fstring){
    df<-d
    if(fstring != ""){
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
  
  ##List available files in directory
  observeEvent(input$list_dir, {
    withProgress(message="Listing files...",value=0,{
      output$inFiles <- renderUI({
        tagList(
          radioButtons('files', 'Select file:',list.files(input$dir,include.dirs = F,recursive=input$recursive,full.names = T,pattern=input$pattern))
        )
      })
    })
  })
  
  ##Get data
  Data<-reactive({
    if(is.null(input$file) & is.null(input$files)){
      df<-test
    }
    else if(input$inputType=="Upload"){
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
  
  ##Create a file download button
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
   if(is.null(input$file) & is.null(input$files)){return(NULL)}
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
     if(is.null(input$file) & is.null(input$files)){return(NULL)}
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

   ##ggplot data
   output$ggplot_cols <- renderUI({
     if(is.null(input$file) & is.null(input$files)){return(NULL)}
     #fdf<-filter(input$filts)
     if(!input$freeze){
       fdf<-Data()
       items=names(fdf) #get numeric columns only
       tagList(
         selectInput("gg_geom","Choose plot geometry",c("histogram","bar","point","line","boxplot","violin")),
         selectInput("ggx", "Select x-axis",items),
         selectInput("ggy", "Select y-axis",items),
         #selectInput("ggz", "Select z-axis",items),
         checkboxInput("gg_logx","Log x-axis"),
         checkboxInput("gg_logy","Log y-axis"),
         selectInput("gg_facet", "Facet plot by:",c("NA",items))
       )
     }
   })  
  
   output$ggplot_colours <- renderUI({
     if(is.null(input$file) & is.null(input$files)){return(NULL)}
     #fdf<-filter(input$filts)
     if(!input$freeze){
       fdf<-Data()
       items=names(fdf) #get numeric columns only
       tagList(
          selectInput("gg_fill","Colour Fill by:",c("NA",items)),
          selectInput("gg_colour","Colour points by:",c("NA",items)),
          selectInput("gg_man_colour","Solid colour:",c("NA","firebrick","forest green","dodger blue")),
          selectInput("gg_man_fill","Solid fill:",c("NA","firebrick","forest green","dodger blue"))
      )
     }
   })  
   
   ##ggplot controls
   output$ggplot_plot <- renderUI({
     if(is.null(input$file) & is.null(input$files)){return(NULL)}
     #fdf<-filter(input$filts)
     if(!input$freeze){
       fdf<-Data()
       items=names(fdf) #get numeric columns only
       tagList(
         numericInput("gg_xrotate","Rotate X-axis labels",0),
         checkboxInput("gg_plotly","Activate Plotly"),
         selectInput("gg_theme", "Plot theme:",c("grey","bw","dark","light","void","linedraw","minimal","classsic")),
         checkboxInput("gg_manual","Set axes manually:",F),
         conditionalPanel(
           condition = "input.gg_manual == true",
           numericInput("gg_xmin","X-axis minimum",NULL),
           numericInput("gg_xmax","X-axis maximum",NULL),
           numericInput("gg_ymin","Y-axis minimum",NULL),
           numericInput("gg_ymax","Y-axis maximum",NULL)
         )
       )
     }
   })
   
   ##ggplot controls
   output$ggplot_controls <- renderUI({
     if(is.null(input$file) & is.null(input$files)){return(NULL)}
     #fdf<-filter(input$filts)
     if(!input$freeze){
       fdf<-Data()
       items=names(fdf) #get numeric columns only
       tagList(
         conditionalPanel(condition="input.gg_geom == 'histogram'",
              numericInput("gg_binwidth","Bin widths (0=default):",0)
         ),
         conditionalPanel(condition="input.gg_geom == 'bar'",
              selectInput("gg_bar.position","Bar positioning",c("stack","dodge","fill"))
         ),
         conditionalPanel(condition="input.gg_geom == 'point'",
              selectInput("gg_smooth","Add a smoothing line to points",c("NA","auto","gam","lm","glm","rlm","loess"))
         ),
         conditionalPanel(condition="input.gg_geom == 'line'"
         ),
         conditionalPanel(condition="input.gg_geom == 'boxplot'",
              checkboxInput("gg_outliers","Remove outliers from plot")
         ),  
         conditionalPanel(condition="input.gg_geom == 'violin'"         )
       )
     }
   })
   
   #Do not disactivate tabs
   outputOptions(output, 'ggplot_cols', suspendWhenHidden=FALSE)
   outputOptions(output, 'ggplot_colours', suspendWhenHidden=FALSE)
   outputOptions(output, 'ggplot_plot', suspendWhenHidden=FALSE)
   outputOptions(output, 'ggplot_controls', suspendWhenHidden=FALSE)
   
   ##ggplot
   output$ggplot <- renderPlot({ 
     if(is.null(input$file) & is.null(input$files)){return(NULL)}
     fdf<-Data()
     #fdf<-filter(input$filts)
     if(input$auto){
       xlim=NA
       ylim=NA
       if(input$gg_manual==T){
        xlim<-c(input$gg_xmin,input$gg_xmax)
        ylim<-c(input$gg_ymin,input$gg_ymax)
       }
       fill=NA
       colour=NA
       man_fill=NA
       man_colour=NA
       facet=NA
       smooth=NA
       if(input$gg_fill != "NA"){
         fill = input$gg_fill
       }
       if(input$gg_colour != "NA"){
         colour = input$gg_colour
       }
       if(input$gg_man_fill != "NA"){
         man_fill = input$gg_man_fill
       }
       if(input$gg_man_colour != "NA"){
         man_colour = input$gg_man_colour
       }
       if(input$gg_smooth != "NA"){
         smooth = input$gg_smooth
       }
       if(input$gg_facet != "NA"){
         facet = input$gg_facet
       }
         ggplot_builder(d=fdf,x=input$ggx,y=input$ggy,z=input$ggz,logx=input$gg_logx,logy=input$gg_logy,facet=facet,
                        geom=input$gg_geom,smooth=smooth,xrotate=input$gg_xrotate,colour=colour,
                        fill=fill,bar.position = input$gg_bar.position,binwidth=input$gg_binwidth,theme = input$gg_theme,
                        enable.plotly = input$gg_plotly,outliers=input$gg_outliers,
                        xlim=xlim,ylim=ylim,man_colour=man_colour,man_fill=man_fill)    

     }  
   })
  
   ###duplicated for plotly (can this be simplified?)
   output$ggplotly <- renderPlotly({ 
     if(is.null(input$file) & is.null(input$files)){return(NULL)}
     fdf<-Data()
     #fdf<-filter(input$filts)
     if(input$auto){
       xlim=NA
       ylim=NA
       if(input$gg_manual==T){
         xlim<-c(input$gg_xmin,input$gg_xmax)
         ylim<-c(input$gg_ymin,input$gg_ymax)
       }
       fill=NA
       colour=NA
       man_fill=NA
       man_colour=NA
       facet=NA
       smooth=NA
       if(input$gg_fill != "NA"){
         fill = input$gg_fill
       }
       if(input$gg_colour != "NA"){
         colour = input$gg_colour
       }
       if(input$gg_man_fill != "NA"){
         man_fill = input$gg_man_fill
       }
       if(input$gg_man_colour != "NA"){
         man_colour = input$gg_man_colour
       }
       if(input$gg_smooth != "NA"){
         smooth = input$gg_smooth
       }
       if(input$gg_facet != "NA"){
         facet = input$gg_facet
       }
       ggplot_builder(d=fdf,x=input$ggx,y=input$ggy,z=input$ggz,logx=input$gg_logx,logy=input$gg_logy,facet=facet,
                      geom=input$gg_geom,smooth=smooth,xrotate=input$gg_xrotate,colour=colour,
                      fill=fill,bar.position = input$gg_bar.position,binwidth=input$gg_binwidth,theme = input$gg_theme,
                      enable.plotly = input$gg_plotly,outliers=input$gg_outliers,
                      xlim=xlim,ylim=ylim,man_colour=man_colour,man_fill=man_fill)    
       
     }  
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
    if(is.null(input$file) & is.null(input$files)){return(NULL)}
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
    if(is.null(input$file) & is.null(input$files)){return(NULL)}
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
    if(is.null(input$file) & is.null(input$files)){return(NULL)}
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


