shinyServer(function(input, output,session) {

  ##close app button
  observeEvent(input$close, {
    stopApp()
  })
  
  values<-reactiveValues(items=vector(),numeric=vector(),factor=vector())
  
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
  
  ##Get filtering code
  codefilter<-eventReactive(input$execute,{
    return(input$add)
  })
  
  ##List available files in directory (no longer required with shinyFiles)
  #observeEvent(input$list_dir, {
  #  withProgress(message="Listing files...",value=0,{
  #    output$inFiles <- renderUI({
  #      tagList(
  #        radioButtons('files', 'Select file:',list.files(input$dir,include.dirs = F,recursive=input$recursive,full.names = T,pattern=input$pattern))
  #      )
  #    })
  #  })
  #})
  
  ##ui to select an environment object
  output$inObjects <- renderUI({
    tagList(
      radioButtons('object', 'Select data.frame object from current environment:',ls(pos=1))
    )
  })
  
  ##ui to select server file
  roots<-c(home= '~')
  shinyFileChoose(input, 'sfile', roots=roots, session=session,filetypes=c('', 'txt','tab','tsv','csv','xls','xlsx'))
  input_files <- reactive({
    id<-""
    if(!is.null(input$sfile)){
      id<-parseFilePaths(roots, input$sfile)
      id<-as.character(id$datapath)
    }
    return(id)
  })
  output$path <- renderText(input_files())
  
  ##Get data
  Data<-reactive({
    sep<-switch(input$sep,tab="\t",space=" ",comma=",")
    if(input$inputType=="Upload"){
      if(is.null(input$file)){
        df<-test
      }
      else{
        inFile<-input$file
        if(inFile == "" | is.null(inFile)){
          return(NULL)
        }
        if(input$xls){
          df<-read.xls(inFile$datapath,header=input$header,fill=T)
        }
        else{
          df<-read.table(inFile$datapath,header=input$header,fill=T,sep=sep)
        }
      }
    }
    else if(input$inputType=="Server"){
      inFile<-input_files()
      if(is.null(inFile)){
        return(NULL)
      }
      if(input$xls){
        df<-read.xls(inFile,header=input$header,fill=T)
      }
      else{
        df<-read.table(inFile,header=input$header,fill=T,sep=sep)
      }
    }
    else if(input$inputType=="Environment"){
      if(is.data.frame(get(input$object))){
        df<-get(input$object)
      }
      else{
        return(NULL)
      }
    }
    try({
        df<-filter(df,codefilter())
        values$numeric<-names(df[,sapply(df,is.numeric),drop=F])
        values$factor<-names(df[,sapply(df,is.factor),drop=F])
        values$items<-names(df)
        return(df)
    })
    values$numeric<-names(df[,sapply(df,is.numeric),drop=F])
    values$factor<-names(df[,sapply(df,is.factor),drop=F])
    values$items=names(df)
    return(df)
  })
  
  ##Create a file download button
  output$downloadFiles<-renderUI({
    downloadName<-"Table.tab"
    if(input$inputType=="Server" & !is.null(input$sfile)){
      downloadName<-basename(input_files())
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
    return(fdf)
  },options = list(bSortClasses = TRUE,aLengthMenu = c(5,10,20,50,100), iDisplayLength = 5)
  )
  
  output$summarycols = renderUI({
    selectInput("summarycol","Select column",choices=values$items)
  })
  
  output$tablesum = renderPrint({
    fdf<-Data()
    summary(fdf[,input$summarycol])
  })
  
  output$colnames = renderPrint({
    fdf<-Data()
    names(fdf)
  })
 ##1d plot controls
 output$plot_cols <- renderUI({
   isolate({
    if(is.null(Data())){return(NULL)}
   })
   items=values$numeric #get numeric columns only
   tagList(
      selectInput("x", "Columns to plot",items,multiple=T,selected = items[1])    
    )
 })
  
 ##1d plot
 output$plot <- renderPlot({ 
   if(is.null(Data())){return(NULL)}
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
   isolate({
    if(is.null(Data())){return(NULL)}
   })  
   items=values$numeric #get numeric columns only
   tagList(
       selectInput("dx", "Column to plot",items),
       selectInput("dy", "Column to plot",items)
   )
 })
 
   ##2d plot
   output$dplot <- renderPlot({ 
     if(is.null(Data())){return(NULL)}
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
     isolate({
      if(is.null(Data())){return(NULL)}
     })
     items=values$items
     items.f=values$factor
     tagList(
       selectInput("ggx", "Select x-axis",c("NA",items)),
       selectInput("ggy", "Select y-axis",c("NA",items)),
       checkboxInput("gg_faceted","Facet plots",F),
       conditionalPanel(condition = "input.gg_faceted == true",
          selectInput("gg_facet", "Facet plot by:",items.f,multiple = T),
          checkboxInput("gg_facet_drop","Drop faceted panels without data",T),
          numericInput("gg_facet_row","Number of rows:",0),
          numericInput("gg_facet_col","Number of columns:",0)
       )
     )
   })  
  
   output$ggplot_colours <- renderUI({
     isolate({
      if(is.null(Data())){return(NULL)}
     })
     items=values$items
     tagList(
          selectInput("gg_fill","Colour fill by:",c("NA",items)),
          selectInput("gg_colour","Colour points and lines by:",c("NA",items)),
          selectInput("gg_alpha","Set transparency (alpha) by:",c("NA",items)),
          selectInput("gg_text","Set text for plotly hover:",c("NA",items))
      )
   })  
   
   ##ggplot controls
   output$ggplot_controls <- renderUI({
       items=values$items
       tagList(
         conditionalPanel(condition="input.gg_geom == 'histogram'",
              numericInput("gg_bins","Number of bins (0=default):",0),
              numericInput("gg_binwidth","Bin widths (0=default):",0),
              helpText("Manually setting bin widths overwrites number of bins")
              
         ),
         conditionalPanel(condition="input.gg_geom == 'bar'",
              selectInput("gg_bar.position","Bar positioning",c("stack","dodge","fill")),
              selectInput("gg_stat_method","Statistic for y-values",c("count","identity","summary")),
              conditionalPanel(condition="input.gg_stat_method == 'summary'",
                    selectInput("gg_stat.func","Summary method",c("mean","median","sum"))
              )
         ),
         conditionalPanel(condition="input.gg_geom == 'point'",
              selectInput("gg_smooth","Add a smoothing line to points",c("NA","auto","gam","lm","glm","rlm","loess")),
              checkboxInput("gg_smooth.se","Include standard error",T),
              selectInput("gg_labels","Label points",c("NA",items)),
              conditionalPanel(condition="input.gg_labels != 'NA'",
                numericInput("gg_nudge_x","Nudge labels on X-axis",0),
                numericInput("gg_nudge_y","Nudge labels on Y-axis",0),
                textInput("gg_label_display","Labels to display","")
              )
         ),
         conditionalPanel(condition="input.gg_geom == 'line'"
         ),
         conditionalPanel(condition="input.gg_geom == 'boxplot'",
              checkboxInput("gg_outliers","Include outliers in plot",T),
              checkboxInput("gg_varwidth","Use variable width boxes",F),
              selectInput("gg_cut_method","Group continuous x-axis by",c("number","interval","width")),
              numericInput("gg_cut.n","Group number (n)",10),
              helpText("number = n groups with approx. equal observations, interval = n groups of equal range, width = groups of width n")
         ),  
         conditionalPanel(condition="input.gg_geom == 'violin'",
                          selectInput("gg_cut_method","Group continuous x-axis by",c("number","interval","width")),
                          numericInput("gg_cut.n","Group number (n)",10),
                          helpText("number = n groups with approx. equal observations, interval = n groups of equal range, width = groups of width n")
         ),
         conditionalPanel(condition="input.gg_geom == 'tile'",
              checkboxInput("gg_condense","Summarise overlaps",F),
              conditionalPanel(condition = "input.gg_condense == true",
                          selectInput("gg_condense_func", "Summary function",c("mean","median","sum","count")),
                          numericInput("gg_condense.x","Size of X bin",10),
                          numericInput("gg_condense.y","Size of Y bin",10)
              ),
              checkboxInput("gg_tile_manual","Set tile dimensions manually:",F),
              conditionalPanel(
              condition = "input.gg_tile_manual == true",
                      numericInput("gg_tile_height","Tile height",NULL),
                      numericInput("gg_tile_width","Tile width",NULL)
              )
         )
       )
   })
   
   #Do not inactivate tabs
   outputOptions(output, 'ggplot_cols', suspendWhenHidden=FALSE)
   outputOptions(output, 'ggplot_colours', suspendWhenHidden=FALSE)
   #outputOptions(output, 'ggplot_plot', suspendWhenHidden=FALSE)
   outputOptions(output, 'ggplot_controls', suspendWhenHidden=FALSE)
   
   #plot_gg<-eventReactive(input$gg_plot,{
   plot_gg<-reactive({
     if(is.null(Data())){return(NULL)}
     fdf<-Data()
     #fdf<-filter(input$filts)
     if(input$auto){
       xlim=NA
       ylim=NA
       zlim=NA
       tile_width=NA
       tile_height=NA
       if(input$gg_manual==T){
        xlim<-c(input$gg_xmin,input$gg_xmax)
        ylim<-c(input$gg_ymin,input$gg_ymax)
       }
       if(input$gg_manual==T){
         xlim<-c(input$gg_xmin,input$gg_xmax)
         ylim<-c(input$gg_ymin,input$gg_ymax)
       }
       if(input$gg_grad_manual==T){
         zlim<-c(input$gg_gradient.min,input$gg_gradient.max)
       }
       if(input$gg_tile_manual==T){
         tile_height<-input$gg_tile_height
         tile_width<-input$gg_tile_width
       }
       fill=NA
       colour=NA
       alpha=NA
       text=NA
       man_fill=NA
       man_colour=NA
       man_alpha=NA
       facet=NA
       smooth=NA
       labels=NA
       label_display=NA
       x=NA
       y=NA
       if(input$gg_fill != "NA"){
         fill = input$gg_fill
       }
       if(input$gg_colour != "NA"){
         colour = input$gg_colour
       }
       if(input$gg_alpha != "NA"){
         alpha = input$gg_alpha
       }
       if(input$gg_text != "NA"){
         text = input$gg_text
       }
       if(input$gg_man_fill != "NA"){
         man_fill = input$gg_man_fill
       }
       if(input$gg_man_colour != "NA"){
         man_colour = input$gg_man_colour
       }
       if(input$gg_man_alpha != 0){
         man_alpha = input$gg_man_alpha
       }
       if(input$gg_smooth != "NA"){
         smooth = input$gg_smooth
       }
       if(input$gg_labels != "NA"){
         labels = input$gg_labels
       }
       if(input$gg_label_display != ""){
         label_display = eval(parse(text=paste0("subset(fdf,fdf$",input$gg_label_display,")[,input$gg_labels]")))
       }
       if(input$gg_faceted){
         if(!is.null(input$gg_facet)){
           facet = input$gg_facet
         }
       }
       if(input$ggx != "NA"){
         x = input$ggx
       }
       if(input$ggy != "NA"){
         y = input$ggy
       }
       p<-ggplot_builder(d=fdf,x=x,y=y,logx=input$gg_logx,logy=input$gg_logy,facet=facet,facet_drop=input$gg_facet_drop,facet_row=input$gg_facet_row,facet_col=input$gg_facet_col,
                        geom=input$gg_geom,smooth=smooth,smooth.se=input$gg_smooth.se,xrotate=input$gg_xrotate,colour=colour,
                        fill=fill,alpha=alpha,text=text,labels=labels,label_display=label_display,nudge_y=input$gg_nudge_y,nudge_x=input$gg_nudge_x,bar.position = input$gg_bar.position,binwidth=input$gg_binwidth,bins=input$gg_bins,stat.method=input$gg_stat_method,
                        stat.func=input$gg_stat.func,theme = input$gg_theme,coord_flip=input$gg_coord_flip,
                        enable.plotly = input$gg_plotly,outliers=input$gg_outliers,varwidth=input$gg_varwidth,colourset=input$gg_colourset,
                        gradient=input$gg_gradient,gradient.trans=input$gg_gradient.trans,gradient.steps=input$gg_gradient.steps,xlim=xlim,ylim=ylim,man_colour=man_colour,man_fill=man_fill,man_alpha=man_alpha,
                        cut_method=input$gg_cut_method,cut.n=input$gg_cut.n,factorlim=input$factorlim,tile_width=tile_width,tile_height=tile_height,
                        gradient.range=zlim,condense=input$gg_condense,condense.x = input$gg_condense.x,condense.y = input$gg_condense.y,condense.func = input$gg_condense_func)
       return(p)
     }  
   })
   
   ##ggplot
   output$ggplot <- renderPlot({ 
     withProgress(message="Plotting...",value=0,{
      plot_gg()
     })
   })
   
   output$ggplotly <- renderPlotly({
     withProgress(message="Plotting...",value=0,{
       plot_gg()
     })
   })
   
  ##bin plot controls
  output$bin_cols <- renderUI({
    isolate({
      if(is.null(Data())){return(NULL)}
    })
    items=values$numeric #get numeric columns only
    tagList(
        selectInput("bx", "Column to bin X-axis by",items), 
        selectInput("by", "Columns to plot",items,multiple=T,selected = items[1]),
        selectInput("baxis3", "Column to plot on separate axis (e.g. length of regions)",c("NA",items))
    )
  })
  
  ##bin plot
  output$bplot <- renderPlot({ 
    if(is.null(Data())){return(NULL)}
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
    isolate({
      if(is.null(Data())){return(NULL)}
    })
    items=values$numeric #get numeric columns only
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
    if(is.null(Data())){return(NULL)}
    if(length(input$tx)==0 | length(input$ty)==0 | length(input$tz)==0){
      return(NULL)
    }
    fdf<-Data()
    #fdf<-filter(input$filts)
    txrange=NA
    tyrange=NA
    tzrange=NA
    if(input$txman){
      txrange=c(input$txmin,input$txmax)
    }
    if(input$tyman){
      tyrange=c(input$tymin,input$tymax)
    }
    if(input$tzman){
      tzrange=c(input$tzmin,input$tzmax)
    }
    if(input$auto){
      tiler(t=fdf,x=input$tx,xl=input$tlogx,xs=input$txs,y=input$ty,yl=input$tlogy,ys=input$tys,
            z=input$tz,zl=input$tlogz,zs=input$tzs,bin=input$bins,zrange=tzrange,
            xrange=txrange,yrange=tyrange,
            func=input$tsummary)  
    }  
  })
  
  ##heatmap
  ##plot controls
  output$h_cols <- renderUI({
    isolate({
      if(is.null(Data())){return(NULL)}
    })
    items=values$numeric #get numeric columns only
    rnames=values$items
    tagList(
        selectInput("hrows", "Column to use for row names:",rnames), 
        selectInput("hcols", "Columns to include in heatmap",items,multiple=T,selected = items[1:2])      
    )
  })
  
  output$hmap<-renderD3heatmap({
    if(is.null(Data())){return(NULL)}
    fdf<-Data()
    m<-as.matrix(fdf[1:input$hnrow,c(input$hcols)])
    rownames(m)<-as.character(fdf[1:input$hnrow,input$hrows])
    colnames(m)<-c(input$hcols)
    colors <- colorRampPalette( rev(brewer.pal(9, "Blues")) )(255)
    d3heatmap(m, scale = "none",k_row=input$hkrow,cexRow = 0.7,cexCol=0.7)
  })
  
  # Download ###handlers????
  output$downloadData <- downloadHandler(
    filename = function() {input$tableName},
    content = function(file) {
      fdf<-Data()
      write.table(fdf, file,sep="\t",quote=F,row.names=F)
    }
  )
  
  output$downloadggplot <- downloadHandler(
    filename = function() {paste0(input$ggplotName,".pdf")},
    content = function(file) {
      ggsave(file, plot = plot_gg(), device = "pdf")
    }
  )

 
}
)


