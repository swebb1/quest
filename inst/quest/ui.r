library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  title="Quest",
  skin="yellow",
  dashboardHeader(title = "Quest", titleWidth = 220),
  dashboardSidebar(width=220,
      sidebarMenu(
        #sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Search..."),
        menuItem("Files",tabName="files",icon=shiny::icon("upload")),
        menuItem("Data Table",tabName="data",icon=shiny::icon("database")),
        menuItem("1D plots",tabName="1d",icon=shiny::icon("line-chart")),
        menuItem("2D plots",tabName="2d",icon=shiny::icon("line-chart")),
        menuItem("ggplot wrapper",tabName="gg",icon=shiny::icon("line-chart")),
        menuItem("Binned plots",tabName="bin",icon=shiny::icon("line-chart")),
        menuItem("3D tile plots",tabName="3d",icon=shiny::icon("line-chart")),
        menuItem("Heatmaps",tabName="heatmap",icon=shiny::icon("th")),
        menuItem("Settings",tabName="settings",icon=shiny::icon("cogs")),
        menuItem("Help",tabName="help",icon=shiny::icon("question")),
        checkboxInput("auto","Auto-plot",value = T),
        actionButton("close","Close Quest",icon = shiny::icon("close"),style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      )
  ),
  dashboardBody(
     includeCSS("www/custom.css"),
     tabItems(
     tabItem(tabName="files",
               fluidRow(
                 box(
                   title="Load from your computer",width = 12,status="primary",solidHeader=TRUE,
                   #htmlOutput("fileUI"),
                   selectInput("inputType","Input file location:",choices=c("Upload","Server","Environment")),
                   conditionalPanel(
                     condition = "input.inputType == 'Upload'",
                     fileInput("file", "Input File",multiple = FALSE) ##May add upload file option)
                   ),
                   conditionalPanel(
                     condition = "input.inputType == 'Server'",
                     shinyFilesButton('sfile','Browse','Locate file',FALSE),
                     h4(textOutput('path'))
                     #textInput("dir","Select file directory:",value=ifelse(exists("wd"),wd,"")), ##maybe a bad hack to set wd as getwd() during runQuest call
                     #checkboxInput("recursive", "Search directory recursively", FALSE),
                     #textInput("pattern","Search pattern","",placeholder="*.tab"),
                     #actionButton("list_dir","List",icon = shiny::icon("folder-open")),
                     #uiOutput("inFiles")
                   ),
                   conditionalPanel(
                     condition = "input.inputType == 'Environment'",
                     uiOutput("inObjects")
                   ),
                   checkboxInput("header", "File has column headers", TRUE)
                 ),
                 box(
                   title="Download table",width = 12,status="primary",solidHeader=TRUE,
                   uiOutput("downloadFiles")
                 )
             )
     ),
     tabItem(tabName="data",
             fluidRow(
               box(
                 title = "Data Table", width = 12, status = "primary",solidHeader=TRUE,
                 div(style = 'overflow-x: scroll', dataTableOutput('table'))
               )
             ),
             fluidRow(
               box(
                 title = "Data Summary", width = 6, status = "primary",solidHeader=TRUE,collapsible = T,collapsed = T,
                 uiOutput('summarycols'),
                 verbatimTextOutput('tablesum')
               ),
               box(
                 title = "Column names", width = 6, status = "primary",solidHeader=TRUE,collapsible = T,collapsed = T,
                 verbatimTextOutput('colnames')
               )
            )
     ),
     tabItem(tabName="1d",
             fluidRow(
               box(
                 title="1D plots",width = 8,status="primary",solidHeader=TRUE,
                 plotOutput("plot",height = "600px")
               ),
               box(
                 title="Controls",width = 4,collapsible = T,status="success",solidHeader=TRUE,
                 wellPanel(p(strong("Data")), 
                           uiOutput("plot_cols")
                 ),
                 wellPanel(p(strong("Controls")),
                           selectInput("type","Plot Type:",choices=c("boxplot","histogram")),
                           conditionalPanel(condition="input.type=='boxplot'",
                                            textInput("bversus","Add filters to plot against a rival",value="")
                           ),
                           conditionalPanel(condition="input.type=='histogram'",
                                            checkboxInput("hlogx","Log X-axis",value = F),
                                            numericInput("breaks","Breaks",0),
                                            helpText("Uses default if set to 0")
                           )          
                 )
               )
             )
     ),
     tabItem(tabName="2d",
             fluidRow(
               box(
                   title="2D plots",width = 8,status="primary",solidHeader=TRUE,
                   plotOutput("dplot",height = "600px")
               ),
               box(
                 title="Controls",width = 4,collapsible = T,status="success",solidHeader=TRUE,
                 wellPanel(p(strong("Data")), 
                           uiOutput("dplot_cols")
                 ),
                 wellPanel(p(strong("Controls")),
                           selectInput("dtype","Plot type",choices=c("scatter","smoothScatter")),
                           checkboxInput("logx","Log X-axis",value = F),
                           checkboxInput("logy","Log Y-axis",value = F),
                           textInput("hilite","Highlight subset",value="")
                 )
               )
             )
     ),
     tabItem(tabName="gg",
             fluidRow(
               box(
                 title="gg plot",width = 8,status="primary",solidHeader=TRUE,
                 conditionalPanel(condition="input.gg_plotly==false",
                  plotOutput("ggplot",height = "600px")
                  #actionButton("gg_plot",label = "Plot")
                 ),
                 conditionalPanel(condition = "input.gg_plotly==true",
                  plotlyOutput("ggplotly",height = "600px")
                 )
               ),
               tabBox(
                 width = 4,
                 tabPanel("Inputs",
                          selectInput("gg_geom","Choose plot geometry",c("histogram","bar","point","line","boxplot","violin","tile")),
                          uiOutput("ggplot_cols"),
                          checkboxInput("gg_logx","Log x-axis",F),
                          checkboxInput("gg_logy","Log y-axis",F),
                          textInput("ggplotName","Save as:","Quest_plot"),
                          downloadButton('downloadggplot', 'Save plot as pdf')
                 ),
                 tabPanel("Colours",uiOutput("ggplot_colours"),
                          selectInput("gg_colourset","Select colourset:",c("default","Set1","Set2","Set3","Spectral")),
                          selectInput("gg_gradient","Select colours for gradients:",c("default","Matlab")),
                          selectInput("gg_gradient.trans","Transform gradient values:",c("identity","log","log2","log10","sqrt")),
                          numericInput("gg_gradient.steps","Number of steps in gradient",10),
                          checkboxInput("gg_grad_manual","Set gradient range manually:",F),
                          conditionalPanel(
                            condition = "input.gg_grad_manual == true",
                            numericInput("gg_gradient.min","Minimum gradient value",NULL),
                            numericInput("gg_gradient.max","Maximum gradient value",NULL)
                          ),
                          selectInput("gg_man_fill","Solid colour fill:",c("NA","firebrick","forest green","dodger blue")),
                          selectInput("gg_man_colour","Solid colour points and lines:",c("NA","firebrick","forest green","dodger blue")),
                          numericInput("gg_man_alpha","Select an alpha value for transparency:",0)
                 ),
                 tabPanel("Layout",
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
                          ),
                          checkboxInput("gg_coord_flip","Flip axes:",F)
                 ),
                 tabPanel("Controls",uiOutput("ggplot_controls"))
               )
             )
     ),
     tabItem(tabName="bin",
             fluidRow(
               box(
                 title="Binned plot",width = 8,status="primary",solidHeader=TRUE,
                 plotOutput("bplot",height = "600px")
               ),
               box(
                 title="Controls",width = 4,status="success",solidHeader=TRUE,
                 div(style = 'overflow-y: scroll', 
                 wellPanel(p(strong("Data")), 
                           uiOutput("bin_cols")
                 ),
                 wellPanel(p(strong("Controls")),style = 'overflow-y: scroll; max-height: 400px',
                           numericInput("bw","Bin size",200,min=1),
                           numericInput("bs","Step size",40,min=1),
                           numericInput("bys","Rescale y-axis ",1,min=1),
                           selectInput("bf","Operation",choices=c("mean","median","boxes","sum","max","min")),
                           selectInput("bscale","Scale",choices=c("linear","log","bins")), 
                           selectInput("bleg","Legend Position",choices=c("topleft","topright","bottomleft","bottomright")),
                           textInput("bmin","Minimum y-axis value","default"),
                           textInput("bmax","Maximum y-axis value","default"),
                           numericInput("bystep","Y axis step size",0,min=0),
                           textInput("bylab","Y axis label",""),
                           textInput("bfeature","Name of features","data points")
                 )
                 )
                 )
             )
     ),
     tabItem(tabName="3d",
             fluidRow(
               box(
                   title="3D tile plot",width = 8,status="primary",solidHeader=TRUE,
                   plotOutput("tplot",height = "600px")
               ),
               box(
                 title="Data",width = 4,status="success",solidHeader=TRUE,
                 wellPanel(p(strong("Data")),style = 'overflow-y: scroll; max-height: 300px',
                  uiOutput("t_cols")
                 )
               )
             ),
             fluidRow(
               box(
                 title="Controls",width = 12,status="success",solidHeader=TRUE,
                 wellPanel(p(strong("Controls")),style = 'overflow-y: scroll; max-height: 400px',
                           numericInput("bins","Bins",1,min=1,max=1000),
                           selectInput("tsummary","Operation",choices=c("mean","median","sum","count")),                                   
                           checkboxInput("tzman","Manually alter colour scale",F),
                           conditionalPanel("input.tzman == true",
                             numericInput("tzmin","Minimum colour scale",0),
                             numericInput("tzmax","Maximum colour scale",0)
                           ),
                           checkboxInput("txman","Manually alter X scale",F),
                           conditionalPanel("input.txman == true",
                             numericInput("txmin","Minimum x-axis value",0),
                             numericInput("txmax","Maximum x-axis value",0)
                           ),
                           checkboxInput("tyman","Manually alter Y scale",F),
                           conditionalPanel("input.tyman == true",                                                    
                            numericInput("tymin","Minimum y-axis value",0),
                            numericInput("tymax","Maximum y-axis value",0)
                           )
                 )
               )
             )
     ),
     tabItem(tabName="heatmap",
             fluidRow(
               box(
                 title="Heatmaps",width = 8,status="primary",solidHeader=TRUE,
                 d3heatmapOutput("hmap",height = "600px")
               ),
               box(
                 title="Data",width = 4,status="success",solidHeader=TRUE,
                 uiOutput("h_cols")
               )
             ),
             fluidRow(
               box(
                 title="Controls",width = 12,status="success",solidHeader=TRUE,
                 numericInput("hnrow","Row limit",100,min=1,max=2000),
                 numericInput("hkrow","Number of K-means clusters",5,min=1,max=10)
               )
             )
     ),
     tabItem(tabName="settings",
             fluidRow(
               box(
                   title="Settings",width = 12,status="primary",solidHeader=TRUE,
                   numericInput("factorlim","Limit on factor levels to process in plots",50)
               )
             )
     ),
     tabItem(tabName="help",
             fluidRow(
               box(
                 title="Help",width = 12,status="primary",solidHeader=TRUE,
                 includeMarkdown("README.md")
               )
             )
     )
   ), 
   fluidRow(
     box(
       title="R Code",width = 6,status="danger",collapsible=TRUE,collapsed = TRUE,solidHeader=TRUE,
       HTML('<textarea id="add" rows="6" cols="100"></textarea>'),
       helpText("See help tab for examples"),
       actionButton("execute","Apply code",icon=shiny::icon("arrow-circle-right"))
    ),
    box(title="Basic Filter",width=6,status="danger",collapsible=TRUE,collapsed = TRUE,solidHeader=TRUE,
        uiOutput('filterui'),
        #checkboxInput('filterCheck',label = "Apply filter",value = F)
        actionButton("filterButton","Apply filter",icon=shiny::icon("arrow-circle-right")),
        actionButton("filterClear","Clear filter",icon=shiny::icon("arrow-circle-right"))
    )
   )
  )
)
)
