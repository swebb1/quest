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
        menuItem("Binned plots",tabName="bin",icon=shiny::icon("line-chart")),
        menuItem("3D tile plots",tabName="3d",icon=shiny::icon("line-chart")),
        menuItem("Heatmaps",tabName="heatmap",icon=shiny::icon("th")),
        menuItem("Help",tabName="help",icon=shiny::icon("question")),
        checkboxInput("auto","Auto-plot",value = T),
        checkboxInput("freeze","Freeze inputs",value = F),
        checkboxInput("execute","Apply R code",value = F)
      )
  ),
  dashboardBody(
   tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
   ),
   tabItems(
     tabItem(tabName="files",
               fluidRow(
                 box(
                   title="Load from your computer",width = NULL,status="primary",solidHeader=TRUE,
                   #htmlOutput("fileUI"),
                   selectInput("inputType","Input file location:",choices=c("Upload","Server")),
                   conditionalPanel(
                     condition = "input.inputType == 'Upload'",
                     fileInput("file", "Input File",multiple = FALSE) ##May add upload file option)
                   ),
                   conditionalPanel(
                     condition = "input.inputType == 'Server'",
                     textInput("dir","Select file directory:",value="/homes/swebb/interactive_plotting/Odyssey/test_data"),
                     checkboxInput("recursive", "Search directory recursively", FALSE),
                     textInput("pattern","Search pattern","",placeholder="*.tab"),
                     actionButton("list_dir","List",icon = shiny::icon("folder-open")),
                     uiOutput("inFiles")
                   ),
                   #checkboxInput("show", "Show columns", FALSE),
                   #checkboxInput('show_all', 'All/None', TRUE),
                   #conditionalPanel(
                   #condition = "input.show == true",
                    #uiOutput("show_cols")
                   #),
                   checkboxInput("header", "File has column headers", TRUE)
                 ),
                 box(
                   title="Download table",width = NULL,status="primary",solidHeader=TRUE,
                   uiOutput("downloadFiles")
                 )
             )
     ),
     tabItem(tabName="data",
             fluidRow(
               box(
                 title = "Data Table", width = NULL, status = "primary",solidHeader=TRUE,
                 div(style = 'overflow-x: scroll', dataTableOutput('table'))
               )
             )
     ),
     tabItem(tabName="1d",
             fluidRow(
               box(
                 title="1D plots",width = 8,status="primary",solidHeader=TRUE,
                 plotOutput("plot")
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
                   plotOutput("dplot")
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
     tabItem(tabName="bin",
             fluidRow(
               box(
                 title="Binned plot",width = 8,status="primary",solidHeader=TRUE,
                 plotOutput("bplot")
               ),
               box(
                 title="Controls",width = 4,status="success",solidHeader=TRUE,
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
     ),
     tabItem(tabName="3d",
             fluidRow(
               box(
                   title="3D tile plot",width = 8,status="primary",solidHeader=TRUE,
                   plotOutput("tplot")
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
                           numericInput("tmin","Minimum colour scale",0),
                           numericInput("tmax","Maximum colour scale",1),
                           numericInput("txmin","Minimum x-axis value",0),
                           numericInput("txmax","Maximum x-axis value",100),
                           numericInput("tymin","Minimum y-axis value",0),
                           numericInput("tymax","Maximum y-axis value",100),
                           selectInput("tsummary","Operation",choices=c("mean","median","sum","count"))                                   
                 )
               )
             )
     ),
     tabItem(tabName="heatmap",
             fluidRow(
               box(
                 title="Heatmaps",width = 8,status="primary",solidHeader=TRUE,
                 d3heatmapOutput("hmap")
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
     tabItem(tabName="help",
             fluidRow(
               box(
                   title="Help",width = NULL,status="primary",solidHeader=TRUE,
                   includeMarkdown("README.md")
               )
             )
     )
   ), 
   fluidRow(
     box(
       title="R Code",width = NULL,status="danger",collapsible=TRUE,collapsed = TRUE,solidHeader=TRUE,
       HTML('<textarea id="add" rows="6" cols="200"></textarea>'),
       helpText("See help tab for examples")
     )
   )
  )
)
)