library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(
  title="Quest",
  skin="yellow",
  dashboardHeader(title = "Quest", titleWidth = 220),
  dashboardSidebar(width=220,
      sidebarMenu(
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search..."),
        menuItem("Data",tabName="data",icon=shiny::icon("database")),
        menuItem("1D plots",tabName="1d",icon=shiny::icon("line-chart")),
        menuItem("2D plots",tabName="2d",icon=shiny::icon("line-chart")),
        menuItem("Binned plots",tabName="bin",icon=shiny::icon("line-chart")),
        menuItem("3D tile plots",tabName="3d",icon=shiny::icon("line-chart")),
        menuItem("Help",tabName="help",icon=shiny::icon("question")),
        selectInput("addFilt","Filters",choices=c("No Filter","Apply Filters")),
        textInput("filts","Type filter expression:",value = ""),
        #textInput("add","Add a column to table:",value = ""), ##feature to add currently issue with filter update
        checkboxInput("off","Auto-plot",value = T)
      )
  ),
  dashboardBody(
   tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
   ),
   tabItems(
     tabItem(tabName="data",
             fluidRow(
               box(
                 title = "Data Table", width = NULL, status = "primary",
                 div(style = 'overflow-x: scroll', dataTableOutput('table'))
               ),
               box(
                title="Controls",width = NULL,status="success",
                htmlOutput("fileUI"),
                #fileInput("file", "Input File",multiple = FALSE) ##May add upload file option)
                checkboxInput("show", "Show columns", FALSE),
                checkboxInput('show_all', 'All/None', TRUE),
                conditionalPanel(
                  condition = "input.show == true",
                  uiOutput("show_cols")
                ),       
                textInput("tableName","Table Name:",value = "Table.tab"),
                downloadButton('downloadData', 'Download Table')
              )
            )
     ),
     tabItem(tabName="1d",
             fluidRow(
               box(
                 title="1D plots",width = 8,status="primary",
                 plotOutput("plot")
               ),
               box(
                 width = 4,collapsible = T,status="success",
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
                   title="2D plots",width = 8,status="primary",
                   plotOutput("dplot")
               ),
               box(
                 width = 4,collapsible = T,status="success",
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
                 title="Binned plots",width = 8,status="primary",
                 plotOutput("bplot")
               ),
               box(
                 width = 4,collapsible = T,status="success",
                 wellPanel(p(strong("Data")), 
                           uiOutput("bin_cols")
                 ),
                 wellPanel(p(strong("Controls")),
                           numericInput("win","Bin size",200,min=1),
                           numericInput("step","Step size",40,min=1),
                           selectInput("func","Operation",choices=c("mean","median","boxes","sum","max","min")),
                           selectInput("scale","Scale",choices=c("linear","log","bins")), 
                           selectInput("leg","Legend Position",choices=c("topleft","topright","bottomleft","bottomright")),
                           numericInput("bymin","Minimum y-axis value",0),
                           numericInput("bymax","Maximum y-axis value",100)
                 )
               )
             )
     ),
     tabItem(tabName="3d",
             fluidRow(
               box(
                   title="3D plots",width = 8,status="primary",
                   plotOutput("tplot")
               ),
               box(
                 width = 4,collapsible = T,status="success",
                 wellPanel(p(strong("Data")), 
                           uiOutput("t_cols")
                 ),
                 wellPanel(p(strong("Controls")),
                           numericInput("bins","Bins",1,min=1,max=1000),
                           numericInput("tmin","Minimum colour scale",0),
                           numericInput("tmax","Maximum colour scale",1),
                           numericInput("txmin","Minimum x-axis value",0),
                           numericInput("txmax","Maximum x-axis value",100),
                           numericInput("tymin","Minimum y-axis value",0),
                           numericInput("tymax","Maximum y-axis value",100),
                           selectInput("tscale","Colour scheme",choices=c("rainbow","zero")),
                           selectInput("tsummary","Operation",choices=c("mean","median","sum","count"))                                   
                 )
               )
             )
     ),
     tabItem(tabName="help",
             fluidRow(
               box(
                   title="Help",width = NULL,status="primary",
                   includeMarkdown("README.md")
               )
             )
     )
   ) 
  )
)
)