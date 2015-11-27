library(shiny)
shinyUI(fluidPage(
  ##CSS
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 400;
        font-size: 500%;
        line-height: 1.1;
        color: #ad1d28;
      }
      .well { background-color: #99CCFF; }

    "))
  ),
  ##Panels
  headerPanel("Quest"),
  sidebarPanel(style = "overflow-y:auto; max-height: 700px", ##scrolling in side bar
      htmlOutput("fileUI"),
      #     fileInput("file", "Input File",multiple = FALSE) ##May add upload file option
      selectInput("addFilt","Filters",choices=c("No Filter","Apply Filters")),
      textInput("filts","Type filter expression:",value = ""),
      #textInput("add","Add a column to table:",value = ""), ##feature to add currently issue with filter update
      checkboxInput("off","Auto-plot",value = T),
      hr(),
      conditionalPanel(
                condition="input.conditionedPanels==1",
                p(strong("Tables")),       
                wellPanel(p(strong("Controls")),
                          checkboxInput("show", "Show columns", FALSE),
                          checkboxInput('show_all', 'All/None', TRUE),
                          conditionalPanel(
                                condition = "input.show == true",
                                uiOutput("show_cols")
                          ),       
                          textInput("tableName","Table Name:",value = "Table.tab"),
                          downloadButton('downloadData', 'Download Table')
                )          
     ),
     conditionalPanel(
                condition="input.conditionedPanels==2",
                p(strong("1D Plots")),
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
     ),
    conditionalPanel(
                  condition="input.conditionedPanels==3",
                  p(strong("2D Plots")),
                  wellPanel(p(strong("Data")), 
                            uiOutput("dplot_cols")
                  ),
                  wellPanel(p(strong("Controls")),
                            selectInput("dtype","Plot type",choices=c("scatter","smoothScatter")),
                            checkboxInput("logx","Log X-axis",value = F),
                            checkboxInput("logy","Log Y-axis",value = F),
                            textInput("hilite","Highlight subset",value="")
                  )
       ),
    conditionalPanel(condition="input.conditionedPanels==4",
                  p(strong("Binned Plots")),
                  wellPanel(p(strong("Data")), 
                     uiOutput("bin_cols")
                  ),
                       wellPanel(p(strong("Controls")),
                         numericInput("win","Bin size",200,min=1),
                         numericInput("step","Step size",40,min=1),
                         selectInput("func","Operation",choices=c("mean","median","boxes","sum","max","min")),
                         selectInput("scale","Scale",choices=c("linear","log","bins")),  
                         numericInput("bymin","Minimum y-axis value",0),
                         numericInput("bymax","Maximum y-axis value",100)
                       )
      ),
      conditionalPanel(condition="input.conditionedPanels==5",
                       p(strong("3D Tiles")),
                       wellPanel(p(strong("Data")), 
                          uiOutput("t_cols")
                       ),
                       wellPanel(p(strong("Controls")),
                          numericInput("bins","Bins",1,min=1,max=1000),
                          numericInput("tmin","Minimum colour scale",-1),
                          numericInput("tmax","Maximum colour scale",1),
                          numericInput("txmin","Minimum x-axis value",0),
                          numericInput("txmax","Maximum x-axis value",100),
                          numericInput("tymin","Minimum y-axis value",0),
                          numericInput("tymax","Maximum y-axis value",100),
                          selectInput("tscale","Colour scheme",choices=c("zero","rainbow")),
                          selectInput("tsummary","Operation",choices=c("mean","median","sum","count"))                                   
                       )
      ) 
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Table", value=1,
               br(),
               dataTableOutput("table")         
      ),
      tabPanel("1D Plots", value=2,
               plotOutput("plot")
      ), 
      tabPanel("2D Plots", value=3,
               plotOutput("dplot")
      ), 
      tabPanel("Binned Plots", value=4,
               plotOutput("bplot")
      ), 
      tabPanel("3D Tiles", value=5,
               plotOutput("tplot"),
               plotOutput("tplot2")
      )
      , id = "conditionedPanels"
    )
#    )
  )
)
)