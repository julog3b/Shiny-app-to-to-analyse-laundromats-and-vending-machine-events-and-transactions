shinyUI(fluidPage(
  
  fluidPage(
    useShinyjs(),
    titlePanel("Transaction Data for DATA 601"),
    
    #Setting the font weight and color for the dashboard
    style = "font-weight: 500; color: #4d3a7d;",   
    
    tabsetPanel(
      
      tabPanel("Date Selection",
               dashboardPage(skin = "blue",
                             dashboardHeader(disable = TRUE),
                             dashboardSidebar(disable = TRUE),
                             dashboardBody(
                               
                               fluidPage( 
                                 box(background= "green", 
                                     dateRangeInput("daterange", 
                                                    label = h3("Choose the dates
                                                               for the reporting
                                                               period:"),
                                                    start = Sys.Date() - 7,
                                                    end = Sys.Date())),
                                 width = 12,
                                 actionButton(inputId = "Go1", 
                                              label = "Load Data", 
                                              icon = icon("play"))  
                               )))),
      
      
      
      
      tabPanel("DATA",
               verbatimTextOutput(outputId = "heading1"),
               tabsetPanel(
                 
                 tabPanel("Terminal result string Details",
                          verbatimTextOutput(outputId = "SummaryB6"),
                          selectizeInput(inputId = "VariablesG4", 
                                         label = "Choose Sitename or type name:",
                                         choices = NULL, multiple = FALSE,
                                         selected = NULL),
                          withSpinner(
                            DT::dataTableOutput(outputId = 
                                                  "approved_per_appliance")
                          ),
                          verbatimTextOutput(outputId = "allStringsNotes")
                          
                 ),
                 
                 
                 
                 tabPanel("Approved transactions as a percentage",
                          sliderInput(inputId = "unapproved", 
                                      label = 
                                        "Maximum percentage approved (0.5 = 50%)",
                                      min = 0, max = 1, step = 0.05, value = 0.5),
                          
                          withSpinner(
                            DT::dataTableOutput(outputId = "approved_less50")
                          ),
                          verbatimTextOutput(outputId = "approved_less50notes")
                 ),
                 
                 
                 
                 
                 
                 tabPanel("Raw Data",
                          withSpinner(
                            DT::dataTableOutput(outputId = "datatable1")
                          )
                 ),
                 
                 
                 
                 
                 tabPanel("Data Summary",
                          withSpinner(
                            verbatimTextOutput(outputId = "SummaryA2")
                          )
                 ),
                 
                 
                 
               )
      ),
      
      tabPanel("PLOTS",
               verbatimTextOutput(outputId = "heading2"),
               tabsetPanel(
                 tabPanel("Terminal result string summary and plot",
                          withSpinner(
                            highchartOutput(outputId = "SummaryB0plot", height = 500),
                            
                          ),
                          verbatimTextOutput(outputId = "SummaryB0"),
                          verbatimTextOutput(outputId = "stringNotes1")
                 ),
                 
                 
                 
                 
                 
                 tabPanel("Total sales from each site",
                          selectInput(inputId = "graph3", label = "Sort:",
                                      choices = c("By sitename (A-Z)", 
                                                  "By sales descending"), 
                                      selected = "By sitename (A-Z)"),
                          
                          
                          mainPanel(
                            withSpinner(
                              highchartOutput(outputId = "selected_sort", height = 500)),
                            width = 500,  
                            
                          ), 
                          verbatimTextOutput(outputId = 'totalSalesNotes')
                 ),
                 
                 
                 tabPanel("Daily & Weekly Time Series Graphs of Sales",
                          selectInput(inputId = "VariablesG20", 
                                      label = "Choose Sitename or type name:",
                                      choices = NULL, multiple = FALSE, 
                                      selected = NULL),
                          selectInput(inputId = "graph", 
                                      label = "Choose the period:", 
                                      choices = c("Daily", "Weekly"), 
                                      selected = "Daily"),
                          mainPanel(
                            withSpinner(
                              highchartOutput(outputId = "selected_graph", height = 500)),
                            width = 800,
                            
                          ), 
                          selectInput(inputId = "ts_line_bar", 
                                      label = "Choose graph type",
                                      choices = c("Line graph" = "line",
                                                  "Histogram" = "column"), 
                                      selected = "Line graph"),
                          verbatimTextOutput(outputId = "timeseriesNotes")
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 tabPanel("Time Series Decomposition",
                          selectInput(inputId = "VariablesG22", 
                                      label = "Choose Sitename or type name:",
                                      choices = NULL, multiple = FALSE, 
                                      selected = NULL),
                          sliderInput(inputId = "window", 
                                      label = "Choose trend window", 
                                      min = 1, max = 33, step = 2, value = 11),
                          mainPanel(
                            withSpinner(
                              plotOutput(outputId = "ts_decompose", height = 500)),
                            width = 500,  
                            
                          ),
                          verbatimTextOutput(outputId = "ts_decomposeNOTES")
                 ),
                 
                 
                 
                 
                 
                 
                 tabPanel("All appliances and all machine sales",
                          selectInput(inputId = "VariablesG2", 
                                      label = "Choose Sitename or type name:",
                                      choices = NULL, multiple = FALSE, 
                                      selected = NULL),
                          selectInput(inputId = 'graph4', 
                                      label =  "Select graph to view:",
                                      choices = c("machines", "appliances"),
                                      selected = "machines"),
                          
                          
                          verbatimTextOutput(outputId = 'SummaryB1'),
                          
                          withSpinner(
                            highchartOutput(outputId ="selected_graph4", height = 500)
                          ),
                          verbatimTextOutput(outputId = 'barNotes'),
                          verbatimTextOutput(outputId = 'SummaryB7'),
                          verbatimTextOutput(outputId = "applianceNotes")
                 ),
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 tabPanel("Sales Bubble plot",
                          selectInput(inputId = "VariablesG6", 
                                      label = "Choose Sitename or type name:",
                                      choices = NULL, multiple = FALSE, 
                                      selected = NULL),
                          checkboxInput(inputId = "hclegend", 
                                        label = "Show legend"),
                          
                          withSpinner(
                            highchartOutput(outputId = "appliance_serialNo", height = 500)
                          ),
                          verbatimTextOutput(outputId = "bubbleSummary")
                 ),
                 
                 
                 
                 
               )
      )
    )
  )
)
)