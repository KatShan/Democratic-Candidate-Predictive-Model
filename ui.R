library(shinythemes)
library(DT)


navbarPage("Democratic Candidate Analytics", theme = shinytheme("yeti"),
           tabPanel("Streamgraph", 
                    titlePanel("General Issue Tracking June 2019 to May 2020"),
                    
                    sidebarLayout(
                      # Sidebar with a slider and selection inputs
                      sidebarPanel(
                        dateRangeInput("streamgraph", label = "Choose your Date Range", start = '2019-06-01', 
                                       end = '2020-05-07', format = "yyyy-mm-dd", separator = " to ", width = NULL)
                      ),
                      
                      # Show Word Cloud
                      mainPanel(
                        streamgraphOutput("stream")
                      )
                    )
           ),
           tabPanel("Word Cloud", 
                    titlePanel("Word Cloud of Twitter Data"),
                    
                    sidebarLayout(
                      # Sidebar with a slider and selection inputs
                      sidebarPanel(
                        selectInput("selection", "Choose a candidate:",
                                    choices = candidate),
                        actionButton("update", "Change"),
                        hr(),
                        sliderInput("freq",
                                    "Minimum Frequency:",
                                    min = 1,  max = 50, value = 15),
                        sliderInput("max",
                                    "Maximum Number of Words:",
                                    min = 1,  max = 300,  value = 100)
                      ),
                      
                      # Show Word Cloud
                      mainPanel(
                        plotOutput("plot", width = "100%")
                      )
                    )
           ), 
           tabPanel("Biden and Sanders Comparison", 
                    titlePanel("Views on Topics: Biden and Sanders"),
                    sidebarLayout(
                      sidebarPanel(
                        selectizeInput("selection1", "Choose your candidates (max two):",
                                       choices = candidate2, multiple = TRUE,
                                       options = list(placeholder = 'Select Candidate(s)')),
                        hr(),
                        uiOutput("selection2"),
                        hr(),
                        #Add option for topics they agree on, disagree on, and have singular views on
                        #that the other doesn't have (anti-join)
                        conditionalPanel(
                          condition = "input.selection1.length != 1",
                          
                          selectizeInput("selection3", "Comparisons", 
                                         choices = c("All", "Agreements", "Disagreements"), multiple = FALSE, 
                                         selected = 0)
                        )
                      ), 
                      
                      mainPanel(
                        DT::dataTableOutput("table")
                      )
                    )
           ),
           tabPanel("LDA of Biden", 
                    titlePanel("LDA Analysis of Joe Biden with Data from the Democratic Debates"),
                    tabsetPanel(
                      tabPanel("Plot", 
                               hr(),
                               plotOutput("LDAPlot", width = "100%")), 
                      tabPanel("Data", 
                               hr(),
                               sidebarPanel(
                                 selectizeInput("selection4", "Select Topics",
                                                choices = topicsNumbers, multiple = TRUE,
                                                options = list(placeholder = 'Select Topic(s)'))
                               ),
                               mainPanel(
                                 DT::dataTableOutput("LDADataset")
                               )
                      )
                    )
           )
)