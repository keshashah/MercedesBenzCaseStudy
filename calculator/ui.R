library(shiny)
library(googleVis)
library(plotrix)
library(plotly)
library(ECharts2Shiny)

ui <- fluidPage(
  
  # We HAVE TO to load the ECharts javascript library in advance
  loadEChartsLibrary(),
  
  # App title ----
  titlePanel("Mercedes Benz Dealership Sales Employee Attrition Impact Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Include clarifying text ----
      helpText("The industry average for sales turnover in automobile industry dealership is 60%."),
      helpText("Previous Attrition Rate of Mercedes Benz Dealers Across Germany is 40%."),
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "attritionRate",
                  label = "New Attrition Rate:",
                  min = 1,
                  max = 100,
                  value = 20),
      
      br(),
      
      h4("What-If Analysis (Other Inputs' Sensitivity):"),
      h5("Bottom-line Sensitivity:"),
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "turnovercost",
                   label = "Cost of 1 Sales Employee Turnover (in €) :",
                   value = 9000)
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
        
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Industry Comparision", br(), verbatimTextOutput("industry"), plotOutput("industry1"), verbatimTextOutput("industry2")),
                  tabPanel("Bottom-Line (MB)", br(), verbatimTextOutput("bottomline"), h3(htmlOutput("bottomline1")), br(), verbatimTextOutput("bottomline2")),
                  tabPanel("Sales (MB)", verbatimTextOutput("sales2"), h3(htmlOutput("sales3")), verbatimTextOutput("sales"),br(),column(6,tags$div(id="sales1", style="width:500px;height:400px;position: relative;top: -20px;"), deliverChart(div_id = "sales1"))),
                  tabPanel("Customers (MB)", br(), verbatimTextOutput("customers"),plotlyOutput("customers1")),
                  tabPanel("Employees (MB)", br(), verbatimTextOutput("employees"), h3(htmlOutput("employees1")), column(5, tags$div(id="wc_div", style="width:400px;height:400px;position: relative;top: -20px;"), deliverChart("wc_div"))),
                  tabPanel("Stock (MB)", br(), verbatimTextOutput("stock"), br(), h3(htmlOutput("stock1")), htmlOutput("stock2"))
      )
    )
    
  )
  
)
      