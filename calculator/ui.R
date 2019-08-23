library(shiny)
library(googleVis)
library(plotrix)
library(plotly)
library(ECharts2Shiny)

ui <- fluidPage(
  
  # We HAVE TO to load the ECharts javascript library in advance
  loadEChartsLibrary(),
  
  # strong horizonal line break
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  # App title ----
  titlePanel("Mercedes Benz Dealership Sales Employee Attrition Impact Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Include clarifying text ----
      h4("Previous Attrition Rate of Mercedes Benz Dealers Across Germany is 40%."),
      
      # Input: Slider for the attrition rate  ----
      sliderInput(inputId = "attritionRate",
                  label = "New Attrition Rate:",
                  min = 1,
                  max = 100,
                  value = 20),
      strong(hr()),
      br(),
      h4("What-If Analysis (Other Inputs' Sensitivity):"),
      br(),
      h4("Bottom-Line Sensitivity:"),
      h5("Variable = Hiring and Training Cost "),
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "turnovercost",
                   label = "Cost of 1 Sales Employee Turnover (in €) :",
                   value = 9000),
      
      br(),
      h4("Sales and Customers Senstivity:"),
      h5("Variable = Sales Funnel Conversion Rate"),
      
      # Input: Slider for leads conversion ----
      sliderInput(inputId = "leadrate",
                  label = "Lead Conversion Rate:",
                  min = 60,
                  max = 80,
                  value = 60),
      
      # Input: Slider for prospect conversion ----
      sliderInput(inputId = "prosprate",
                  label = "Prospect Conversion Rate:",
                  min = 25,
                  max = 40,
                  value = 25),
      
      # Input: Slider for testdrive conversion ----
      sliderInput(inputId = "tdrate",
                  label = "Test-Drive Conversion Rate:",
                  min = 70,
                  max = 90,
                  value = 70),
      
      # Input: Slider for prospect conversion ----
      sliderInput(inputId = "salerate",
                  label = "Sale Conversion Rate:",
                  min = 60,
                  max = 80,
                  value = 60)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Market Comparision", h4("The industry average for sales turnover in automobile industry dealership is 60%."), plotOutput("industry1"), verbatimTextOutput("industry2")),
                  tabPanel("Bottom-Line (MB)", br(), verbatimTextOutput("bottomline"), h3(htmlOutput("bottomline1")), br(), verbatimTextOutput("bottomline2")),
                  tabPanel("Sales (MB)", verbatimTextOutput("sales2"), h3(htmlOutput("sales3")), verbatimTextOutput("sales"),br(),column(6,tags$div(id="sales1", style="width:500px;height:400px;position: relative;top: -20px;"), deliverChart(div_id = "sales1"))),
                  tabPanel("Customers (MB)", br(), verbatimTextOutput("customers"),plotlyOutput("customers1")),
                  tabPanel("Employees (MB)", br(), verbatimTextOutput("employees"), h3(htmlOutput("employees1")), column(5, tags$div(id="wc_div", style="width:400px;height:400px;position: relative;top: -20px;"), deliverChart("wc_div"))),
                  tabPanel("Stock (MB)", br(), verbatimTextOutput("stock"), br(), h3(htmlOutput("stock1")), htmlOutput("stock2"))
      )
    )
  )
)
      