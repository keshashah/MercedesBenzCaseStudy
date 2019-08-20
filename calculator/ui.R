library(shiny)
library(googleVis)

ui <- fluidPage(
  
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
                  value = 20)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Industry Comparision", br(), verbatimTextOutput("industry"), plotOutput("industry1"), verbatimTextOutput("industry2")),
                  tabPanel("Bottom-Line (MB)",br(), verbatimTextOutput("bottomline")),
                  tabPanel("Sales (MB)", br(), verbatimTextOutput("sales")),
                  tabPanel("Customers (MB)", br(), verbatimTextOutput("customers")),
                  tabPanel("Employees (MB)", br(), verbatimTextOutput("employees")),
                  tabPanel("Stock (MB)", br(), verbatimTextOutput("stock"), htmlOutput("stock1"))
      )
    )
    
  )
  
)
      