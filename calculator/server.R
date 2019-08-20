# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Output for Tab-0 i.e. industry
  output$industry <- renderText({
    "Show industry here"
  })  
  
  # Output for Tab-1 i.e. bottomline
  output$bottomline <- renderText({
    "Show bottomline here"
  })  
  
  # Output for Tab-2 i.e. sales
  output$sales <- renderText({
    "Show sales here"
  })  
  
  # Output for Tab-3 i.e. customers
  output$customers <- renderText({
    "Show customers here"
  })  
  
  # Output for Tab-4 i.e. employees
  output$employees <- renderText({
    "Show employees here"
  })  
  
  # Output for Tab-4 i.e. employees
  output$stock <- renderText({
    "Show stock here"
  })  
}
