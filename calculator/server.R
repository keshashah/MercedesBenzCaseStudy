# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Output for Tab-0 i.e. industry
  #Output for Tab-0 i.e. Industry
  output$industry <- renderText({
    "According to Recent Study, 10-point increase in turnover costs avg dealership €500,000 in gross profit annually.
    Multiplied by about 16,500 dealerships in the entire Europe, it's an €8 billion-plus problem !!"
  })
  
  data<-read.table(text=
                     "sal rev attr
                   4  66  0
                   4.25  65  5
                   4.75  64  10
                   5  62  15
                   5.75  60  20
                   6  57  25
                   5.5  53  30
                   5  49  35
                   4.5  45  40
                   4.25  42  45
                   4  39  50
                   3.5  37  55
                   3  34  60
                   2.5  30  65
                   2  26  70
                   1.5  22  75
                   1  18  80
                   0.7  14  85
                   0.5  10  90
                   0.3   5  95
                   0   0  100", header=T)
  
  output$industry1 <- renderPlot({
    twoord.stackplot(lx=data$attr, rx=data$attr, 
                     ldata=data$sal,
                     rdata=data$rev,
                     lcol="blue",
                     rcol="red", 
                     ltype="o",
                     rtype="l",
                     lylab="Monthly Cars Sold/Sales Employee", rylab="Annual Dealer's Revenue (million €)", 
                     xlab="Sales Employee Attrition Rate",
                     main="Industry-wide Dealer's Throughput(sales/employee) and Revenue graph vs Attrition of Sales Employees Graph.",
                     border="grey80",
                     leftfront = TRUE,
                     incrylim=0.5,
                     halfwidth=0.5
    )  
  })
  
  output$industry2 <- renderText({
    "Dealer's Profitability      = function1(Attrition Of Sales Employee);
    Attrition Of Sales Employee = function2(Voluntary Individual Termination, Car Sale Capacity, Non-Dealer Factors);
    Car Sale Capacity           = function3(Industry Downturn, Labor Market,  Government Regulations, Tax, Fuel price,
    Interest Rates, Insurance Costs, Emission Rules, Export/Import);
    Non-Dealer Factors          = fucntion4(Technology, New Cars Launched, Organizational Model Change);
    Voluntary Termination       = function5(Good Attrition, Bad Attrition);
    = function6(Fresher Attrition, Infant Attrition, 
    Key Employees Attrition, Non-performer Attrition);"
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
    paste("The attrition change from 40% to", input$attritionRate, "%.
          Thus a change in attrition amongst sales employees by ",input$attritionRate-40,"% has a direct impact on 
          market sentiments and thus stock price of Mercedes Benz")
  })  
  
  # Showing market sentiment ----
  output$stock1 <- renderGvis({
    df1 <- data.frame(Label = "Attrition Change", Value = input$attritionRate-40)
    gvisGauge(df1,
              options=list(min= -40 , max=60, greenFrom=-40,
                           greenTo=-10, yellowFrom=-9.99, yellowTo=20,
                           redFrom=20.1, redTo=60, width=300, height=300));  
    
  }) 
}
