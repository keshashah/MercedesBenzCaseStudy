# Define server logic required --
server <- function(input, output) {
  
  # To turn off numeric output in scientific notations.. View whole numbers.
  options(scipen=999)
  
  # Output for Tab-0 i.e. industry
  output$industry <- renderText({
    "According to Recent Study, 10-point increase in turnover costs avg dealership €500,000 in gross profit annually.
    Multiplied by about 16,500 dealerships in the entire Europe, it's an €8 billion-plus problem !!"
  })
  
  industrydata<-read.table(text=
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
    twoord.stackplot(lx=industrydata$attr, rx=industrydata$attr, 
                     ldata=industrydata$sal,
                     rdata=industrydata$rev,
                     lcol="blue",
                     rcol="red", 
                     ltype="o",
                     rtype="l",
                     lylab="Average Monthly Cars Sold/Sales Employee", rylab="Annual Dealer's Revenue (billion Rs.)", 
                     xlab="Sales Employee Attrition Rate (%)",
                     main="Source: Study from Globe Ecologistics Pvt. Ltd., India",
                     border="grey80",
                     leftfront = TRUE,
                     incrylim=0.5,
                     halfwidth=0.5
    )  
  })
  
  output$industry2 <- renderText({
    "Dealer's Profitability      = f(Attrition Of Sales Employee);
    Attrition Of Sales Employee = f(Voluntary Individual Termination, Car Sale Capacity, Non-Dealer Factors);
    Car Sale Capacity           = f(Industry Downturn, Labor Market,  Government Regulations, Tax, Fuel price,
    Interest Rates, Insurance Costs, Emission Rules, Export/Import);
    Non-Dealer Factors          = f(Technology, New Cars Launched, Organizational Model Change);
    Voluntary Termination       = f(Good Attrition, Bad Attrition);
    = f(Fresher Attrition, Infant Attrition, 
    Key Employees Attrition, Non-performer Attrition);"
  })
  
  # Output for Tab-1 i.e. bottomline
  output$bottomline <- renderText({
    "Attrition has a major fall-out on the bottom-line. 
    Bottom-Line = function(Hard costs, Soft costs);
    
    Hard costs are quantifiable costs related to hiring and training expenses, which includes expenses incured 
    from creating new position, posting advertisement, checking resume of applicants, interviewing candidates 
    to onbarding and training selected candidates.
    
    Soft cost are difficult to quantify as they are due to the lost opportunity cost and includes
    lost sales, unanswered customer calls, decrease in customer satisfaction index, morale of remaining employees
    down, productivity loss, etc. A conservative estimate of soft cost by experts is twice that of hard costs."
  })  
  
  output$bottomline1 <- renderText({
    paste('<span style=\"color:', ifelse(input$turnovercost*150*30*(0.4-input$attritionRate/100) < 0, "red", "green") , '\"> Change of Attrition Rate from 40 % to ',input$attritionRate,'% <br>
          will result in change of bottom-line by €',input$turnovercost*150*30*(0.4-input$attritionRate/100),'<br><br>
          As per Diamler Group 2018 Financial Report, it will impact <br>Return on Sales by ',round(input$turnovercost*150*30*(0.4-input$attritionRate/100)/10824000*7.8,digits=2),'% </span>')
  })
  
  output$bottomline2 <- renderText({
    paste('Brief Calculation:
          
          At an average dealership:
          Cost of 1 employee turnover                = €',input$turnovercost,'
          Average number of sales employees          = 30  
          Number of Mercedes Benz Dealers in Germany     = 150
          
          At 40 % attrition rate, the cost of attrition  = €',as.numeric(input$turnovercost*150*30*0.4),'
          At',input$attritionRate,'% attrition rate, the cost of attrition  = €',input$turnovercost*150*30*input$attritionRate/100
    )
  })
  
  # Output for Tab-2 i.e. sales
  output$sales2 <- renderText({
    "Knowing conversion rate in sales funnel, profit per sale and sales commision paid to employees as expense, 
    we can calculate the return on investment(ROI) for retaining a sales employee.
    
    Return on Investment (ROI)=  (Profit from Sales – Expense Incurred for Sale) / Expense Incurred for Sale;
    where 
    Expense Incurred for Sale = function1(Fixed cost, Variable cost)
    Variable cost = function2(Salary of sales employees = minimum fixed base wage + variable sales commisions)"
  }) 
  
  output$sales3 <- renderText({
    paste('<span style=\"color:', ifelse(0.078*(40-input$attritionRate) < 0, "red", "green") , '\"> Change in ROI = ',0.078*(40-input$attritionRate),'% when attrition changes from 40 % to ',input$attritionRate,'%, thus ',ifelse(0.078*(40-input$attritionRate) < 0, "NEGATIVELY", "POSITIVELY"),
          ' impacting dealer profitability.</span>')
  })  
  
  output$sales <- renderText({
    "Change in attrition rate of sales employees impacts the sales twofold - 1. number of leads 2. conversion to sales. 
    A decrease(increase) in attrition rate will increase(decreases) sales by attracting(losing) more leads as well as
    doing a better(poor) job of converting existing leads to successful sales."
  })  
  
  salesFunneldat <- reactiveValues(df_data = data.frame(c(0,0,0,0),c(0,0,0,0),c(0,0,0,0)))
  
  observeEvent(c(input$attritionRate,
                 60, 25, 70, input$salerate*input$salerate/100),{
                   salesFunneldat$df_data <- data.frame(ZeroAttrition = c(round(1*60,digits = 3), round(1*60*25/100,digits=3), round(1*60*25/100*70/100,digits = 3), round(1*60*25/100*70/100*input$salerate*input$salerate/100/100,digits = 3)), 
                                                        OldAttrition = c(round(0.6*60,digits = 3), round(0.6*60*25/100,digits=3), round(0.6*60*25/100*70/100,digits = 3), round(0.6*60*25/100*70/100*input$salerate*input$salerate/100/100,digits = 3)), 
                                                        NewAttrition = c(round(round((1-(input$attritionRate/100)),digits=3)*60,digits = 3), round(round((1-(input$attritionRate/100)),digits=3)*60*25/100,digits=3), round(round((1-(input$attritionRate/100)),digits=3)*60*25/100*70/100,digits = 3), round(round((1-(input$attritionRate/100)),digits=3)*60*25/100*70/100*input$salerate*input$salerate/100/100,digits = 3)))
                   
                   row.names(salesFunneldat$df_data) <- c("% Leads", "% Prospect", "% Test Drive", "% Sales Made")
                   
                   renderRadarChart("sales1", data = salesFunneldat$df_data, shape = "circle", line.width = 5, theme = "shine")
                 })
  
  # Output for Tab-3 i.e. customers
  output$customers <- renderText({
    paste('Customers trust salesperson who has in-depth knowledge of all models and have participated in long sales-cycle.
          Studies have shown that even most loyal customers may see sales employee departure as a reason to consider 
          competitive offereing.
          
          Mercedes Benz is trying to reduce the reliance on a particular sales employee by-
          1. Asking dealers to having multiple connections between potential customers and sales employees. 
          2. Having centralized platform for capturing customer preferences and tracking sale pipeline.
          Thus it tries to ensure essential information is not lost with departing employee.
          
          While this works for Cold leads, "Hot leads" are still severly impacted by attrition of sales employees as below:')
  })  
  
  CustomerType <- c("New Leads", "Customers Retained", "Lost Prospects", "Lost Sales")
  
  output$customers1 <- renderPlotly({
    plot_ly(data=data.frame(CustomerType, 
                            c(30*0.6*60, 
                              30*0.6*60*25/100*70/100*input$salerate*input$salerate/100/100, 
                              30*0.4*60*25/100, 
                              30*0.4*60*25/100*70/100*input$salerate*input$salerate/100/100 ), 
                            c(30*round((1-(input$attritionRate/100)),digits=2)*60, 
                              30*round((1-(input$attritionRate/100)),digits=2)*60*25/100*70/100*input$salerate*input$salerate/100/100,
                              30*round((input$attritionRate/100),digits=2)*60*25/100,
                              30*round((input$attritionRate/100),digits=2)*60*25/100*70/100*input$salerate*input$salerate/100/100)), 
            x = CustomerType, 
            y = c(30*0.6*60, 
                  30*0.6*60*25/100*70/100*input$salerate*input$salerate/100/100,
                  30*0.4*60*25/100, 
                  30*0.4*60*25/100*70/100*input$salerate*input$salerate/100/100 ), 
            type = 'bar', name = '40 % Attrition Rate') %>%
      
      add_trace(y = c(30*round((1-(input$attritionRate/100)),digits=2)*60,
                      30*round((1-(input$attritionRate/100)),digits=2)*60*25/100*70/100*input$salerate*input$salerate/100/100,
                      30*round((input$attritionRate/100),digits=2)*60*25/100,
                      30*round((input$attritionRate/100),digits=2)*60*25/100*70/100*input$salerate*input$salerate/100/100), 
                name = 'New Attrition Rate') %>%
      
      layout(yaxis = list(title = 'Monthly Customers/Dealer',type='log'), barmode = 'group')
  })  
  
  # Output for Tab-4 i.e. employees
  output$employees <- renderText({
    "The attrition rate among organizations is contagious and triggers chain reaction.
    A high attrition rate will lead to more people leaving the organization, while
    lower attrition will act as retention strategy.
    
    1. Co-employees need to cope-up and work overtime to maintain the same productivity for dealer and sell more cars.
    2. Existing employees are more aware of outside job opportunities from network of former colleagues."
  })  
  
  output$employees1 <- renderText({
    paste('<span style=\"color:', 
          ifelse(input$attritionRate > 40, "red", "green")
          , '\"> Change of Attrition Rate from 40 % to ',input$attritionRate,'%
          MAGNIFY Attrition Rate to',
          ifelse(
            input$attritionRate > 40, 
            min(100,round(input$attritionRate + (100*(input$attritionRate-40)/(100-input$attritionRate)/(100-input$attritionRate)),digits=0)),
            max(0,round(input$attritionRate + (100*(input$attritionRate-40)/(input$attritionRate*input$attritionRate)),digits=0))
          )
          ,'% soon.<br></span>
          <br>Thoughts of remaining employees: </h3>')
  })
  
  sample_employee_emotion <- data.frame(name = c("Job satisfaction", "Salary", "Perks", "Work environment",
                                                 "Co-employees", "relation", "Work Pressure", "Family situation","Health condition",
                                                 "Competency", "Tenure", "Poor Motivation", "Insurance", "Peer",
                                                 "Attrition","Performers", "new ideas", "competition", "manager",
                                                 "Workload","Stress","Process","Job","Organization","Responsibilities",
                                                 "mismatch","dissatisfaction","appreciation","management","overwork",
                                                 "Work-Life","Balance","Trust","Support","Opportunities","Job Hunt"),
                                        value = c(10, 8, 5, 7, 6, 8, 9, 3, 2, 5, 6, 11, 6,15,12,13,3,11, 14,15,
                                                  12, 8,6,9,13,10,7,8,9,10,14,11,13,10,15,15))
  
  # Radar Chart
  observeEvent(input$attritionRate, renderWordcloud("wc_div", data = sample_employee_emotion, shape = 'circle',
                                                    grid_size = 5, sizeRange = c(10, 30)))
  
  # Output for Tab-4 i.e. employees
  output$stock <- renderText({
    paste("The attrition change from 40% to", input$attritionRate, "%.
          Thus a change in attrition amongst sales employees by ",input$attritionRate-40,"% has a direct impact on 
          market sentiments and thus stock price of Mercedes Benz.")
  })  
  
  output$stock1 <- renderText({
    paste('Market Sentiment Meter: <span style=\"color:', 
          ifelse(input$attritionRate < 30, "green", ifelse(input$attritionRate < 60,"Orange","red"))
          ,'\">',
          ifelse(input$attritionRate < 30, "Positive", ifelse(input$attritionRate < 60,"Neutral","Negative"))
          ,'</span>')
  })
  
  # Showing market sentiment ----
  output$stock2 <- renderGvis({
    df1 <- data.frame(Label = "Attrition Change", Value = input$attritionRate-40)
    gvisGauge(df1,
              options=list(min= -40 , max=60, greenFrom=-40,
                           greenTo=-10, yellowFrom=-9.99, yellowTo=20,
                           redFrom=20.1, redTo=60, width=300, height=300));  
  }) 
  }
