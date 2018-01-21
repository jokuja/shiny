library(shiny)
library(shinydashboard)
library(ggplot2)
library(RSQLite)
library(shinyjs)
library(xts)
library(dygraphs)
library(V8)
library(DBI)

Nd2old = 0

#necessary for remote box-collapsing
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

sqlite <- dbConnect(SQLite(), "db.sqlite")

server <- function(input, output, session) {
  
  N_D_1 <- 0
  
  observeEvent(input$ab_Initial_Pricing, {
    js$collapse("box_Do")
    hide(id = "box_Initial_Pricing", anim = FALSE)
    
    temp_db_Stock_Derivative_Static <-
      cbind.data.frame(
        input$ti_Type_Of_Stock_Derivative,
        input$ti_Stock_ISIN,
        input$ti_Exercise_Or_Forward_Price,
        as.character(input$ti_Contracting_Date),
        as.character(input$ti_Expiration_Date),
        input$ti_Contract_Size,
        input$ti_Number_Of_Contracts,
        input$ti_Stock_Volatility,
        input$ti_Interest_Rate,
        input$ti_Mark_To_Model
      )
    names(temp_db_Stock_Derivative_Static) <-
      c(
        "Type_Of_Stock_Derivative",
        "Stock_ISIN",
        "Exercise_Or_Forward_Price",
        "Contracting_Date",
        "Expiration_Date",
        "Contract_Size",
        "Number_Of_Contracts",
        "Stock_Volatility",
        "Interest_Rate",
        "Mark_To_Model"
      )
    dbWriteTable(sqlite,
                 "Stock_Derivative_Static",
                 temp_db_Stock_Derivative_Static,
                 append = TRUE)
  })
  
  v2 <-reactiveValues(Nd1old=0)
  v2 <-reactiveValues(Nd2old=0)
  v2 <-reactiveValues(asset=0)
  v2 <-reactiveValues(liability=0)
  
  
  observeEvent(input$ab_Initial_Pricing2, { #initial writes into Stock_Pricing_Dynamic
    js$collapse("box_Do2")
    js$collapse("box_Check2")
    js$collapse("box_Act2")
    js$collapse("box_Plan2")
    hide(id = "box_Initial_Pricing2", anim = FALSE)
    
    temp_db_Stock_Derivative_Static <-
      cbind.data.frame(
        input$ti_Type_Of_Stock_Derivative2,
        input$ti_Stock_ISIN2,
        input$ti_Exercise_Or_Forward_Price2,
        as.character(input$ti_Contracting_Date2),
        as.character(input$ti_Expiration_Date2),
        input$ti_Contract_Size2,
        input$ti_Number_Of_Contracts2,
        input$ti_Stock_Volatility2,
        input$ti_Interest_Rate2,
        input$ti_Mark_To_Model2
      )
    names(temp_db_Stock_Derivative_Static) <-
      c(
        "Type_Of_Stock_Derivative",
        "Stock_ISIN",
        "Exercise_Or_Forward_Price",
        "Contracting_Date",
        "Expiration_Date",
        "Contract_Size",
        "Number_Of_Contracts",
        "Stock_Volatility",
        "Interest_Rate",
        "Mark_To_Model"
      )
    dbWriteTable(sqlite,
                 "Stock_Derivative_Static",
                 temp_db_Stock_Derivative_Static,
                 append = TRUE)
    
    ###
    # initial calculations
    ###
    X_0_t = as.numeric(input$ti_Exercise_Or_Forward_Price2)
    r_0_t = as.numeric(input$ti_Interest_Rate2)/ 100
    volatility = as.numeric(input$ti_Stock_Volatility2)/ 100
    # is not working -> 100 becomes 1
    #P_A_0 = as.integer(temp_db_Stock_Pricing_Dynamic$Stock_Price)
    P_A_0 = as.numeric(input$ti_Exercise_Or_Forward_Price2)
    
    # Time_to_maturity -> how to calculate? (inDays(End - Current)?) ##percentage of remaining time in weeks?
    T_0_t = 1
    
    #print(P_A_0/X_0_t)
    
    # d1
    d_1_0 = (log(P_A_0/X_0_t) + (r_0_t + (volatility^2)/2) * T_0_t)/(volatility*sqrt(T_0_t))
    # d2
    d_2_0 = (log(P_A_0/X_0_t) + (r_0_t - (volatility^2)/2) * T_0_t)/(volatility*sqrt(T_0_t))
    
    N_d_1 = pnorm(d_1_0)
    N_d_2 = pnorm(d_2_0)
    v2$Nd1old = N_d_1
    v2$Nd2old=N_d_2
    
    print(N_d_1)
    print(N_d_2)
    
    
    ASSET = P_A_0 * N_d_1
    LIABILITY = X_0_t * exp((-(r_0_t) * T_0_t)) * N_d_2
    
    # forward value
    FV = ASSET - LIABILITY
    
    #now store all data into tables
    
    #initial store into Stock_Pricing_Dynamic
    temp_db_Stock_Pricing_Dynamic2 <-
      cbind.data.frame(
        input$ti_Stock_ISIN2,
        input$ti_Exercise_Or_Forward_Price2,
        as.character(input$ti_Contracting_Date2)
      )
    
    names(temp_db_Stock_Pricing_Dynamic2) <-
      c("Stock_ISIN",
        "Stock_Price",
        "timestamp")
    
    dbWriteTable(sqlite,
                 "Stock_Pricing_Dynamic",
                 temp_db_Stock_Pricing_Dynamic2,
                 append = TRUE)
    
    
    
    #initial store into Derivative_Instrument_Dynamic 
    temp_Stock_Derivative_Static2 <-
      dbReadTable(sqlite, "Stock_Derivative_Static")
    
    temp_db_Derivative_Instrument_Dynamic <-
      cbind.data.frame(
        tail(temp_Stock_Derivative_Static2$Stock_Derivative_Static_ID, 1),
        as.character(input$ti_Contracting_Date2),
        as.character(FV)
      )
    names(temp_db_Derivative_Instrument_Dynamic) <-
      c("Stock_Derivative_Static_ID",
        "timestamp",
        "Fair_Value")
    dbWriteTable(
      sqlite,
      "Derivative_Instrument_Dynamic",
      temp_db_Derivative_Instrument_Dynamic,
      append = TRUE
    )
    
    #initial store into asset/liability/off_balance
    
    if (FV > 0) { #save as asset
      #Asset
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_asset <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Contracting_Date2),
          FV
        )
      names(temp_db_asset) <-
        c("Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Fair_Value")
      dbWriteTable(sqlite, "Asset", temp_db_asset, append = TRUE)
    } else if (tail(temp_db_draw$'Forward Value', 1) < 0) {#save as liability
      #Liability
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_liability <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Contracting_Date2),
          FV
        )
      names(temp_db_liability) <-
        c("Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Fair_Value")
      dbWriteTable(sqlite, "Liability", temp_db_liability, append = TRUE)
    }
    else {#save as off balance
      # Off_Balance
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_off_balance <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Contracting_Date2)
        )
      names(temp_db_off_balance) <-
        c("Derivative_Instrument_Dynamic_ID",
          "timestamp")
      dbWriteTable(sqlite, "Off_Balance", temp_db_off_balance, append = TRUE)
    }
    
    
    
    #initial into risky income
    temp_Derivative_Instrument_Dynamic <-
      dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
    
    isAsset=0
    if(FV > 0) {
      isAsset = 1
    }
    
    temp_db_Economic_Resource_Risky_Income <-
      cbind.data.frame(
        tail(
          temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
          1
        ),
        as.character(input$ti_Contracting_Date2),
        N_d_1, #this value isnt 1 for option call
        N_d_1*100,
        isAsset#maybe
      )
    names(temp_db_Economic_Resource_Risky_Income) <-
      c(
        "Derivative_Instrument_Dynamic_ID",
        "timestamp",
        "Nd1t",
        "Value",
        "Asset_Or_Liability"
      )
    dbWriteTable(
      sqlite,
      "Economic_Resource_Risky_Income",
      temp_db_Economic_Resource_Risky_Income,
      append = TRUE
    )
    
    
    #initial insert into Economic_Resource_Fixed_Income 
    temp_Derivative_Instrument_Dynamic <-
      dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
    temp_db_Economic_Resource_Fixed_Income <-
      cbind.data.frame(
        tail(
          temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
          1
        ),
        as.character(input$ti_Contracting_Date2),
        (N_d_1*100 - FV)*(-1),
        1#
      )
    names(temp_db_Economic_Resource_Fixed_Income) <-
      c(
        "Derivative_Instrument_Dynamic_ID",
        "timestamp",
        "Present_Value",
        "Asset_Or_Liability"
      )
    dbWriteTable(
      sqlite,
      "Economic_Resource_Fixed_Income",
      temp_db_Economic_Resource_Fixed_Income,
      append = TRUE
    )
    
  }) 
  #end of initial part
  
  
  
  #do button for forward pricing
  observeEvent(input$button_Do, { 
    temp_db_Stock_Pricing_Dynamic <-
      cbind.data.frame(
        input$ti_Stock_ISIN,
        input$ti_Do_Stock_Price,
        as.character(input$ti_Do_timestamp)
      )
    names(temp_db_Stock_Pricing_Dynamic) <-
      c("Stock_ISIN",
        "Stock_Price",
        "timestamp")
    dbWriteTable(sqlite,
                 "Stock_Pricing_Dynamic",
                 temp_db_Stock_Pricing_Dynamic,
                 append = TRUE)
    
    js$collapse("box_Plan")
  })
  
  
  observeEvent(input$button_Do2, {#do button for option call
    temp_db_Stock_Pricing_Dynamic <-
      cbind.data.frame(
        input$ti_Stock_ISIN2,
        input$ti_Do_Stock_Price2,
        as.character(input$ti_Do_timestamp2)
      )
    
    names(temp_db_Stock_Pricing_Dynamic) <-
      c("Stock_ISIN",
        "Stock_Price",
        "timestamp")
    dbWriteTable(sqlite,
                 "Stock_Pricing_Dynamic",
                 temp_db_Stock_Pricing_Dynamic,
                 append = TRUE)
    
    #js$collapse("box_Plan2")
  })
  
  
  #plan for forward pricing
  observeEvent(input$button_Plan, { 
    
    output$to_Plan <- renderText("N(d1) = 1")
    js$collapse("box_Check")
  })
  
  
  v2 <-reactiveValues(Nd1new = 1)
  v2 <-reactiveValues(liability = 0)
  v2 <-reactiveValues(Nd2new=0)
  
  observeEvent(input$button_Plan2, {  #plan for call option
    
    ###
    # calculate d1_t
    ###
    X_0_t = as.numeric(input$ti_Exercise_Or_Forward_Price2)#exercise price is always initial value
    r_0_t = as.numeric(input$ti_Interest_Rate2)/ 100
    volatility = as.numeric(input$ti_Stock_Volatility2)/ 100
    # is not working -> 100 becomes 1
    #P_A_0 = as.integer(temp_db_Stock_Pricing_Dynamic$Stock_Price)
    P_A_0 = as.numeric(input$ti_Do_Stock_Price2) #price of stock from button 'plan' input
    
    # Time_to_maturity -> how to calculate? (inDays(End - Current)?) 
    #get date from dynamic
    
    pending_weeks <- 
      as.numeric(difftime(
        as.Date(input$ti_Expiration_Date2),
        as.Date(input$ti_Do_timestamp2),
        unit = "weeks"
      ))
    
    T_0_t <- round(pending_weeks / 52.1775,2)
    
    # d1
    d_1_0 = (log(P_A_0/X_0_t) + (r_0_t + (volatility^2)/2) * T_0_t)/(volatility*sqrt(T_0_t))
    # d2
    d_2_0 = (log(P_A_0/X_0_t) + (r_0_t - (volatility^2)/2) * T_0_t)/(volatility*sqrt(T_0_t))
    #for final case - zero division
    
    print("d_1_0")
    print(d_1_0)
    
    
    N_d_1 = pnorm(d_1_0)
    v2$Nd1new=N_d_1
    
    N_d_2 = pnorm(d_2_0)#little bit different 
    v2$Nd2new=N_d_2
    
    if (T_0_t == 0)
    {
      d_1_0 = 0
      d_2_0 = 0
      N_d_1 = 0
      N_d_1 = 0
      v2$Nd2new = 0
      v2$Nd1new = 0
      
    }
    
    ASSET = P_A_0 * N_d_1
    LIABILITY = X_0_t * exp((-(r_0_t) * T_0_t)) * N_d_2
    v2$liability=LIABILITY
    
    # forward value
    
    
    FV = ASSET - LIABILITY
    
    N_d_1r = round(N_d_1*100,2)#just to print nice value
    nd1string = paste0("N(d1) = ",N_d_1r,"%" , collapse = NULL)
    #nd1string = nd1string + as.character(N_d_1)
    
    output$to_Plan2 <- renderText(nd1string)
    

    #renderText("N(d1) =", str(N_d_1))
    #js$collapse("box_Check2")
  })
  
  #https://stackoverflow.com/questions/19611254/r-shiny-disable-able-shinyui-elements
  
  
  #check button for forward pricing
  observeEvent(input$button_Check, { 
    output$to_Check <- renderText("Delta N(d1) = 0")
    js$collapse("box_Act")
  })
  
  v2 <- reactiveValues(N_diff=0)
  v2 <- reactiveValues(change=0) #when 0 no change needed, when 1 propose change
  
  
  #check button for call option
  observeEvent(input$button_Check2, {
    
    temp_db_Economic_Resource_Risky_Income <-
      dbReadTable(sqlite, "Economic_Resource_Risky_Income")
    
    N_d1_old <- tail(temp_db_Economic_Resource_Risky_Income$Nd1t,1)
    
    N_diff <- v2$Nd1new - N_d1_old
    

    
      v2$N_diff = N_diff
    
    
    if ( N_diff  != 0)
    {
      v2$change = 1 #if i want rebalance of portfolio
    }
  
    if (v2$Nd1new == 0) {
      N_diff = 0
    }
    N_diffr = round(N_diff*100,2)
    output$to_Check2 <- renderText(paste("delta(Nd1) = ",N_diffr,"%"))
    
    
    
    #js$collapse("box_Act2")
  })
  
  
  #Act button for forward pricing
  observeEvent(input$button_Act, { 
    output$to_Act <- renderText("Forward: No action possible")
    v$doCalcAndPlot <- input$button_Act #CalcAndPlot
  })
  
  #Act button for call option
  observeEvent(input$button_Act2, {
    output$to_Act2 <- renderText(paste("PF value = ",round(FV,2)))
    v2$doCalcAndPlot2 <- input$button_Act2 #CalcAndPlot
    
    
    temp_Stock_Derivative_Static2 <-
      dbReadTable(sqlite, "Stock_Derivative_Static")
    
    X_0_t = as.numeric(input$ti_Exercise_Or_Forward_Price2)#exercise price is always initial value
    r_0_t = as.numeric(input$ti_Interest_Rate2)/ 100
    volatility = as.numeric(input$ti_Stock_Volatility2)/ 100
    # is not working -> 100 becomes 1
    #P_A_0 = as.integer(temp_db_Stock_Pricing_Dynamic$Stock_Price)
    P_A_0 = as.numeric(input$ti_Do_Stock_Price2) #price of stock from button 'plan' input
    
    # Time_to_maturity -> how to calculate? (inDays(End - Current)?) 
    #get date from dynamic
    temp_Stock_Derivative_Dynamic2 <-
      dbReadTable(sqlite, "Stock_Pricing_Dynamic")#timestamp of previous rebalance
    
    pending_weeks <- 
      as.numeric(difftime(
        as.Date(input$ti_Expiration_Date2),
        as.Date(input$ti_Do_timestamp2),
        unit = "weeks"
      ))
    
    
    T_0_t <- round(pending_weeks / 52.1775,2)
    
    
    # d1
    d_1_0 = (log(P_A_0/X_0_t) + (r_0_t + (volatility^2)/2) * T_0_t)/(volatility*sqrt(T_0_t))
    # d2
    d_2_0 = (log(P_A_0/X_0_t) + (r_0_t - (volatility^2)/2) * T_0_t)/(volatility*sqrt(T_0_t))
    # print(d_1_0)
    
    
    
    N_d_1 = pnorm(round(d_1_0,4))
    
    N_d_2 = pnorm(round(d_2_0,4))
    v2$Nd2new = N_d_2
    #
    if (T_0_t == 0)
    {
      d_1_0 = 0
      d_2_0 = 0
      N_d_1 = 0
      N_d_1 = 0
      
    }
    print('nd1old')
    print(tail(v2$Nd1old,1))
    print('nd2old')
    print(tail(v2$Nd2old,1))
    
    print('nd1new')
    print(tail(v2$Nd1new,1))
    print('nd2new')
    print(tail(v2$Nd2new,1))
    
    ASSET = P_A_0 * v2$Nd1old
    LIABILITY = X_0_t * exp((-(r_0_t) * T_0_t)) * v2$Nd2old #before transaction
    
    # forward value
    FV = ASSET - LIABILITY
    
    ASSET = P_A_0 * v2$Nd1new #after transaction
    LIABILITY = ASSET - FV #after transaction
    
    v2$liability=LIABILITY
    v2$asset=ASSET
    
    v2$Nd2old = N_d_2
    Nd2old=N_d_2
    v2$Nd1old = N_d_1
    
    #  if (v2$change == 1)#if rebalance needed store into database
    
    {
      #act2 store into Derivative_Instrument_Dynamic 
      temp_Stock_Derivative_Static2 <-
        dbReadTable(sqlite, "Stock_Derivative_Static")
      
      temp_db_Derivative_Instrument_Dynamic2 <-
        cbind.data.frame(
          tail(temp_Stock_Derivative_Static2$Stock_Derivative_Static_ID, 1),
          as.character(input$ti_Do_timestamp2),
          as.character(FV)
        )
      names(temp_db_Derivative_Instrument_Dynamic2) <-
        c("Stock_Derivative_Static_ID",
          "timestamp",
          "Fair_Value")
      
      dbWriteTable(
        sqlite,
        "Derivative_Instrument_Dynamic",
        temp_db_Derivative_Instrument_Dynamic2,
        append = TRUE
      )
      
      #initial store into asset/liability/off_balance
      
      if (N_d_1 > 0) {
        #Asset
        temp_Derivative_Instrument_Dynamic2 <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_asset <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic2$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp2),
            FV
          )
        names(temp_db_asset) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(sqlite, "Asset", temp_db_asset, append = TRUE)
      } else if (N_d_1 < 0) {
        #Liability
        temp_Derivative_Instrument_Dynamic2 <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_liability <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic2$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp2),
            FV
          )
        names(temp_db_liability) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(sqlite, "Liability", temp_db_liability, append = TRUE)
      }
      else {
        # Off_Balance
        temp_Derivative_Instrument_Dynamic2 <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_off_balance <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic2$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp2)
            
          )
        names(temp_db_off_balance) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp")
        dbWriteTable(sqlite, "Off_Balance", temp_db_off_balance, append = TRUE)
      }
      
      
      
      #initial into risky income
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      
        isAsset=0
        if(FV > 0) {
          isAsset = 1
        }
          
      
      temp_db_Economic_Resource_Risky_Income <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Do_timestamp2),
          
          N_d_1, #this value isnt 1 for option call
          N_d_1*P_A_0, #value of stock
          isAsset#maybe
        )
      names(temp_db_Economic_Resource_Risky_Income) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Nd1t",
          "Value",
          "Asset_Or_Liability"
        )
      dbWriteTable(
        sqlite,
        "Economic_Resource_Risky_Income",
        temp_db_Economic_Resource_Risky_Income,
        append = TRUE
      )
      
      
      #initial insert into Economic_Resource_Fixed_Income 
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_Economic_Resource_Fixed_Income <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Do_timestamp2),
          
          (N_d_1*P_A_0 - FV)*(-1),
          1#
        )
      names(temp_db_Economic_Resource_Fixed_Income) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Present_Value",
          "Asset_Or_Liability"
        )
      dbWriteTable(
        sqlite,
        "Economic_Resource_Fixed_Income",
        temp_db_Economic_Resource_Fixed_Income,
        append = TRUE
      )
      #end of storing in ACT2
      
      
    }
    
  })
  
  
  observeEvent(input$button_Act_Continue, {
    js$collapse("box_Act")
    js$collapse("box_Plan")
    js$collapse("box_Check")
    
    output$to_Plan <- renderText("")
    output$to_Check <- renderText("")
    output$to_Act <- renderText("")
    
  })
  
  observeEvent(input$button_Act_Continue2, {
   #js$collapse("box_Act2")
    #js$collapse("box_Plan2")
    #js$collapse("box_Check2")
    
    output$to_Plan2 <- renderText("")
    output$to_Check2 <- renderText("")
    output$to_Act2 <- renderText("")
    
  })
  
  
  observeEvent(input$reset_db, {
    dbSendStatement(sqlite, "DELETE from Stock_Derivative_Static")
    dbSendStatement(sqlite, "DELETE from Stock_Pricing_Dynamic")
    dbSendStatement(sqlite, "DELETE from Derivative_Instrument_Dynamic")
    dbSendStatement(sqlite, "DELETE from Economic_Resource_Risky_Income")
    dbSendStatement(sqlite, "DELETE from Economic_Resource_Fixed_Income")
    dbSendStatement(sqlite, "DELETE from Asset")
    dbSendStatement(sqlite, "DELETE from Liability")
    dbSendStatement(sqlite, "DELETE from Off_Balance")
    
  })
  
  v2 <- reactiveValues(doCalcAndPlot2 = FALSE) #recalc and redraw
  
  output$timeline2 <- renderDygraph({
    if (v2$doCalcAndPlot2 == FALSE)
      return()
    
    isolate({
      
      temp_db_draw2 <- dbReadTable(sqlite, "Stock_Pricing_Dynamic")
      temp_db_draw2$Pricing_Date <-
        as.Date(as.POSIXct(temp_db_draw2$timestamp))
      
      #   print(tail(v2$liability,1))
      #  print(tail(v2$N_diff,1))
      #  print(tail(temp_db_draw2$Stock_Price,1))
      
      temp_db_draw2$F_Price <- tail((v2$liability + v2$N_diff*temp_db_draw2$Stock_Price),1)#exercise price 
      
      temp_db_draw2$Liability <-v2$liability
      temp_db_draw2$Asset <- v2$asset
      
      temp_db_draw2$'Forward Value' <-
        temp_db_draw2$F_Price
      
      #print(tail(temp_db_draw2$F_Price,1))
      
      #Composing XTS
      temp_xts_draw2 <-
        xts(x = temp_db_draw2[, c("Asset", "Liability", "Forward Value")]
            , order.by =
              temp_db_draw2[, 5])
      
      
      
      #Plotting XTS
      dygraph(temp_xts_draw2) %>%
        dyRangeSelector()
      
      
    })
  })
  
  v <- reactiveValues(doCalcAndPlot = FALSE) #recalc and redraw
  
  output$timeline <- renderDygraph({
    if (v$doCalcAndPlot == FALSE)
      return()
    isolate({
      temp_db_draw <- dbReadTable(sqlite, "Stock_Pricing_Dynamic")
      temp_db_draw$Pricing_Date <-
        as.Date(as.POSIXct(temp_db_draw$timestamp))
      
      #legacy calc 
      #time to maturity
      temp_db_draw$TtM <- 
        as.numeric(difftime(
          as.Date(isolate(input$ti_Expiration_Date)),
          as.Date(temp_db_draw$Pricing_Date),
          unit = "weeks"
        )) / 52.1775
      
      #interest rate from input
      temp_db_draw$Interest_Rate <-
        as.numeric(input$ti_Interest_Rate) / 100
      
      #interest rate log
      temp_db_draw$Interest_Rate_Cont <-
        log(1 + temp_db_draw$Interest_Rate)
      
      temp_db_draw$F_Price <-
        temp_db_draw[1, 3] * (1 + as.numeric(input$ti_Interest_Rate) / 100) ^ (as.numeric(difftime(
          as.Date(input$ti_Expiration_Date),
          as.Date(input$ti_Contracting_Date),
          unit = "weeks"
        )) / 52.1775)
      
      temp_db_draw$Liability <-
        -temp_db_draw$F_Price * exp(-temp_db_draw$Interest_Rate_Cont * temp_db_draw$TtM)
      
      temp_db_draw$Asset <- temp_db_draw$Stock_Price
      
      temp_db_draw$'Forward Value' <-
        round(temp_db_draw$Liability + temp_db_draw$Stock_Price, 1)
      
      #Composing XTS
      temp_xts_draw <-
        xts(x = temp_db_draw[, c("Asset", "Liability", "Forward Value")], order.by =
              temp_db_draw[, 5])
      
      
      #Derivative_Instrument_Dynamic entry
      temp_Stock_Derivative_Static <-
        dbReadTable(sqlite, "Stock_Derivative_Static")
      
      
      if (tail(temp_Stock_Derivative_Static$Type_Of_Stock_Derivative, 1) == 0 ) 
      {
        temp_db_Derivative_Instrument_Dynamic <-
          cbind.data.frame(
            tail(temp_Stock_Derivative_Static$Stock_Derivative_Static_ID, 1),
            as.character(input$ti_Do_timestamp),
            tail(temp_db_draw$'Forward Value', 1)
          )
        names(temp_db_Derivative_Instrument_Dynamic) <-
          c("Stock_Derivative_Static_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(
          sqlite,
          "Derivative_Instrument_Dynamic",
          temp_db_Derivative_Instrument_Dynamic,
          append = TRUE
        )
      }
      
      #Economic_Resource_Risky_Income entry
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      
      
      temp_db_Economic_Resource_Risky_Income <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Do_timestamp),
          1, #this value isnt 1 for option call
          tail(temp_db_draw$'Asset', 1),
          1#
        )
      names(temp_db_Economic_Resource_Risky_Income) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Nd1t",
          "Value",
          "Asset_Or_Liability"
        )
      dbWriteTable(
        sqlite,
        "Economic_Resource_Risky_Income",
        temp_db_Economic_Resource_Risky_Income,
        append = TRUE
      )
      
      
      #Economic_Resource_Fixed_Income entry
      temp_Derivative_Instrument_Dynamic <-
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      temp_db_Economic_Resource_Fixed_Income <-
        cbind.data.frame(
          tail(
            temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
            1
          ),
          as.character(input$ti_Do_timestamp),
          tail(temp_db_draw$'Liability', 1),
          1#
        )
      names(temp_db_Economic_Resource_Fixed_Income) <-
        c(
          "Derivative_Instrument_Dynamic_ID",
          "timestamp",
          "Present_Value",
          "Asset_Or_Liability"
        )
      dbWriteTable(
        sqlite,
        "Economic_Resource_Fixed_Income",
        temp_db_Economic_Resource_Fixed_Income,
        append = TRUE
      )
      
      #Asset, Liability of Off Balance
      if (tail(temp_db_draw$'Forward Value', 1) > 0) {
        #Asset
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_asset <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp),
            tail(temp_Derivative_Instrument_Dynamic$Fair_Value, 1)
          )
        names(temp_db_asset) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(sqlite, "Asset", temp_db_asset, append = TRUE)
      } else if (tail(temp_db_draw$'Forward Value', 1) < 0) {
        #Liability
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_liability <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp),
            tail(temp_Derivative_Instrument_Dynamic$Fair_Value, 1)
          )
        names(temp_db_liability) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp",
            "Fair_Value")
        dbWriteTable(sqlite, "Liability", temp_db_liability, append = TRUE)
      }
      else {
        # Off_Balance
        temp_Derivative_Instrument_Dynamic <-
          dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
        temp_db_off_balance <-
          cbind.data.frame(
            tail(
              temp_Derivative_Instrument_Dynamic$Derivative_Instrument_Dynamic_ID,
              1
            ),
            as.character(input$ti_Do_timestamp)
          )
        names(temp_db_off_balance) <-
          c("Derivative_Instrument_Dynamic_ID",
            "timestamp")
        dbWriteTable(sqlite, "Off_Balance", temp_db_off_balance, append = TRUE)
      }
      
      #Plotting XTS
      dygraph(temp_xts_draw) %>%
        dyRangeSelector()
    })
  })
  
  observeEvent(
    input$load_table_Stock_Pricing_Dynamic,
    output$table_Stock_Pricing_Dynamic <- renderDataTable({
      dbReadTable(sqlite, "Stock_Pricing_Dynamic")
    })
  )
  
  observeEvent(
    input$load_table_Stock_Information_Static,
    output$table_Stock_Information_Static <- renderDataTable({
      dbReadTable(sqlite, "Stock_Information_Static")
    })
  )
  
  observeEvent(
    input$load_table_Stock_Derivative_Static,
    output$table_Stock_Derivative_Static <-
      renderDataTable({
        dbReadTable(sqlite, "Stock_Derivative_Static")
      })
  )
  
  observeEvent(
    input$load_table_Derivative_Instrument_Dynamic,
    output$table_Derivative_Instrument_Dynamic <-
      renderDataTable({
        dbReadTable(sqlite, "Derivative_Instrument_Dynamic")
      })
  )
  
  observeEvent(
    input$load_table_Economic_Resource_Risky_Income,
    output$table_Economic_Resource_Risky_Income <-
      renderDataTable({
        dbReadTable(sqlite, "Economic_Resource_Risky_Income")
      })
  )
  
  observeEvent(
    input$load_table_Economic_Resource_Fixed_Income,
    output$table_Economic_Resource_Fixed_Income <-
      renderDataTable({
        dbReadTable(sqlite, "Economic_Resource_Fixed_Income")
      })
  )
  
  observeEvent(input$load_table_Asset,
               output$table_Asset <- renderDataTable({
                 dbReadTable(sqlite, "Asset")
               }))
  
  observeEvent(input$load_table_Liability,
               output$table_Liability <- renderDataTable({
                 dbReadTable(sqlite, "Liability")
               }))
  
  observeEvent(input$load_table_Off_Balance,
               output$table_Off_Balance <- renderDataTable({
                 dbReadTable(sqlite, "Off_Balance")
               }))
}
