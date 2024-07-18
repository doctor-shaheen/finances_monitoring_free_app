# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(lubridate)
library(googlesheets4)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(plotly)
library(shinyjs)

# Set up Google Sheets authentication
gs4_auth(cache = ".secrets", email = "add_your_email")

# Google Sheet IDs
income_sheet_id <- "add_link_to_blank_google_sheet1"
account_sheet_id <- "add_link_to_blank_google_sheet1"
credentials_sheet_id <- "add_link_to_blank_google_sheet1" # Add manually your user name and password in the sheet in your drive to keep it save and personal

# Function to read data from Sheet 1 of Google Sheets
read_gs_data <- function(sheet_id) {
  tryCatch({
    data <- read_sheet(sheet_id, sheet = 1)
    if (nrow(data) == 0) {
      return(NULL)
    }
    return(data)
  }, error = function(e) {
    showNotification("Failed to read data from Google Sheet", type = "error")
    return(NULL)
  })
}

# Function to append data to Sheet 1 of Google Sheets
append_gs_data <- function(sheet_id, new_data) {
  existing_data <- read_gs_data(sheet_id)
  if (is.null(existing_data)) {
    combined_data <- new_data
  } else {
    combined_data <- bind_rows(existing_data, new_data)
  }
  tryCatch({
    sheet_write(combined_data, ss = sheet_id, sheet = 1)
  }, error = function(e) {
    showNotification("Failed to update Google Sheet", type = "error")
  })
}

# Function to update data in Sheet 1 of Google Sheets
update_gs_data <- function(sheet_id, updated_data) {
  tryCatch({
    sheet_write(updated_data, ss = sheet_id, sheet = 1)
  }, error = function(e) {
    showNotification("Failed to update Google Sheet", type = "error")
  })
}

# Initialize data
income_data <- read_gs_data(income_sheet_id)
personal_account_data <- read_gs_data(account_sheet_id)
credentials_data <- read_gs_data(credentials_sheet_id)

# Create empty dataframes if data is NULL
if (is.null(income_data)) {
  income_data <- data.frame(
    Date = as.Date(character()),
    Price_Per_Item = numeric(),
    Number_of_Items = numeric(),
    Total_Price = numeric(),
    Logistics = numeric(),
    Total_Earnings = numeric(),
    Currency = character(),
    Note = character(),
    Customers = character(),
    stringsAsFactors = FALSE
  )
}

if (is.null(personal_account_data)) {
  personal_account_data <- data.frame(
    Date = as.Date(character()),
    Account_Balance = numeric(),
    Deposit = numeric(),
    Spend = numeric(),
    Total = numeric(),
    Currency = character(),
    Details = character(),
    stringsAsFactors = FALSE
  )
}

if (is.null(credentials_data)) {
  credentials_data <- data.frame(
    Username = character(),
    Password = character(),
    stringsAsFactors = FALSE
  )
}

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body { background-color: #f4f6f9; }
      .login-panel { 
        max-width: 300px; 
        margin: 100px auto; 
        padding: 20px; 
        background-color: white; 
        border-radius: 5px; 
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
      }
    "))
  ),
  
  # Login UI
  div(id = "login",
      wellPanel(class = "login-panel",
                h2("Login", align = "center"),
                textInput("username", "Username"),
                passwordInput("password", "Password"),
                br(),
                actionButton("login", "Login", class = "btn-primary btn-block"),
                br()
      )
  ),
  
  # Main app UI (initially hidden)
  hidden(
    div(id = "main_app",
        dashboardPage(
          dashboardHeader(title = "Finance Monitor"),
          dashboardSidebar(
            sidebarMenu(
              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              menuItem("Income", tabName = "income", icon = icon("money-bill-wave")),
              menuItem("Personal Account", tabName = "account", icon = icon("piggy-bank")),
              menuItem("Reports", tabName = "reports", icon = icon("chart-line")),
              menuItem("Currency Converter", tabName = "converter", icon = icon("exchange-alt"))
            )
          ),
          dashboardBody(
            tabItems(
              # Dashboard tab
              tabItem(tabName = "dashboard",
                      fluidRow(
                        infoBoxOutput("total_income_box"),
                        infoBoxOutput("total_balance_box"),
                        box(
                          title = "Transfer Income to Account",
                          status = "success",
                          solidHeader = TRUE,
                          width = 4,
                          actionButton("transfer_income", "Transfer", class = "btn-success")
                        )
                      ),
                      fluidRow(
                        box(plotlyOutput("income_trend_plot"), width = 6),
                        box(plotlyOutput("balance_trend_plot"), width = 6)
                        
                      )
              ),
              
              # Income tab
              tabItem(tabName = "income",
                      fluidRow(
                        box(
                          title = "Add Income",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 12,
                          dateInput("income_date", "Date"),
                          numericInput("price_per_item", "Price Per Item", value = 0),
                          numericInput("number_of_items", "Number of Items", value = 1),
                          numericInput("logistics", "Logistics", value = 0),
                          selectInput("income_currency", "Currency", choices = c("USD", "EGP"), selected = "USD"),
                          textInput("note", "Note"),
                          textInput("customers", "Customers"),
                          actionButton("add_income", "Add Income", class = "btn-success")
                        ),
                        box(
                          title = "Income Data",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 12,
                          DTOutput("income_table")
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Income Plot",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 12,
                          plotlyOutput("income_plot")
                        )
                      )
              ),
              
              # Account tab
              tabItem(tabName = "account",
                      fluidRow(
                        box(
                          title = "Add Account Entry",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 12,
                          dateInput("account_date", "Date"),
                          numericInput("account_balance", "Account Balance", value = 0),
                          numericInput("deposit", "Deposit", value = 0),
                          numericInput("spend", "Spend", value = 0),
                          selectInput("account_currency", "Currency", choices = c("USD", "EGP"), selected = "USD"),
                          textInput("details", "Details"),
                          actionButton("add_account", "Add Account Entry", class = "btn-success")
                        ),
                        box(
                          title = "Account Data",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 12,
                          DTOutput("account_table")
                        )
                      ),
                      fluidRow(
                        box(
                          title = "Account Plot",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 12,
                          plotlyOutput("account_plot")
                        )
                      )
              ),
              
              # Reports tab
              tabItem(tabName = "reports",
                      fluidRow(
                        box(
                          title = "Income Report",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 6,
                          dateRangeInput("income_date_range", "Select Date Range"),
                          actionButton("generate_income_report", "Generate Report", class = "btn-primary"),
                          hr(),
                          textOutput("total_income_selected"),
                          plotlyOutput("income_report_plot")
                        ),
                        box(
                          title = "Account Report",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 6,
                          dateRangeInput("account_date_range", "Select Date Range"),
                          actionButton("generate_account_report", "Generate Report", class = "btn-primary"),
                          hr(),
                          textOutput("total_balance_selected"),
                          plotlyOutput("account_report_plot")
                        )
                      )
              ),
              
              # Currency Converter tab
              tabItem(tabName = "converter",
                      fluidRow(
                        box(
                          title = "Currency Converter",
                          status = "primary",
                          solidHeader = TRUE,
                          width = 6,
                          numericInput("amount", "Amount", value = 1),
                          selectInput("from_currency", "From", choices = c("USD", "EGP"), selected = "USD"),
                          selectInput("to_currency", "To", choices = c("EGP", "USD"), selected = "EGP"),
                          numericInput("exchange_rate", "Exchange Rate (USD-EGP)", value = NA),
                          actionButton("convert", "Convert", class = "btn-primary"),
                          hr(),
                          textOutput("conversion_result")
                        ),
                        hidden(
                          div(id = "rate_info",
                              box(
                                title = "Conversion Rate Info",
                                status = "info",
                                solidHeader = TRUE,
                                width = 6,
                                textOutput("rate_info_text")
                              )
                          )
                        )
                      )
              )
            )
          )
        )
    )
  )
)

# Server
server <- function(input, output, session) {
  observeEvent(input$login, {
    username <- input$username
    password <- input$password
    
    if (username %in% credentials_data$Username) {
      stored_password <- credentials_data$Password[credentials_data$Username == username]
      if (password == stored_password) {
        showNotification("Login successful", type = "message")
        hide("login")
        show("main_app")
      } else {
        showNotification("Invalid password", type = "error")
      }
    } else {
      showNotification("Invalid username", type = "error")
    }
  })
  
  observeEvent(input$add_income, {
    if (input$income_currency == "EGP") {
      showModal(modalDialog(
        title = "Currency Conversion",
        numericInput("egp_to_usd_rate", "Enter USD to EGP conversion rate", value = NA),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_income_conversion", "Confirm")
        )
      ))
      
      observeEvent(input$confirm_income_conversion, {
        conversion_rate <- 1/input$egp_to_usd_rate
        converted_price_per_item <- input$price_per_item * conversion_rate
        converted_logistics <- input$logistics * conversion_rate
        
        new_income <- data.frame(
          Date = as.Date(input$income_date),
          Price_Per_Item = converted_price_per_item,
          Number_of_Items = input$number_of_items,
          Total_Price = converted_price_per_item * input$number_of_items,
          Logistics = converted_logistics,
          Total_Earnings = (converted_price_per_item * input$number_of_items) - converted_logistics,
          Currency = "USD",
          Note = input$note,
          Customers = input$customers,
          stringsAsFactors = FALSE
        )
        
        append_gs_data(income_sheet_id, new_income)
        
        income_data <<- read_gs_data(income_sheet_id)
        output$income_table <- renderDT({
          datatable(income_data, options = list(pageLength = 10))
        })
        
        showNotification("Income entry added and converted to USD", type = "message")
        removeModal()
      })
      
    } else {
      new_income <- data.frame(
        Date = as.Date(input$income_date),
        Price_Per_Item = input$price_per_item,
        Number_of_Items = input$number_of_items,
        Total_Price = input$price_per_item * input$number_of_items,
        Logistics = input$logistics,
        Total_Earnings = (input$price_per_item * input$number_of_items) - input$logistics,
        Currency = "USD",
        Note = input$note,
        Customers = input$customers,
        stringsAsFactors = FALSE
      )
      
      append_gs_data(income_sheet_id, new_income)
      
      income_data <<- read_gs_data(income_sheet_id)
      output$income_table <- renderDT({
        datatable(income_data, options = list(pageLength = 10))
      })
      
      showNotification("Income entry added", type = "message")
    }
  })
  
  observeEvent(input$add_account, {
    if (input$account_currency == "EGP") {
      showModal(modalDialog(
        title = "Currency Conversion",
        numericInput("egp_to_usd_rate_account", "Enter USD to EGP conversion rate", value = NA),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_account_conversion", "Confirm")
        )
      ))
      
      observeEvent(input$confirm_account_conversion, {
        conversion_rate <- 1/input$egp_to_usd_rate_account
        converted_account_balance <- input$account_balance * conversion_rate
        converted_deposit <- input$deposit * conversion_rate
        converted_spend <- input$spend * conversion_rate
        
        new_account <- data.frame(
          Date = as.Date(input$account_date),
          Account_Balance = converted_account_balance,
          Deposit = converted_deposit,
          Spend = converted_spend,
          Total = converted_account_balance + converted_deposit - converted_spend,
          Currency = "USD",
          Details = input$details,
          stringsAsFactors = FALSE
        )
        
        append_gs_data(account_sheet_id, new_account)
        
        personal_account_data <<- read_gs_data(account_sheet_id)
        output$account_table <- renderDT({
          datatable(personal_account_data, options = list(pageLength = 10))
        })
        
        showNotification("Account entry added and converted to USD", type = "message")
        removeModal()
      })
      
    } else {
      new_account <- data.frame(
        Date = as.Date(input$account_date),
        Account_Balance = input$account_balance,
        Deposit = input$deposit,
        Spend = input$spend,
        Total = input$account_balance + input$deposit - input$spend,
        Currency = "USD",
        Details = input$details,
        stringsAsFactors = FALSE
      )
      
      append_gs_data(account_sheet_id, new_account)
      
      personal_account_data <<- read_gs_data(account_sheet_id)
      output$account_table <- renderDT({
        datatable(personal_account_data, options = list(pageLength = 10))
      })
      
      showNotification("Account entry added", type = "message")
    }
  })
  
  # Total income and balance info boxes
  output$total_income_box <- renderInfoBox({
    total_income <- sum(income_data$Total_Earnings)
    infoBox("Total Income", total_income ,"USD", icon = icon("money-bill-wave"), color = "green")
  })
  
  output$total_balance_box <- renderInfoBox({
    total_balance <- sum(personal_account_data$Total)
    infoBox("Total Balance", total_balance, "USD",icon = icon("piggy-bank"), color = "blue")
  })
  
  
  
  # Income trend plot
  output$income_trend_plot <- renderPlotly({
    p <- ggplot(income_data, aes(x = Date, y = Total_Earnings)) +
      geom_line() +
      labs(title = "Income Trend", x = "Date", y = "Total Earnings")
    ggplotly(p)
  })
  
  # Balance trend plot
  output$balance_trend_plot <- renderPlotly({
    p <- ggplot(personal_account_data, aes(x = Date, y = Total)) +
      geom_line() +
      labs(title = "Balance Trend", x = "Date", y = "Total Balance")
    ggplotly(p)
  })
  
  # Transfer income to account
  observeEvent(input$transfer_income, {
    total_income <- sum(income_data$Total_Earnings)
    new_account_entry <- data.frame(
      Date = Sys.Date(),
      Account_Balance = total_income,
      Deposit = total_income,
      Spend = 0,
      Total = total_income,
      Currency = "USD",
      Details = "Transferred Income",
      stringsAsFactors = FALSE
    )
    
    append_gs_data(account_sheet_id, new_account_entry)
    
    # Clear income data
    income_data <<- data.frame(
      Date = as.Date(character()),
      Price_Per_Item = numeric(),
      Number_of_Items = numeric(),
      Total_Price = numeric(),
      Logistics = numeric(),
      Total_Earnings = numeric(),
      Currency = character(),
      Note = character(),
      Customers = character(),
      stringsAsFactors = FALSE
    )
    update_gs_data(income_sheet_id, income_data)
    
    output$income_table <- renderDT({
      datatable(income_data, options = list(pageLength = 10))
    })
    
    showNotification("Income transferred to account", type = "message")
  })
  
  # Generate income report
  observeEvent(input$generate_income_report, {
    date_range <- input$income_date_range
    filtered_data <- income_data %>% filter(Date >= date_range[1] & Date <= date_range[2])
    
    output$total_income_selected <- renderText({
      total_income_selected <- sum(filtered_data$Total_Earnings)
      paste("Total Income: ", total_income_selected,"USD")
    })
    
    output$income_report_plot <- renderPlotly({
      p <- ggplot(filtered_data, aes(x = Date, y = Total_Earnings)) +
        geom_line() +
        labs(title = "Income Report", x = "Date", y = "Total Earnings")
      ggplotly(p)
    })
  })
  
  # Generate account report
  observeEvent(input$generate_account_report, {
    date_range <- input$account_date_range
    filtered_data <- personal_account_data %>% filter(Date >= date_range[1] & Date <= date_range[2])
    
    output$total_balance_selected <- renderText({
      total_balance_selected <- sum(filtered_data$Total)
      paste("Total Balance: ", total_balance_selected, "USD")
    })
    
    output$account_report_plot <- renderPlotly({
      p <- ggplot(filtered_data, aes(x = Date, y = Total)) +
        geom_line() +
        labs(title = "Account Report", x = "Date", y = "Total Balance")
      ggplotly(p)
    })
  })
  
  # Currency conversion
  observeEvent(input$convert, {
    amount <- input$amount
    from_currency <- input$from_currency
    to_currency <- input$to_currency
    exchange_rate_usd_egp <- input$exchange_rate
    
    
    conversion_rate <- if (from_currency == "USD" && to_currency == "EGP") {
      exchange_rate_usd_egp
    } else if (from_currency == "EGP" && to_currency == "USD") {
      1/exchange_rate_usd_egp
    }
    
    converted_amount <- amount * conversion_rate
    output$conversion_result <- renderText({
      paste("Converted Amount: ", converted_amount, to_currency)
    })
    
    output$rate_info_text <- renderText({
      paste("Conversion Rate: 1", from_currency, "=", conversion_rate, to_currency)
    })
    
    show("rate_info")
  })
  
  
  #####
  # Read and update reactive values for income and account data on app launch
  income <- reactiveVal(read_gs_data(income_sheet_id) %>% as.data.frame())
  account <- reactiveVal(read_gs_data(account_sheet_id) %>% as.data.frame())
  
  output$income_table <- renderDT({
    datatable(income(), options = list(pageLength = 10))
  })
  
  output$income_plot <- renderPlotly({
    data <- income()
    if (nrow(data) > 0) {
      data$Date <- as.Date(data$Date)
      p <- plot_ly(data, x = ~Date) %>%
        add_trace(y = ~Total_Earnings, name = 'Total Earnings', type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~Logistics, name = 'Logistics', type = 'scatter', mode = 'lines') %>%
        layout(title = "Income Details Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Amount (USD)"))
      return(p)
    }
  })
  
  output$account_table <- renderDT({
    datatable(account(), options = list(pageLength = 10))
  })
  
  output$account_plot <- renderPlotly({
    data <- account()
    if (nrow(data) > 0) {
      data$Date <- as.Date(data$Date)
      p <- plot_ly(data, x = ~Date) %>%
        add_trace(y = ~Total, name = 'Total Balance', type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~Deposit, name = 'Deposits', type = 'scatter', mode = 'lines') %>%
        add_trace(y = ~Spend, name = 'Spending', type = 'scatter', mode = 'lines') %>%
        layout(title = "Account Activity Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Amount (USD)"))
      return(p)
    }
  })
  ###
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
