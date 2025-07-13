library(shiny)
library(lubridate)

# Utility functions
next_weekday <- function(x = Sys.Date(), week_start = 1) {
  lubridate::ceiling_date(as.Date(x), unit = "week", week_start = week_start)
}

get_cycle_dates <- function(start_date,
                            cycle_number,
                            cycle_length,
                            week_start = NA) {
  start_date <- as.Date(start_date)
  if (!is.na(week_start)) {
    current_wday <- lubridate::wday(start_date, week_start = 1)
    if (current_wday != week_start) {
      start_date <- next_weekday(start_date, week_start = week_start)
    }
  }
  cycle_dates <- start_date + seq(0, by = cycle_length, length.out = cycle_number)
  cycle_dates
}

pretty_date <- function(date, include_year = TRUE) {
  date <- as.Date(date)
  month <- as.integer(format(date, "%m"))
  day <- as.integer(format(date, "%d"))
  year <- as.integer(format(date, "%y"))
  if(include_year){
    final <- paste(month, day, year, sep = "/")
  }else{
    final <- paste(month, day, sep = "/")
  }
  final
}

# UI
ui <- fluidPage(titlePanel("Chemo Cycle Date Generator"),
                sidebarLayout(
                  sidebarPanel(
                    dateInput("start_date", "Start Date", value = Sys.Date()),
                    numericInput(
                      inputId = "cycle_number",
                      label = "Number of Cycles",
                      value = 6,
                      min = 1
                    ),
                    numericInput(
                      inputId = "cycle_length",
                      label = "Cycle Length (days)",
                      value = 14,
                      min = 7,
                      step = 7
                    ),
                    selectInput(
                      inputId = "week_start",
                      label = "Align to Weekday?",
                      choices = c(
                        "None" = NA,
                        Monday = 1,
                        Tuesday = 2,
                        Wednesday = 3,
                        Thursday = 4,
                        Friday = 5,
                        Saturday = 6,
                        Sunday = 7
                      ),
                      selected = NA
                    ),
                    checkboxInput(
                      inputId = "include_year",
                      label = "Include Year?"
                    )
                  ),
                  mainPanel(
                    p("Written by Brandon Rose, MD, MPH using R, shiny, shinylive, and WebR."),
                    verbatimTextOutput("cycle_text"),
                    verbatimTextOutput("cycle_text2")
                  )
                ))

# Server
server <- function(input, output, session) {
  output$cycle_text <- renderText({
    week_start <- as.numeric(input$week_start)
    if (is.na(week_start))
      week_start <- NA
    dates <- get_cycle_dates(
      start_date = input$start_date,
      cycle_number = input$cycle_number,
      cycle_length = input$cycle_length,
      week_start = week_start
    )
    text_lines <- paste0("C",
                         seq_along(dates),
                         ": ",
                         pretty_date(dates, include_year = input$include_year))
    paste(text_lines, collapse = "\n")
  })

  output$cycle_text2 <- renderText({
    week_start <- as.numeric(input$week_start)
    if (is.na(week_start))
      week_start <- NA
    dates <- get_cycle_dates(
      start_date = input$start_date,
      cycle_number = input$cycle_number,
      cycle_length = input$cycle_length,
      week_start = week_start
    )
    text_lines <- paste0("Dates: ", paste0(
      pretty_date(dates, include_year = input$include_year),
      collapse = ", "
    ))
    text_lines
  })
}

# Run the app
shinyApp(ui, server)
