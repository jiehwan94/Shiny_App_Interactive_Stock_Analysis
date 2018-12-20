library(shiny)
library(shinysky)
library(quantmod)
library(highcharter)
library(dplyr)

df <- read.csv(file="C:\\Users\\user\\Desktop\\Stock Analysis\\90days_industry_stats.csv", header=TRUE, sep=",")
head(df)


df[is.na(df)]<- 0


my_autocomplete_list <- as.character(df$ticker)



shinyUI(fluidPage(
  
  
  
  # Sidebar with a slider input for number of bins
  # sidebarLayout(
    # sidebarPanel(
      
      fluidRow(
        column(2,
            dateRangeInput('dateRange',
                           label = 'Date range input:',
                           start = Sys.Date() - 365, end = Sys.Date()
            ),
            selectizeInput(
              'foo', "Multi-select", choices=my_autocomplete_list, multiple=TRUE
            ),

            actionButton("submit", "Submit")
              ),
        br(),
        br(),
        br(),
        br(),
        column(10,
          mainPanel(

            highchartOutput("chart1"),
            highchartOutput("chart2"),
            column(4,"Growth", highchartOutput("growthchart")),
            column(8,"Radar chart", chartJSRadarOutput("radarchart"))

          )
    )
  # )
  )
  )
)