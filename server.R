library(shiny)
library(shinysky)
library(quantmod)
library(highcharter)
library(dplyr)
library(radarchart)
#library(tidyverse)

shinyServer(function(input, output) {
  
  output$foo_out<- renderPrint({input$foo[1]})
  
  
  observeEvent(input$submit, {
    first_date<- input$dateRange[1]
    last_date<- input$dateRange[2]
    code1<- as.character(input$foo[1]) #####################
    code2<- as.character(input$foo[2])
    code3<- as.character(input$foo[3])
    x <- getSymbols(code1, from = Sys.Date()-365, auto.assign = FALSE)
    y <- getSymbols(code2, from = Sys.Date()-365, auto.assign = FALSE)
    z <- getSymbols(code3, from = Sys.Date()-365, auto.assign = FALSE)
    
    
    SPY <- adjustOHLC(x)
    
    
    SPY.SMA.10 <- SMA(Cl(SPY), n = 5)
    SPY.SMA.200 <- SMA(Cl(SPY), n = 100)
    SPY.RSI.14 <- RSI(Cl(SPY))
    SPY.RSI.SellLevel <- xts(rep(70, NROW(SPY)), index(SPY))
    SPY.RSI.BuyLevel <- xts(rep(30, NROW(SPY)), index(SPY))
    
    
    colnames(x)<- c("Open","High","Low","Close","Volume","Adjusted")
    colnames(y)<- c("Open","High","Low","Close","Volume","Adjusted")
    colnames(z)<- c("Open","High","Low","Close","Volume","Adjusted")
    
    # output$stockcode  <- renderText({(input$foo[1])})  #####################
    # 
    # 
    # output$chart1<- renderHighchart({
    #   highchart(type = "stock") %>%
    #     hc_yAxis_multiples(
    #       create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)
    #     ) %>%
    #     hc_add_series(stock_price, yAxis = 0, name = code) %>%
    #     hc_add_series(stock_price$Volume, color = "gray", yAxis = 1, name = "Volume", type = "column")
    # })
    # 
    
    output$chart1<- renderHighchart({
      highchart(type = "stock") %>%
        hc_yAxis_multiples(
          create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)
        ) %>% 
        # series :D
        hc_add_series(SPY, yAxis = 0, name = code1) %>% 
        hc_add_series(SPY.SMA.10, yAxis = 0, name = "Fast MA") %>% 
        hc_add_series(SPY.SMA.200, yAxis = 0, name = "Slow MA") %>% 
        hc_add_series(x$Volume, color = "gray", yAxis = 1, name = "Volume", type = "column") %>% 
        hc_add_series(SPY.RSI.14, yAxis = 2, name = "Osciallator", color = hex_to_rgba("green", 0.7)) %>%
        hc_add_series(SPY.RSI.SellLevel, color = hex_to_rgba("red", 0.7),
                      yAxis = 2, name = "Sell level") %>% 
        hc_add_series(SPY.RSI.BuyLevel, color = hex_to_rgba("blue", 0.7),
                      yAxis = 2, name = "Buy level") 
    })
    
    output$chart2<- renderHighchart({
      highchart(type = "stock") %>%
        hc_add_series(x, name = code1, type = "ohlc", color ="red") %>%
        hc_add_series(y, name = code2, type = "ohlc", color="green") %>%
        hc_add_series(z, name = code3, type = "ohlc", color="blue") %>%
        hc_yAxis_multiples(create_yaxis(3, height = c(2, 1, 1), turnopposite = TRUE)) %>%
        # hc_add_series(x, yAxis = 0, name = code1) %>%
        hc_add_series(x$Volume, color = "red", yAxis = 1, name = code1, type = "column") %>%
        hc_add_series(y$Volume, color = "green", yAxis = 1, name = code2, type = "column") %>%
        hc_add_series(z$Volume, color = "blue", yAxis = 1, name = code3, type = "column")
    })
    
    output$growthchart<- renderHighchart({
      highchart() %>%
        hc_add_series_list(list(
          list(
            name = code1,
            color = 'skyblue',
            data= c(df[df$ticker==code1,]$growth_0,#####################
                    df[df$ticker==code1,]$growth_1,#####################
                    df[df$ticker==code1,]$growth_2,#####################
                    df[df$ticker==code1,]$growth_3,#####################
                    df[df$ticker==code1,]$growth_4)#####################
          ),
          list(
            name = code2,
            color = 'green',
            data= c(df[df$ticker==code2,]$growth_0,
                    df[df$ticker==code2,]$growth_1,
                    df[df$ticker==code2,]$growth_2,
                    df[df$ticker==code2,]$growth_3,
                    df[df$ticker==code2,]$growth_4
            )
          ),
          list(
            name = code3,
            color = 'pink',
            data= c(df[df$ticker==code3,]$growth_0,
                    df[df$ticker==code3,]$growth_1,
                    df[df$ticker==code3,]$growth_2,
                    df[df$ticker==code3,]$growth_3,
                    df[df$ticker==code3,]$growth_4
            )
          )
        )
        )
    })
    
    
  })
})
