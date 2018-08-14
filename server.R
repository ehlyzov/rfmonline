
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(openxlsx)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(plotly)
library(magrittr)

source("rfm.R")

xy_str <- function(e) {
  paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
}

# функция возвращает метку для текущего RF-значения
build_style_matrix <- function() {
  df <- data.frame(r=numeric(),f=numeric(), label=factor(), color=character())
  
  df %<>% rbind(cbind(r = 1, f = 1, label = "Разовые потерянные", color=rgb(231, 147, 103, maxColorValue=255)))
  df %<>% rbind(cbind(r = 1, f = 2:3, label = "Отток из перспективных", color=rgb(167, 74, 51, maxColorValue=255)))
  df %<>% rbind(cbind(r = 1, f = 4:5, label = "Отток из лояльных", color=rgb(177, 54, 50, maxColorValue=255)))
  df %<>% rbind(cbind(r = 2:3, f = 1, label = "Разовые в зоне риска", color=rgb(250, 218, 129, maxColorValue=255)))
  df %<>% rbind(cbind(r = rep(2:3, each = 2), f = 2:3, label = "Перспективные в зоне риска", color=rgb(239, 192, 134, maxColorValue=255)))
  df %<>% rbind(cbind(r = rep(2:3, each = 2), f = 4:5, label = "Лояльные в зоне риска", color=rgb(214, 143, 58, maxColorValue=255)))
  df %<>% rbind(cbind(r = 4, f = 1, label = "Новички", color=rgb(213, 189, 129, maxColorValue=255)))
  df %<>% rbind(cbind(r = 4, f = 2:3, label = "Перспективные", color=rgb(136, 141, 81, maxColorValue=255)))
  df %<>% rbind(cbind(r = 4, f = 4:5, label = "Лояльные", color=rgb(116, 125, 59, maxColorValue=255)))
  df %<>% rbind(cbind(r = 5, f = 1, label = "Первая покупка", color=rgb(229, 232, 185, maxColorValue=255)))
  df %<>% rbind(cbind(r = 5, f = 2:3, label = "Перспективные", color=rgb(136, 141, 81, maxColorValue=255)))
  df %<>% rbind(cbind(r = 5, f = 4, label = "Лояльные", color=rgb(116, 125, 59, maxColorValue=255)))
  df %<>% rbind(cbind(r = 5, f = 5, label = "VIP", color=rgb(112, 119, 41, maxColorValue=255)))
  colnames(df) <- c("R_Score", "F_Score", "label", "color")
  df$R_Score %<>% as.numeric()
  df$F_Score %<>% as.numeric()
  df
}

style_matrix <- build_style_matrix()

get_group_by_rf <- function(r,f) {
  as.character(subset(style_matrix, R_Score == r & F_Score == f)$label)
}

shinyServer(function(input, output) {
  
  selectedCell <- reactiveValues()

  data <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    read.csv(
      file = inFile$datapath, 
      header = input$header, 
      col.names = c("date", "id", "sum"),
      colClasses = c("character", "character", "numeric")) %>% 
      mutate(date = parse_date_time(date, c("dmy","mdy", "ymd")))
  })
  
  dataSample  <- reactive({
    if(is.null(data())){return ()}
    head(data())
  })
  
  rfm <- reactive({
    if(is.null(data())){return ()}
    rfm.df <- data()
    getIndependentScore(
      getDataFrame(
        rfm.df, 
        min(rfm.df$date), 
        max(rfm.df$date), 
        tIDColName="id",
        tDateColName="date",
        tAmountColName="sum")) %>% select(-date, -sum)
  })
  
  output$rfmSampleTable <- renderTable({
    if(is.null(rfm())){return ()}
    head(rfm(), n = 10L)
  })

  rfm.matrix <- reactive({
    if(is.null(rfm())){return ()}
    rfm() %>% 
      group_by(R_Score, F_Score) %>%
      tally() %>% full_join(style_matrix)
  })
  
  output$rfMatrix <- renderPlotly({
    if(is.null(rfm.matrix())){return ()}
    plot.colors <- style_matrix %>% group_by(label, color) %>% summarize() 
    plot.colors$color %<>% as.character()
    
    plot.colors.vec <- plot.colors$color
    names(plot.colors.vec) <- as.factor(plot.colors$label)
    plot.g <- ggplot(rfm.matrix(), 
                     aes(R_Score, F_Score, xmin=0.5, ymin=0.5, xmax=5.5, ymax=5.5)) +
      geom_tile(aes(fill = as.factor(label))) + 
      scale_fill_manual(values=plot.colors.vec) + 
      theme(legend.title=element_blank()) +
      geom_text(aes(R_Score, F_Score, label = n), color = "white", size = 4)
    ggplotly(plot.g, source = "matrix", tooltip = NULL) %>% 
      config(displayModeBar = FALSE, editable = FALSE, sendData = TRUE, showTips = FALSE) %>% 
      layout(xaxis=list(fixedrange=TRUE)) %>% 
      layout(yaxis=list(fixedrange=TRUE)) %>%
      layout(hovermode="closest", hoverlabel="")
  })
  
  output$downloadDataCsv <- downloadHandler(
    filename = "rfm.csv",
    content = function(file) {
      write.csv(rfm(), file)
    },
    contentType = "text/csv"
  )
  
  output$downloadCellCsv <- downloadHandler(
    filename = paste0("rfm_",selectedCell$x,"_", selectedCell$y,".csv"),
    content = function(file) {
      write.csv(subset(rfm(), R_Score == selectedCell$x, F_Score == selectedCell$y), file)
    },
    contentType = "text/csv"
  )
  
  output$downloadGroupCsv <- downloadHandler(
    filename = function () { 
      paste0("rfm_group_",selectedCell$x,"_", selectedCell$y,".csv") 
    },
    content = function(file) {
      rfm.filter <- style_matrix %>% 
        filter(label == selectedCell$label) %>% 
        select("R_Score", "F_Score")
      write.csv(rfm() %>% right_join(rfm.filter), file)
    },
    contentType = "text/csv"
  )
  
  output$downloadDataXlsx <- downloadHandler(
    filename = "rfm.xls",
    content = function(file) {
      write.xlsx(rfm(), file = file,
                 colNames = TRUE, sheetName = "RFM")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  output$actions <- renderUI({
    if(is.null(rfm())){return ()}
    tagList(
      downloadButton("downloadDataXlsx", "Скачать для Excel"),
      downloadButton("downloadDataCsv", "Скачать CSV"))
  })
  
  observeEvent(event_data("plotly_click", source = "matrix"), {
    ev <- event_data("plotly_click", source = "matrix")
    selectedCell$x <- ev$x
    selectedCell$y <- ev$y
    selectedCell$label <- get_group_by_rf(selectedCell$x, selectedCell$y)
  })
  
  output$info <- renderUI({
    if(is.null(rfm())){return ()}
    if(is.null(selectedCell$x)){
      helpText("Чтобы выбрать ячейку для экспорта, нажмите на цифру в ячейке один раз.")
    } else {
      tagList(
        helpText(paste0("Выбрана ячейка (R:", selectedCell$x, " , F:", selectedCell$y, ")")),
        br(),
        downloadButton("downloadCellCsv", "Скачать ячейку"),
        downloadButton("downloadGroupCsv", "Скачать фрагмент"))
    }
  })
  
})
