# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(DT)

shinyUI(fluidPage(

  # Application title
  titlePanel("Рассчёт RFM-матрицы"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      p("Загрузите csv-файл с данными о транзакциях (", a(href="https://docs.google.com/spreadsheets/d/1J7QTFK7WRXdR1sYs7967jKXJjWhseYXjPPgQLGsrKG4/edit?usp=sharing", "образец"),")."),
      fileInput("file1", "Выберите CSV файл",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      tags$hr(),
      checkboxInput("header", "Присутствует заголовок", TRUE),
      p("Для обсуждения возможности инетграции, напишите нам на jane@crm-lab.com", style = "font-size: 8pt;")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("rfmSampleTable"),
      tags$hr(),
      uiOutput("actions"),
      plotlyOutput("rfMatrix"),
      uiOutput("info")
    )
  )
))
