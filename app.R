# Load libraries
library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(ggplot2)
library(dplyr)
library(viridis) 
library(shinyjs)

source("pages/sikd/ui_karakteristik.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "Information System"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "dashboard" ,icon = icon("dashboard")),
      menuItem("SIKD", 
               icon = icon("database"),
               menuSubItem("Karakteristik", tabName = "karakteristik", icon = icon("people-group"))
      ),
      menuItem("Pendanaan", tabName = "pendanaan", icon = icon("sack-dollar")),
      menuItem("Peningkatan Perekonomian", tabName = "PeningkatanPerekonomian", icon = icon("arrow-up-right-dots")),
      menuItem("Program Wisata", tabName = "programwisata", icon = icon("arrow-up-from-water-pump"))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        .hidden-box {
          display: none;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "dashboard",
      ),
      tabItemKarakteristik
    )
  )
)


server <- function(input, output) {
  source("pages/sikd/server_karakteristik.R", local=TRUE)
}

shinyApp(ui, server)