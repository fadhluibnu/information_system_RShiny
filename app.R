# Load libraries
library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(ggplot2)
library(dplyr)
library(viridis) 
library(shinyjs)
library(tidyverse)
library(RColorBrewer)
library(tidytext)
library(wordcloud2)

source("pages/sikd/ui_karakteristik.R")
source("pages/sikd/ui_pendanaan.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "Information System"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "dashboard" ,icon = icon("dashboard")),
      menuItem("SIKD", 
               icon = icon("database"),
               menuSubItem("Karakteristik", tabName = "karakteristik", icon = icon("people-group")),
               menuSubItem("Pendanaan", tabName = "pendanaan", icon = icon("sack-dollar")),
               menuSubItem("Peningkatan Perekonomian", tabName = "PeningkatanPerekonomian", icon = icon("arrow-up-right-dots")),
               menuSubItem("Program Wisata", tabName = "programwisata", icon = icon("arrow-up-from-water-pump"))
      )
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
      tabItemKarakteristik,
      tabItemPendanaan
    )
  )
)


server <- function(input, output) {
  source("pages/sikd/server_karakteristik.R", local=TRUE)
  source("pages/sikd/server_pendanaan.R", local=TRUE)
}

shinyApp(ui, server)