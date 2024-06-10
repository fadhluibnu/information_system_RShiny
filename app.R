# Load libraries
library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(ggplot2)
library(reshape2)
library(dplyr)
library(viridis) 
library(shinyjs)
library(tidyverse)
library(RColorBrewer)
library(tidytext)
library(wordcloud2)

source("pages/sikd/ui_karakteristik.R")
source("pages/sikd/ui_pendanaan.R")
source("pages/profil/ui_profilDesa.R")
source("pages/sikd/ui_peningkatanPAD.R")
source("pages/sikd/ui_peningkatanPerekonomian.R")

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = tags$div(
      tags$div(
        "SISTEM INFORMASI KETERBUKAAN DESA MEKARSARI"
      ),
      tags$div(
        tags$img(src = "DesaMekarsari.png", height = "30px", style = "margin-right: 10px;"),
        tags$img(src = "SekolahVokasiIPB.png", height = "30px", style = "margin-right: 10px;"),
      ),
      style = "display: flex; justify-content: space-between"
    ),
    titleWidth = "100%"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Profil Desa", tabName = "dashboard" ,icon = icon("dashboard")),
      menuItem("SIKD", 
               icon = icon("database"),
               menuSubItem("Karakteristik Desa", tabName = "karakteristik", icon = icon("people-group")),
               menuSubItem("Pendanaan Desa", tabName = "pendanaan", icon = icon("sack-dollar")),
                 menuSubItem("Peningkatan PAD", tabName = "PeningkatanPAD", icon = icon("money-bill-trend-up")),
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
      tabItemKarakteristik,
      tabItemPendanaan,
      tabItemProfilDesa,
      tabItemPeningkatanPAD,
      tabItemPeningkatanPerekonomian
    )
  )
)


server <- function(input, output) {
  source("pages/profil/server_profilDesa.R", local=TRUE)
  source("pages/sikd/server_karakteristik.R", local=TRUE)
  source("pages/sikd/server_pendanaan.R", local=TRUE)
  source("pages/sikd/server_peningkatanPAD.R", local=TRUE)
  source("pages/sikd/server_peningkatanPerekonomian.R", local=TRUE)
}

shinyApp(ui, server)