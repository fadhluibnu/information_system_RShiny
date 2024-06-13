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
library(plotly)

source("pages/sikd/ui_karakteristik.R")
source("pages/sikd/ui_pendanaan.R")
source("pages/profil/ui_profilDesa.R")
source("pages/sikd/ui_peningkatanPAD.R")
source("pages/sikd/ui_peningkatanPerekonomian.R")
source("pages/sikd/ui_peningkatanProgramWisata.R")
source("pages/sikd/ui_potensiDesa.R")
source("pages/sikd/ui_tambah_data.R")

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
      id = "tabs",
      menuItem("Profil Desa", tabName = "dashboard" ,icon = icon("dashboard")),
      menuItem("SIKD", 
               icon = icon("database"),
               menuSubItem("Tambah Data", tabName = "tambahdata", icon = icon("file-circle-plus")),
               menuSubItem("Karakteristik Desa", tabName = "karakteristik", icon = icon("people-group")),
               menuSubItem("Potensi Desa", tabName = "potensiDesa", icon = icon("magnifying-glass-chart")),
               menuSubItem("Pendanaan Desa", tabName = "pendanaan", icon = icon("sack-dollar")),
                 menuSubItem("Peningkatan PAD", tabName = "PeningkatanPAD", icon = icon("money-bill-trend-up")),
               menuSubItem("Peningkatan Perekonomian", tabName = "PeningkatanPerekonomian", icon = icon("arrow-up-right-dots")),
               menuSubItem("Peningkatan Program Wisata", tabName = "programwisata", icon = icon("arrow-up-from-water-pump"))
      )
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItemKarakteristik,
      tabItemPendanaan,
      tabItemProfilDesa,
      tabItemPeningkatanPAD,
      tabItemPeningkatanPerekonomian,
      tabItemPeningkatanProgramWisata,
      tabItemPotensiDesa,
      tabItemTambahData
    ),
    tags$script(HTML("
    $(document).on('click', '.update-btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('update_id', id);
      Shiny.setInputValue('form_update', true);
    });
    $(document).on('click', '.delete-btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('delete_id', id);
    });
  "))
  )
)


server <- function(input, output, session) {
  
  source("pages/profil/server_profilDesa.R", local=TRUE)
  source("pages/sikd/server_karakteristik.R", local=TRUE)
  source("pages/sikd/server_pendanaan.R", local=TRUE)
  source("pages/sikd/server_peningkatanPAD.R", local=TRUE)
  source("pages/sikd/server_peningkatanPerekonomian.R", local=TRUE)
  source("pages/sikd/server_peningkatanProgramWisata.R", local=TRUE)
  source("pages/sikd/server_potensiDesa.R", local=TRUE)
  source("pages/sikd/server_tambah_data.R", local=TRUE)
  source("pages/sikd/server_hapus_data.R", local=TRUE)
  
  observeEvent(input$cancel, {
    showModal(modalDialog(
      title = "Success",
      "Data berhasil dihapus.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
}

shinyApp(ui, server)