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

library(rsconnect)


source("pages/sikd/ui_karakteristik.R")
source("pages/sikd/ui_pendanaan.R")
source("pages/profil/ui_profilDesa.R")
source("pages/sikd/ui_peningkatanPAD.R")
source("pages/sikd/ui_peningkatanPerekonomian.R")
source("pages/sikd/ui_peningkatanProgramWisata.R")
source("pages/sikd/ui_potensiDesa.R")
source("pages/sikd/ui_tambah_data.R")
source("pages/aspek/ui_identitas.R")
source("pages/aspek/ui_aspekEkonomi.R")
source("pages/aspek/ui_aspekSosial.R")
source("pages/aspek/ui_aspekTemporal.R")

ui <- dashboardPage(
  title = "SISTEM INFORMASI KETERBUKAAN DESA MEKARSARI",
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
      ),
      menuItem('Aspek', icon = icon('list'),
               menuSubItem(
                 'Tambah Data', 
                 tabName = 'tambahData',
                 icon = icon('file-circle-plus')
              ),
               menuSubItem(
                 'Idensitas', 
                 tabName = 'identitas', 
                 icon = icon('users')
              ),
              menuSubItem(
                'Aspek Ekonomi', 
                tabName = 'aspekEkonomi', 
                icon = icon('magnifying-glass-dollar')
              ),
              menuSubItem(
                'Aspek Sosial', 
                tabName = 'aspekSosial', 
                icon = icon('user-group')
              ),
              menuSubItem(
                'Aspek Temporal', 
                tabName = 'aspekTemporal', 
                icon = icon('user-clock')
              )
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
      tabItemTambahData,
      tabItemIdentitas,
      tabItemAspekEkonomi,
      tabItemAspekSosial,
      tabItemAspekTemporal
    ),
    tags$style(HTML("
    .main-sidebar{
      position: fixed;
    }
    .main-header{
      position: fixed;
      width: 100%;
    }
    .content-wrapper{
    min-height: 307px;
    margin-top: 50px;
    }
    ")),
    tags$script(HTML("
    $(document).on('click', '.update-btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('update_id', id);
      Shiny.setInputValue('form_update', '1');
    });
    $(document).on('click', '.delete-btn', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('delete_id', id);
    });
    
    $(document).on('click', '.update-btn-aspek', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('update_aspek_id', id);
      Shiny.setInputValue('form_update_aspek', '1');
    });
    $(document).on('click', '.delete-btn-aspek', function() {
      var id = $(this).data('id');
      Shiny.setInputValue('delete_aspek_id', id);
    });
    
    Shiny.addCustomMessageHandler('form_update_false', function(message) {
      Shiny.setInputValue('form_update', '0');
    });
  "))
  )
)


server <- function(input, output, session) {
  
  # source("pages/profil/server_profilDesa.R", local=TRUE)
  # source("pages/sikd/server_karakteristik.R", local=TRUE)
  # source("pages/sikd/server_pendanaan.R", local=TRUE)
  # source("pages/sikd/server_peningkatanPAD.R", local=TRUE)
  # source("pages/sikd/server_peningkatanPerekonomian.R", local=TRUE)
  # source("pages/sikd/server_peningkatanProgramWisata.R", local=TRUE)
  # source("pages/sikd/server_potensiDesa.R", local=TRUE)
  # source("pages/sikd/server_tambah_data.R", local=TRUE)
  # source("pages/sikd/server_hapus_data.R", local=TRUE)
  # source("pages/aspek/server_identitas.R", local=TRUE)
  # source("pages/aspek/server_aspekEkonomi.R", local=TRUE)
  # source("pages/aspek/server_aspekSosial.R", local=TRUE)
  source("pages/aspek/server_aspekTemporal.R", local=TRUE)
}

shinyApp(ui, server)