tabItemPotensiDesa <- tabItem(
  tabName = "potensiDesa",
  conditionalPanel(
    condition = "input.JenisPotensi == true || input.Bidang == true || input.JumlahSatuan == true",
    fluidRow(
      box(
        title = tags$b("Grafik Potensi Desa"),
        status = "danger",
        width = 12, 
          plotlyOutput("pieChartPotensiDesa"))
    ),
  ),
  fluidRow(
    box(title = "Potensi Desa", width = 12, 
        DTOutput("data_table_PotensiDesa"))
  ),
)