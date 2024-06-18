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
  tags$script(
    HTML(
      "
      Shiny.addCustomMessageHandler('form_update_false', function(message) {
        Shiny.setInputValue('form_update', '0');
      });
      Shiny.addCustomMessageHandler('selected_id_handler', function(message) {
        Shiny.setInputValue('selected_id', message);
      });
    "
    )
  ), 
  conditionalPanel(
    condition = "input.form_update == '1'", 
    fluidRow(
      box(
        title = h3(tags$b("Update Data")),
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        box(
          status = "success",
          width = 12,
          h4(tags$b("Potensi Desa")),
          
          textInput("Jenis.potensi.edit", "Jenis potensi"),	
          textInput("Bidang.edit", "Bidang"),	
          textInput("Jumlah.satuan.edit", "Jumlah satuan")
        ),
        actionButton("updatePotensiDesa", "Update"),
        actionButton("cancelPotensiDesa", "Cancel")
    )
    )
  ),
  fluidRow(
    box(title = "Potensi Desa", width = 12, 
        DTOutput("data_table_PotensiDesa"))
  ),
)