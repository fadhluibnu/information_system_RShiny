render_server_potensi_desa <- function() {
  pathPotensiDesa <- read_csv("data/PotensiDesa.csv")
  output$data_table_PotensiDesa <- renderDT({
    data <- pathPotensiDesa%>%
      rename(
        `Jenis potensi` = Jenis.potensi,
        `Jumlah satuan` = Jumlah.satuan
      )
    
    datatable(data, selection = 'none', rownames = FALSE, options = list(
      headerCallback = JS(
        "function(thead, data, start, end, display){",
        "  if (!$('#potensiDesa-checkbox').length) {",
        "  $(thead).closest('thead').prepend(`
      <tr id=\"potensiDesa-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"JenisPotensi\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"Bidang\">
        </th>
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"JumlahSatuan\">
        </th>
      </tr>`);",
        "  $('#JenisPotensi').on('click', function(){",
        "    Shiny.setInputValue('JenisPotensi', this.checked);",
        "  });",
        "  $('#Bidang').on('click', function(){",
        "    Shiny.setInputValue('Bidang', this.checked);",
        "  });",
        "  $('#JumlahSatuan').on('click', function(){",
        "    Shiny.setInputValue('JumlahSatuan', this.checked);",
        "  });",
        "  }",
        "}"
      ),
      columnDefs = list(
        list(orderable = FALSE, className = 'select-checkbox-peningkatan-program-wisata', targets = 0)
      ),
      select = list(style = 'multi', selector = 'td:first-child'),
      scrollX = TRUE, 
      autoWidth = TRUE
    ))
  })
  
  output$pieChartPotensiDesa <- renderPlotly({
    data <- pathPotensiDesa 
    
    data_summary <- data %>%
      group_by(Bidang) %>%
      summarise(count = n(),
                Jenis.potensi = paste(Jenis.potensi, collapse = ", "))
    
    plot_ly(data_summary, labels = ~Bidang, values = ~count, type = 'pie',
            hovertext = ~Jenis.potensi, hoverinfo = 'text+label+percent',
            textinfo = 'label+percent') %>%
      layout(title = 'Persentase Setiap Sektor Bidang',
             hovermode = 'closest')
    # 'text+percent'
  })
}

render_server_potensi_desa()