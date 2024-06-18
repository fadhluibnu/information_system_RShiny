render_server_potensi_desa <- function(params) {
  
  pathTambahPotensiDesa <- "data/PotensiDesa.csv"
  
  pathPotensiDesa <- read_csv("data/PotensiDesa.csv")
  output$data_table_PotensiDesa <- renderDT({
    data <- pathPotensiDesa%>%
      rename(
        `Jenis potensi` = Jenis.potensi,
        `Jumlah satuan` = Jumlah.satuan
      )
    
    action_buttons <- if (!is.null(input$stored_user)) {
      paste0(
        '<button class="update-btn" data-id="', data$No, '">Update</button>',
        '<button class="delete-btn" data-id="', data$No, '">Delete</button>'
      )
    } else {
      '<span></span>'
    }
    data$Actions <- action_buttons
    
    data <- data%>%
      rename(
        ID = No
      )
    
    datatable(data, selection = 'none', escape = FALSE, rownames = TRUE, colnames = c('No' = 1), options = list(
      headerCallback = JS(
        "function(thead, data, start, end, display){",
        "  if (!$('#potensiDesa-checkbox').length) {",
        "  $(thead).closest('thead').prepend(`
      <tr id=\"potensiDesa-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
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
        list(orderable = FALSE, className = 'select-checkbox-peningkatan-program-wisata', targets = 0),
        list(targets = 0, visible = TRUE),
        list(targets = ncol(data), orderable = FALSE, searchable = FALSE)
      ),
      select = list(style = 'multi', selector = 'td:first-child'),
      scrollX = TRUE
    ))%>%
      formatStyle(
        columns = c('Actions'),
        cursor = 'pointer'
      )
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
  })
  
  observeEvent(input$update_id, {
    id <- as.integer(input$update_id)
    data_potensi_desa <- loadDataPotensiDesa()
    data_potensi_desa <- data_potensi_desa[data_potensi_desa$No == id, ]
    
    
    updateTextInput(session,"Jenis.potensi.edit", value = data_potensi_desa$Jenis.potensi)	
    updateTextInput(session,"Bidang.edit", value = data_potensi_desa$Bidang)	
    updateTextInput(session,"Jumlah.satuan.edit", value = data_potensi_desa$Jumlah.satuan)
    
    session$sendCustomMessage("selected_id_handler", id)
    
  })
  
  observeEvent(input$cancelPotensiDesa, {
    session$sendCustomMessage("form_update_false", "0")
  })
  
  observeEvent(input$updatePotensiDesa, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data_potensi_desa <- loadDataPotensiDesa()
      data_potensi_desa[data_potensi_desa$No == input$selected_id, ] <- data.frame(
        No = input$selected_id,
        Jenis.potensi=input$Jenis.potensi.edit,	
        Bidang=input$Bidang.edit,	
        Jumlah.satuan=input$Jumlah.satuan.edit,
        stringsAsFactors = FALSE
      )
      
      successPotensiDesa <- safeWriteCSV(data_potensi_desa, paste0(pathTambahPotensiDesa, ".tmp"))
      if (successPotensiDesa){
        file.rename(paste0(pathTambahPotensiDesa, ".tmp"), pathTambahPotensiDesa)
        resetInputs()
        render_server_potensi_desa(FALSE)
        
        session$sendCustomMessage("form_update_false", "0")
        
        showModal(modalDialog(
          title = "Success",
          "Data berhasil diperbarui",
          easyClose = TRUE,
          footer = NULL
        ))
      }else {
        
        unlink(paste0(pathTambahPotensiDesa, ".tmp"))
        
        removeModal()
        
        showModal(modalDialog(
          title = "Error",
          "Gagal menambahkan data.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
    params = TRUE
  })
}

render_server_potensi_desa(TRUE)