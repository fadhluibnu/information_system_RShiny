render_server_identitas <- function (params) {
  
  pathIdentitasResponden <- 'data/IdentitasResponden.csv'
  
  output$data_table_identitas <- renderDT({
    data <- read_csv(pathIdentitasResponden)
    
    data <- data %>%
      mutate(
        Jenis.kelamin = recode(
          Jenis.kelamin,
          `1` = "Perempuan",
          `2` = "Laki - Laki"
        ),
        Status.perkawinan = recode(
          Status.perkawinan,
          `1`='Belum Menikah',
          `2`='Menikah',
          `3`='Duda/ Janda',
          `4`='Lainnya'
        ),
        Apakah.memiliki.anak = recode(
          Apakah.memiliki.anak,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),
        Tingkat.pendidikan = recode(
          Tingkat.pendidikan,
          `1`='Tidak lulus SD',
          `2`='SD',
          `3`='SMP',
          `4`='SMA',
          `5`='Diploma',
          `6`='Sarjana',
          `7`='Pasca Sarjana',
          `8`='Lainnya'
        ),
        Apakah.anda.bekerja.saat.ini = recode(
          Apakah.anda.bekerja.saat.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        )
      )
    
    action_buttons <- paste0(
      '<button class="update-btn-aspek" data-id="', data$No, '">Update</button>',
      '<button class="delete-btn-aspek" data-id="', data$No, '">Delete</button>'
    )
    
    data$Actions <- action_buttons
    
    datatable(
      data, 
      selection = 'none',
      escape = FALSE,
      rownames = TRUE,
      colnames = c(
        'No', 
        'ID', 
        'Nama',	
        'Jenis kelamin',	
        'Usia (tahun)',	
        'Status perkawinan',	
        'Apakah memiliki anak?',	
        'Jumlah anak (orang)',	
        'Tingkat pendidikan ',	
        'Apakah anda bekerja saat ini?',	
        'Jika bekerja, apa pekerjaan anda saat ini?',
        'Actions'
        ),
      options = list(
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  if (!$('#identitas-checkbox').length) {",
          "  $(thead).closest('thead').prepend(` 
      <tr id=\"identitas-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"jenis-kelamin\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"usia\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"status-perkawinan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"apakah-memiliki-anak\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"jumlah-anak\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pendidikan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"apakah-bekerja\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pekerjaan\">
        </th> 
      </tr>
          `);",
          "  $('#jenis-kelamin').on('click', function(){",
          "    Shiny.setInputValue('jenis-kelamin', this.checked);",
          "  });",
          "  $('#usia').on('click', function(){",
          "    Shiny.setInputValue('usia', this.checked);",
          "  });",
          "  $('#status-perkawinan').on('click', function(){",
          "    Shiny.setInputValue('status-perkawinan', this.checked);",
          "  });",
          "  $('#apakah-memiliki-anak').on('click', function(){",
          "    Shiny.setInputValue('apakah-memiliki-anak', this.checked);",
          "  });",
          "  $('#jumlah-anak').on('click', function(){",
          "    Shiny.setInputValue('jumlah-anak', this.checked);",
          "  });",
          "  $('#pendidikan').on('click', function(){",
          "    Shiny.setInputValue('pendidikan', this.checked);",
          "  });",
          "  $('#apakah-bekerja').on('click', function(){",
          "    Shiny.setInputValue('apakah-bekerja', this.checked);",
          "  });",
          "  $('#pekerjaan').on('click', function(){",
          "    Shiny.setInputValue('pekerjaan', this.checked);",
          "  });",
          "  }",
          "}"
        ), 
        scrollX = TRUE,
        columnDefs = list(
          list(orderable = FALSE, className = 'select-checkbox-identitas-responden', targets = 0),
          list(targets = 0, visible = TRUE),
          list(targets = ncol(data), orderable = FALSE, searchable = FALSE)
        ),
        select = list(style = 'multi', selector = 'td:first-child')
      )
    )%>%
      formatStyle(
        columns = c('Actions'),
        cursor = 'pointer'
      )
  })
  
  # Update Identitas
  loadDataIdentitasResponden <- function() {
    if (file.exists(pathIdentitasResponden)) {
      read.csv(pathIdentitasResponden)
    } else {
      data.frame(
        No = character(),
        Nama=character(),	
        Jenis.kelamin=character(),	
        Usia.tahun=character(),	
        Status.perkawinan=character(),	
        Apakah.memiliki.anak=character(),	
        Jumlah.anak.orang=character(),	
        Tingkat.pendidikan=character(),	
        Apakah.anda.bekerja.saat.ini=character(),	
        Jika.bekerja.apa.pekerjaan.anda.saat.ini=character(),
      )
    }
  }
  
  observeEvent(input$cancelIdentitas, {
    session$sendCustomMessage("form_update_aspek", "0")
  })
  
  observeEvent(input$update_aspek_id, {
    id <- as.integer(input$update_aspek_id)
    data <- loadDataIdentitasResponden()
    data <- data[data$No == id, ]
    
    updateSelectInput(session, 'Jenis.kelamin', selected =data$Jenis.kelamin)
    updateNumericInput(session, 'Usia.tahun', value=data$Usia.tahun)
    updateSelectInput(session, 'Status.perkawinan', selected=data$Status.perkawinan)
    updateSelectInput(session, 'Apakah.memiliki.anak', selected=data$Apakah.memiliki.anak)
    updateNumericInput(session, 'Jumlah.anak.orang', value=data$Jumlah.anak.orang)
    updateSelectInput(session, 'Tingkat.pendidikan', selected=data$Tingkat.pendidikan)
    updateSelectInput(session, 'Apakah.anda.bekerja.saat.ini', selected=data$Apakah.anda.bekerja.saat.ini)
    updateTextInput(session, 'Jika.bekerja.apa.pekerjaan.anda.saat.ini', value=data$Jika.bekerja.apa.pekerjaan.anda.saat.ini)
    updateTextInput(session, 'Nama_Identitas', value=data$Nama)
    
    session$sendCustomMessage("selected_id_aspek", id)
  })
  
  observeEvent(input$updateIdentitas, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data <- loadDataIdentitasResponden()
      
      data[data$No == input$selected_id_aspek, ] <- data.frame(
        No = input$selected_id_aspek,
        Nama=input$Nama_Identitas,	
        Jenis.kelamin=input$Jenis.kelamin,	
        Usia.tahun=input$Usia.tahun,	
        Status.perkawinan=input$Status.perkawinan,	
        Apakah.memiliki.anak=input$Apakah.memiliki.anak,	
        Jumlah.anak.orang=input$Jumlah.anak.orang,	
        Tingkat.pendidikan=input$Tingkat.pendidikan,	
        Apakah.anda.bekerja.saat.ini=input$Apakah.anda.bekerja.saat.ini,	
        Jika.bekerja.apa.pekerjaan.anda.saat.ini=input$Jika.bekerja.apa.pekerjaan.anda.saat.ini,
        stringsAsFactors = FALSE
      )
      
      saveData <- function(data, path) {
        tryCatch({
          write.csv(data, path, row.names = FALSE)
          TRUE
        }, error = function(e) {
          FALSE
        })
      }
      
      temp <- paste0(pathIdentitasResponden, ".tmp")
      
      if(saveData(data, temp)){
        file.rename(temp, pathIdentitasResponden)
        render_server_identitas(FALSE)
        removeModal()
        
        session$sendCustomMessage("form_update_aspek", "0")
        
        showModal(modalDialog(
          title = "Success",
          "Data berhasil diperbarui",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        unlink(temp)
        
        removeModal()
        
        showModal(modalDialog(
          title = "Error",
          "Gagal memperbrui data.",
          easyClose = TRUE,
          footer = NULL
        ))
      }
    }
    params = TRUE
  })
}

render_server_identitas(TRUE)