render_server_aspek_sosial <- function(params) {
  
  pathTambahAspekSosial <- 'data/AspekSosial.csv'
  
  output$data_table_apekSosial <- renderDT({
    data <- read_csv(pathTambahAspekSosial)%>%
      mutate(
        Apakah.Anda.tinggal.di.desa.ini.sejak.lahir = recode(
          Apakah.Anda.tinggal.di.desa.ini.sejak.lahir, 
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ), 
        Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini = recode(
          Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ), 
        Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa = recode(
          Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa,
          `4`='Sering',
          `3`='Kadang-kadang',
          `2`='Jarang',
          `1`='Tidak Pernah'
        ), 
        Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini = recode(
          Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ), 
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
    
    datatable(
      data,
      selection = 'none',
      escape = FALSE,
      rownames = TRUE,
      colnames = c(
        "No",
        "ID",
        'Apakah Anda tinggal di desa ini sejak lahir?', 
        'Jika tidak, sebutkan asal tempat', 
        'Apakah Anda aktif dalam kegiatan sosial kemasyarakatan di desa ini?',
        'Seberapa sering Anda berpartisipasi dalam pertemuan warga atau komunitas di desa?', 
        'Apakah anda merasa adanya dukungan sosial yang memadai di desa ini?', 
        "Actions"
      ),
      options = list(
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  if (!$('#aspekEkonomi-checkbox').length) {",
          "  $(thead).closest('thead').prepend(` 
      <tr id=\"aspekEkonomi-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"tinggal-didesa\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"asal-tempat\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"kegiatan-masyarakat\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pertemuan-masyarakat\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"dukungan-sosial\">
        </th> 
      </tr>
          `);",
          "  $('#tinggal-didesa').on('click', function(){",
          "    Shiny.setInputValue('tinggal_didesa', this.checked);",
          "  });",
          "  $('#kegiatan-masyarakat').on('click', function(){",
          "    Shiny.setInputValue('kegiatan_masyarakat', this.checked);",
          "  });",
          "  $('#pertemuan-masyarakat').on('click', function(){",
          "    Shiny.setInputValue('pertemuan_masyarakat', this.checked);",
          "  });",
          "  $('#asal-tempat').on('click', function(){",
          "    Shiny.setInputValue('asal_tempat', this.checked);",
          "  });",
          "  $('#dukungan-sosial').on('click', function(){",
          "    Shiny.setInputValue('dukungan_sosial', this.checked);",
          "  });",
          "  }",
          "}"
        ), 
        scrollX = TRUE,
        columnDefs = list(
          list(orderable = FALSE, className = 'select-checkbox-aspek-sosial', targets = 0),
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
  
  # Update Sosial
  loadDataAspekSosial <- function() {
    if (file.exists(pathTambahAspekSosial)) {
      read.csv(pathTambahAspekSosial, stringsAsFactors = FALSE)
    } else {
      data.frame(
        No = character(),
        Apakah.Anda.tinggal.di.desa.ini.sejak.lahir = character(), Jika.tidak.sebutkan.asal.tempat =
          character(), Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini =
          character(), Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa =
          character(), Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini =
          character(), stringsAsFactors = FALSE
      )
    }
  }
  
  observeEvent(input$cancelAspekSosial, {
    session$sendCustomMessage("form_update_aspek", "0")
  })
  
  observeEvent(input$update_aspek_id, {
    id <- as.integer(input$update_aspek_id)
    data <- loadDataAspekSosial()
    data <- data[data$No == id, ]
    
    updateSelectInput(
      session,
      'Apakah.Anda.tinggal.di.desa.ini.sejak.lahir',
      selected = data$Apakah.Anda.tinggal.di.desa.ini.sejak.lahir
    )	
    updateTextInput(
      session,
      'Jika.tidak.sebutkan.asal.tempat',
      value = data$Jika.tidak.sebutkan.asal.tempat
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini',
      selected = data$Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa',
      selected = data$Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa
    )	
    updateSelectInput(
      session,
      'Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini',
      selected = data$Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini
    )
    
    session$sendCustomMessage("selected_id_aspek", id)
  })
  
  observeEvent(input$updateAspekSosial, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data <- loadDataAspekSosial()
      
      data[data$No == input$selected_id_aspek, ] <- data.frame(
        No = input$selected_id_aspek,
        Apakah.Anda.tinggal.di.desa.ini.sejak.lahir = input$Apakah.Anda.tinggal.di.desa.ini.sejak.lahir, Jika.tidak.sebutkan.asal.tempat =
          input$Jika.tidak.sebutkan.asal.tempat, Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini =
          input$Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini, Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa =
          input$Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa, Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini =
          input$Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini, 
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
      
      temp <- paste0(pathTambahAspekSosial, ".tmp")
      
      if(saveData(data, temp)){
        file.rename(temp, pathTambahAspekSosial)
        render_server_aspek_sosial(FALSE)
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

render_server_aspek_sosial(TRUE)