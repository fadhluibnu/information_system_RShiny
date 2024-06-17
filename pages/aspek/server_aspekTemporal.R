render_server_aspek_temporal <- function(params) {
  
  pathTambahAspekTemporal <- 'data/AspekTemporal.csv'
  
  output$data_table_apekTemporal <- renderDT({
    data <- read_csv(pathTambahAspekTemporal)%>%
      mutate(
        Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama= recode(
          Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),
        Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir= recode(
          Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),
        
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
        "No",
        "ID",
        'Berapa lama Anda sudah tinggal di desa ini? (tahun)',	
        'Apakah Anda berencana untuk tinggal di desa ini dalam jangka waktu yang lama?',	
        'Apakah Anda melihat perubahan signifikan dalam kondisi ekonomi dan sosial desa dalam beberapa tahun terakhir?',	
        'Apakah Anda memiliki saran atau masukan untuk meningkatkan aspek sosial-ekonomi di desa ini?',
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
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"tahun-tinggal\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pindah-tempat\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"perubahan-ekonomi\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"saran-ekonomi\">
        </th> 
      </tr>
          `);",
          "  $('#tahun-tinggal').on('click', function(){",
          "    Shiny.setInputValue('tahun_tinggal', this.checked);",
          "  });",
          "  $('#pindah-tempat').on('click', function(){",
          "    Shiny.setInputValue('pindah_tempat', this.checked);",
          "  });",
          "  $('#perubahan-ekonomi').on('click', function(){",
          "    Shiny.setInputValue('perubahan_ekonomi', this.checked);",
          "  });",
          "  $('#saran-ekonomi').on('click', function(){",
          "    Shiny.setInputValue('saran_ekonomi', this.checked);",
          "  });",
          "  }",
          "}"
        ), 
        scrollX = TRUE,
        columnDefs = list(
          list(orderable = FALSE, className = 'select-checkbox-aspek-temporal', targets = 0),
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
  loadDataAspekTemporal <- function() {
    if (file.exists(pathTambahAspekTemporal)) {
      read.csv(pathTambahAspekTemporal, stringsAsFactors = FALSE)
    } else {
      data.frame(
        No = character(),
        Berapa.lama.Anda.sudah.tinggal.di.desa.ini.tahun = character(), Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama =
          character(), Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir =
          character(), Apakah.Anda.memiliki.saran.atau.masukan.untuk.meningkatkan.aspek.sosial.ekonomi.di.desa.ini = character(), 
        stringsAsFactors = FALSE
      )
    }
  }
  
  observeEvent(input$cancelAspekTemporal, {
    session$sendCustomMessage("form_update_aspek", "0")
  })
  
  observeEvent(input$update_aspek_id, {
    id <- as.integer(input$update_aspek_id)
    data <- loadDataAspekTemporal()
    data <- data[data$No == id, ]
    
    updateTextInput(
      session,
      'Berapa.lama.Anda.sudah.tinggal.di.desa.ini.tahun',
      value = data$Berapa.lama.Anda.sudah.tinggal.di.desa.ini.tahun
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama',
      selected = data$Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir',
      selected = data$Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir
    )	
    updateTextAreaInput(
      session,
      'Apakah.Anda.memiliki.saran.atau.masukan.untuk.meningkatkan.aspek.sosial.ekonomi.di.desa.ini',
      value = data$Apakah.Anda.memiliki.saran.atau.masukan.untuk.meningkatkan.aspek.sosial.ekonomi.di.desa.ini
    )
    
    
    session$sendCustomMessage("selected_id_aspek", id)
  })
 
  observeEvent(input$updateAspekTemporal, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data <- loadDataAspekTemporal()
      
      data[data$No == input$selected_id_aspek, ] <- data.frame(
        No = input$selected_id_aspek,
        Berapa.lama.Anda.sudah.tinggal.di.desa.ini.tahun = input$Berapa.lama.Anda.sudah.tinggal.di.desa.ini.tahun, Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama =
          input$Apakah.Anda.berencana.untuk.tinggal.di.desa.ini.dalam.jangka.waktu.yang.lama, Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir =
          input$Apakah.Anda.melihat.perubahan.signifikan.dalam.kondisi.ekonomi.dan.sosial.desa.dalam.beberapa.tahun.terakhir, Apakah.Anda.memiliki.saran.atau.masukan.untuk.meningkatkan.aspek.sosial.ekonomi.di.desa.ini = input$Apakah.Anda.memiliki.saran.atau.masukan.untuk.meningkatkan.aspek.sosial.ekonomi.di.desa.ini, 
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
      
      temp <- paste0(pathTambahAspekTemporal, ".tmp")
      
      if(saveData(data, temp)){
        file.rename(temp, pathTambahAspekTemporal)
        render_server_aspek_temporal(FALSE)
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

render_server_aspek_temporal(TRUE)