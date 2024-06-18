render_server_aspek_spatial <- function(params){
  
  pathTambahAspekSpatial <- 'data/AspekSpatial.csv'
  
  
  output$data_table_apekSpatial <- renderDT({
    data <- read_csv(pathTambahAspekSpatial)%>%
      mutate(
        Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini = recode(
          Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini,
          `1`='Jalan Kaki',
          `2`='Sepeda',
          `3`='Sepeda Motor',
          `4`='Mobil',
          `5`='Angkutan Umum',
          `6`='Lainnya'
        ), 
        Apakah.transportasi.umum.cukup.memadai.di.desa.ini = recode(
          Apakah.transportasi.umum.cukup.memadai.di.desa.ini, 
          `4`='Sangat memadai',
          `3`='Cukup memadai',
          `2`='Tidak cukup',
          `1`='Tidak ada'
        ), 
        Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini = recode(
          Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini, 
          `4`='Baik',
          `3`='Cukup baik',
          `2`='Tidak cukup baik',
          `1`='Tidak ada'
        ), 
        Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah = recode(
          Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ), 
        Bagaimana.pasokan.air.bersih.di.desa.ini = recode(
          Bagaimana.pasokan.air.bersih.di.desa.ini, 
          `4`='Baik',
          `3`='Cukup baik',
          `2`='Tidak cukup baik',
          `1`='Tidak ada'
        ), 
        Apakah.Anda.menghadapi.masalah.terkait.air.bersih = recode(
          Apakah.Anda.menghadapi.masalah.terkait.air.bersih, 
          `4`='Sering',
          `3`='Kadang-kadang',
          `2`='Jarang',
          `1`='Tidak Pernah'
        ), 
        Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya = recode(
          Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ), 
        Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk = recode(
          Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ), 
      )
    
    action_buttons <- if (!is.null(input$stored_user)) {
      paste0(
        '<button class="update-btn-aspek" data-id="', data$No, '">Update</button>',
        '<button class="delete-btn-aspek" data-id="', data$No, '">Delete</button>'
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
        'Seberapa jauh rumah Anda dari pusat desa atau kota terdekat? (Kec. Sagaranten) .... KM', 
        'Bagaimana Anda biasanya mengakses fasilitas umum seperti pasar, sekolah, dan rumah sakit di desa ini?', 
        'Apakah transportasi umum cukup memadai di desa ini?', 
        'Bagaimana kondisi infrastruktur jalan di desa ini? ', 
        'Apakah terdapat masalah terkait sanitasi di desa ini, seperti akses ke fasilitas toilet yang memadai atau masalah limbah?', 
        'Bagaimana pasokan air bersih di desa ini? ', 
        'Apakah Anda menghadapi masalah terkait air bersih?', 
        'Apakah terdapat masalah terkait polusi di desa ini, seperti polusi udara atau pencemaran lingkungan lainnya?', 
        'Apakah Anda merasa bahwa infrastruktur dan fasilitas umum di desa ini memadai untuk memenuhi kebutuhan sehari-hari penduduk? ', 
        "Actions"
      ),
      options = list(
        headerCallback = JS(
          "function(thead, data, start, end, display){",
          "  if (!$('#aspekSpatial-checkbox').length) {",
          "  $(thead).closest('thead').prepend(` 
      <tr id=\"aspekSpatial-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th>  
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"jauh_rumah\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"akses_fasilitas\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"transportasi_memadai\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"kondisi_infrastruktur\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"sanitasi_desa\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pasokan_air\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"air_bersih\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"polusi\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"keberadaan_fasilitas\">
        </th> 
      </tr>
          `);",
          "  $('#jauh_rumah').on('click', function(){",
          "    Shiny.setInputValue('jauh_rumah', this.checked);",
          "  });",
          "  $('#akses_fasilitas').on('click', function(){",
          "    Shiny.setInputValue('akses_fasilitas', this.checked);",
          "  });",
          "  $('#transportasi_memadai').on('click', function(){",
          "    Shiny.setInputValue('transportasi_memadai', this.checked);",
          "  });",
          "  $('#kondisi_infrastruktur').on('click', function(){",
          "    Shiny.setInputValue('kondisi_infrastruktur', this.checked);",
          "  });",
          "  $('#sanitasi_desa').on('click', function(){",
          "    Shiny.setInputValue('sanitasi_desa', this.checked);",
          "  });",
          "  $('#pasokan_air').on('click', function(){",
          "    Shiny.setInputValue('pasokan_air', this.checked);",
          "  });",
          "  $('#air_bersih').on('click', function(){",
          "    Shiny.setInputValue('air_bersih', this.checked);",
          "  });",
          "  $('#polusi').on('click', function(){",
          "    Shiny.setInputValue('polusi', this.checked);",
          "  });",
          "  $('#keberadaan_fasilitas').on('click', function(){",
          "    Shiny.setInputValue('keberadaan_fasilitas', this.checked);",
          "  });",
          "  }",
          "}"
        ), 
        scrollX = TRUE,
        columnDefs = list(
          list(orderable = FALSE, className = 'select-checkbox-aspek-spatial', targets = 0),
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
  loadDataAspekSpatial <- function() {
    if (file.exists(pathTambahAspekSpatial)) {
      read.csv(pathTambahAspekSpatial, stringsAsFactors = FALSE)
    } else {
      data.frame(
        No = character(),
        Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM =
          character(), Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini =
          character(), Apakah.transportasi.umum.cukup.memadai.di.desa.ini = character(), Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini =
          character(), Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah =
          character(), Bagaimana.pasokan.air.bersih.di.desa.ini = character(), Apakah.Anda.menghadapi.masalah.terkait.air.bersih =
          character(), Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya =
          character(), Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk =
          character(), 
        stringsAsFactors = FALSE
      )
    }
  }
  
  observeEvent(input$cancelAspekSpatial, {
    session$sendCustomMessage("form_update_aspek", "0")
  })
  
  observeEvent(input$update_aspek_id, {
    id <- as.integer(input$update_aspek_id)
    data <- loadDataAspekSpatial()
    data <- data[data$No == id, ]
    
    updateNumericInput(
      session,
      'Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM.edit',
      value = data$Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM
    )	
    updateSelectInput(
      session,
      'Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini.edit',
      selected = data$Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Apakah.transportasi.umum.cukup.memadai.di.desa.ini.edit',
      selected = data$Apakah.transportasi.umum.cukup.memadai.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini.edit',
      selected = data$Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah.edit',
      selected = data$Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah
    )	
    updateSelectInput(
      session,
      'Bagaimana.pasokan.air.bersih.di.desa.ini.edit',
      selected = data$Bagaimana.pasokan.air.bersih.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.menghadapi.masalah.terkait.air.bersih.edit',
      selected = data$Apakah.Anda.menghadapi.masalah.terkait.air.bersih
    )	
    updateSelectInput(
      session,
      'Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya.edit',
      selected = data$Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk.edit',
      selected = data$Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk
    )
    
    
    session$sendCustomMessage("selected_id_aspek", id)
  })
  
  observeEvent(input$updateAspekSpatial, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data <- loadDataAspekSpatial()
      
      data[data$No == input$selected_id_aspek, ] <- data.frame(
        No = input$selected_id_aspek,
        Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM =
          input$Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM.edit, 
        Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini =
          input$Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini.edit, 
        Apakah.transportasi.umum.cukup.memadai.di.desa.ini =
          input$Apakah.transportasi.umum.cukup.memadai.di.desa.ini.edit, 
        Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini =
          input$Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini.edit, 
        Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah =
          input$Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah.edit, 
        Bagaimana.pasokan.air.bersih.di.desa.ini =
          input$Bagaimana.pasokan.air.bersih.di.desa.ini.edit, 
        Apakah.Anda.menghadapi.masalah.terkait.air.bersih =
          input$Apakah.Anda.menghadapi.masalah.terkait.air.bersih.edit, 
        Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya =
          input$Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya.edit, 
        Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk =
          input$Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk.edit, 
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
      
      temp <- paste0(pathTambahAspekSpatial, ".tmp")
      
      if(saveData(data, temp)){
        file.rename(temp, pathTambahAspekSpatial)
        render_server_aspek_spatial(FALSE)
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

render_server_aspek_spatial(TRUE)