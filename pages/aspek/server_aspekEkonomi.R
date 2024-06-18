render_server_aspek_ekonomi <- function(params) {
  pathTambahAspekEkonomi <- "data/AspekEkonomi.csv"
  
  output$data_table_apekEkonomi <- renderDT({
    data <- read_csv(pathTambahAspekEkonomi) %>%
      mutate(
        Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini= recode(
          Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),	
        Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah= recode(
          Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah,`1`='500.000 - 1.500.000',
          `2`='1.500.001 - 3.000.000',
          `3`='3.000.001 - 4.500.000',
          `4`='4.500.001 - ke atas'
        )
        ,	
        Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial= recode(
          Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),	
        Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini= recode(
          Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini,
          `4`='Sangat stabil',
          `3`='Stabil',
          `2`='Tidak stabil',
          `1`='Sangat tidak stabil'
        ),	
        Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda= recode(
          Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),	
        Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini= recode(
          Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini,
          `1`='Ya',
          `2`='Tidak',
          `3`='Belum tahu'
        ),	
        Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini= recode(
          Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini,
          `4`='Sangat terbuka',
          `3`='Terbuka',
          `2`='Tidak terbuka',
          `1`='Sangat tidak terbuka'
        ),	
        Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini= recode(
          Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini,
          `4`='Sangat terasa',
          `3`='Terasa',
          `2`='Tidak terasa',
          `1`='Sangat tidak terasa'
        )
        
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
        'No',
        'ID',
        'Apakah Anda atau anggota keluarga Anda memiliki usaha sendiri di desa ini? ',	
        'Berapa pendapatan bulanan Anda atau anggota keluarga Anda secara keseluruhan? (Kisaran jumlah)',	
        'Apakah Anda atau anggota keluarga Anda menerima bantuan sosial?',	
        'Jika ya, sebutkan',	
        'Bagaimana Anda menilai stabilitas harga pangan saat ini?',	
        'Apakah Anda memiliki simpanan atau investasi di luar penghasilan bulanan Anda?',	
        'Apakah Anda merasa terdapat peluang ekonomi yang cukup di desa ini?',	
        'Bagaimana Anda menilai aksesibilitas dan ketersediaan lapangan pekerjaan di desa ini?',	
        'Apakah Anda merasa terdapat kesenjangan ekonomi di antara penduduk di desa ini?',
        'Actions'
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
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"memiliki-usaha\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pendapatan-bulanan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"bantuan-sosial\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"sebutkan-bantuan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"stabilitas-harga\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"simpanan-investasi\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"peluang-ekonomi\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"ketersedian-pekerjaan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"kesenjangan-ekonomi\">
        </th> 
      </tr>
          `);",
          "  $('#memiliki-usaha').on('click', function(){",
          "    Shiny.setInputValue('memiliki_usaha', this.checked);",
          "  });",
          "  $('#pendapatan-bulanan').on('click', function(){",
          "    Shiny.setInputValue('pendapatan_bulanan', this.checked);",
          "  });",
          "  $('#bantuan-sosial').on('click', function(){",
          "    Shiny.setInputValue('bantuan_sosial', this.checked);",
          "  });",
          "  $('#sebutkan-bantuan').on('click', function(){",
          "    Shiny.setInputValue('sebutkan_bantuan', this.checked);",
          "  });",
          "  $('#stabilitas-harga').on('click', function(){",
          "    Shiny.setInputValue('stabilitas_harga', this.checked);",
          "  });",
          "  $('#simpanan-investasi').on('click', function(){",
          "    Shiny.setInputValue('simpanan_investasi', this.checked);",
          "  });",
          "  $('#peluang-ekonomi').on('click', function(){",
          "    Shiny.setInputValue('peluang_ekonomi', this.checked);",
          "  });",
          "  $('#ketersedian-pekerjaan').on('click', function(){",
          "    Shiny.setInputValue('ketersedian_pekerjaan', this.checked);",
          "  });",
          "  $('#kesenjangan-ekonomi').on('click', function(){",
          "    Shiny.setInputValue('kesenjangan_ekonomi', this.checked);",
          "  });",
          "  }",
          "}"
        ), 
        scrollX = TRUE,
        columnDefs = list(
          list(orderable = FALSE, className = 'select-checkbox-aspek-ekonomi', targets = 0),
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
  
  
  # Update Ekonomi
  loadDataAspekEkonomi <- function() {
    if (file.exists(pathTambahAspekEkonomi)) {
      read.csv(pathTambahAspekEkonomi, stringsAsFactors = FALSE)
    } else {
      data.frame(
        No = character(),
        Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini =
          character(), 
        Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah =
          character(), 
        Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial =
          character(), 
        Jika.ya.sebutkan.aspek.ekonomi = character(), 
        Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini =
          character(), 
        Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda =
          character(), 
        Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini =
          character(), 
        Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini =
          character(), 
        Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini =
          character(),
        stringsAsFactors = FALSE
      )
    }
  }
  
  observeEvent(input$cancelAspekEkonomi, {
    session$sendCustomMessage("form_update_aspek", "0")
  })
  
  observeEvent(input$update_aspek_id, {
    id <- as.integer(input$update_aspek_id)
    data <- loadDataAspekEkonomi()
    data <- data[data$No == id, ]
    
    updateSelectInput(
      session,
      'Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini.edit',
      selected = data$Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah.edit',
      selected = data$Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial.edit',
      selected = data$Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial
    )	
    updateTextInput(session, 'Jika.ya.sebutkan.aspek.ekonomi.edit', value = data$Jika.ya.sebutkan.aspek.ekonomi)	
    updateSelectInput(
      session,
      'Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini.edit',
      selected = data$Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda.edit',
      selected = data$Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini.edit',
      selected = data$Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini.edit',
      selected = data$Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini
    )	
    updateSelectInput(
      session,
      'Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini.edit',
      selected = data$Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini
    )
    
    
    session$sendCustomMessage("selected_id_aspek", id)
  })
  
  observeEvent(input$updateAspekEkonomi, {
    
    if (params == TRUE){
      showModal(modalDialog(
        title = "Loading...",
        "Proses Update Data",
        easyClose = FALSE,
        footer = NULL
      ))
      
      data <- loadDataAspekEkonomi()
      
      data[data$No == input$selected_id_aspek, ] <- data.frame(
        No = input$selected_id_aspek,
        Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini = input$Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini.edit, 
        Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah = input$Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah.edit, 
        Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial = input$Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial.edit, 
        Jika.ya.sebutkan.aspek.ekonomi = input$Jika.ya.sebutkan.aspek.ekonomi.edit, 
        Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini = input$Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini.edit, 
        Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda = input$Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda.edit, 
        Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini = input$Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini.edit, 
        Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini = input$Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini.edit, 
        Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini = input$Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini.edit, 
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
      
      temp <- paste0(pathTambahAspekEkonomi, ".tmp")
      
      if(saveData(data, temp)){
        file.rename(temp, pathTambahAspekEkonomi)
        render_server_aspek_ekonomi(FALSE)
        
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
render_server_aspek_ekonomi(TRUE)