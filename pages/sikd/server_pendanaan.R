render_server_pendanaan <- function() {
  
  data_pendanaan <- read_csv("data/Pendanaan.csv")
  
  output$data_table_pendanaan <- renderDT({
    data_pendanaan_tabel <- read_csv("data/Pendanaan.csv")
    data_pendanaan_tabel <- data_pendanaan_tabel %>%
      mutate(
        
        Apakah.BapakIbu.tahu.mengenai.pendanaan.untuk.mengembangkan.usaha = ifelse(Apakah.BapakIbu.tahu.mengenai.pendanaan.untuk.mengembangkan.usaha == 1, "Tahu", "Tidak Tahu"),
        
        Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.desa = ifelse(Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.desa == 1, "Tahu", "Tidak Tahu"),
        
        Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.CSR.Coorporate.Social.Responsibility
        = ifelse(Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.CSR.Coorporate.Social.Responsibility
                 == 1, "Tahu", "Tidak Tahu"),
        
        Modal.Usaha.Bapak.Ibu.diperoleh.dari = recode(Modal.Usaha.Bapak.Ibu.diperoleh.dari, 
                                                      `1` = "Modal sendiri",
                                                      `2` = "Pinjam saudara",
                                                      `3` = "Pinjam bank",
                                                      `4` = "Bantuan desa",
                                                      `5` = "Bantuan dana CSR",
                                                      `6` = "Lainnya"),
        
        Apakah.Bapak.Ibu.mengetahui.adanya.perusahaan.listrik
        = ifelse(Apakah.Bapak.Ibu.mengetahui.adanya.perusahaan.listrik
                 == 1, "Tahu", "Tidak Tahu"),
        
        Apakah.perusahaan.memberikan.bantuan.buat.masyarakat.desa = recode(Apakah.perusahaan.memberikan.bantuan.buat.masyarakat.desa, 	
                                                                           `2`="Tidak Tahu",	
                                                                           `1`="Tahu")
        ,
        dalam.bentuk=recode(dalam.bentuk,
                            `1`="Sumbangan tidak mengikat (hibah)",
                            `2`="Sumbangan bergulir",
                            `3`="Pelatihan",
                            `4`="Bimbingan teknis usaha",
                            `5`="Lainnya",
                            .default ="-",
                            .missing = "-"),
        
        Apakah.pemerintah.desa.memberikan.bantuan.buat.masyarakat=recode(Apakah.pemerintah.desa.memberikan.bantuan.buat.masyarakat, 
                                                                         `2`="Tidak Tahu",	
                                                                         `1`="Tahu"),
        
        Jika.ya.dalam.bentuk=recode(Jika.ya.dalam.bentuk,
                                    `1`="Sumbangan tidak mengikat (hibah)",
                                    `2`="Sumbangan bergulir",
                                    `3`="Pelatihan",
                                    `4`="Bimbingan teknis usaha",
                                    `5`="Lainnya",
                                    .default ="-",
                                    .missing = "-")
        
        
      )
    
    
    data_pendanaan_tabel <- data_pendanaan_tabel %>%
      rename(
        `Apakah Bapak/Ibu tahu mengenai pendanaan untuk mengembangkan usaha?` = Apakah.BapakIbu.tahu.mengenai.pendanaan.untuk.mengembangkan.usaha,
        `Apakah Bapak/Ibu tahu yang dimaksud dengan dana desa?` = Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.desa,
        
        `Apakah Bapak/Ibu tahu yang dimaksud dengan dana CSR (Coorporate Social Responsibility)?` = Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.CSR.Coorporate.Social.Responsibility,
        
        `Modal Usaha Bapak/Ibu diperoleh dari` = Modal.Usaha.Bapak.Ibu.diperoleh.dari,
        
        `Modal awal (Rp)` = Modal.awal,
        
        `Apakah Bapak/Ibu mengetahui adanya perusahaan listrik?` = Apakah.Bapak.Ibu.mengetahui.adanya.perusahaan.listrik,
        
        `Jika tahu, sudah berapa lama perusahaan beraktifitas? (tahun)`=Jika.tahu.sudah.berapa.lama.perusahaan.beraktifitas.tahun,
        
        `Jika ya, dalam bentuk`=dalam.bentuk,
        
        `Apakah perusahaan memberikan bantuan buat masyarakat desa?` = Apakah.perusahaan.memberikan.bantuan.buat.masyarakat.desa,
        `Apakah pemerintah desa memberikan bantuan buat masyarakat?` = Apakah.pemerintah.desa.memberikan.bantuan.buat.masyarakat,
        
        `Jika iya, dalam bentuk` =Jika.ya.dalam.bentuk
      )
    
    
    datatable(data_pendanaan_tabel, options = list(
      headerCallback = JS(
        "function(thead, data, start, end, display){",
        "  if (!$('#pendanaan-checkbox').length) {",
        "  $(thead).closest('thead').prepend(`
      <tr id=\"pendanaan-checkbox\" style=\"position: relative;top: 10px;\"> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"PengembanganUsaha\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"DanaDesa\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"CRS\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"SubmerModal\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"ModalAwal\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"PerusahaanListrik\">
        </th>
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"PerusahaanListrik2\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"PerusahaanListrik3\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"BantuanDesa\">
        </th>
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"BantuanDesa2\">
        </th>
      </tr>`);",
        "  $('#PengembanganUsaha').on('click', function(){",
        "    Shiny.setInputValue('PengembanganUsaha', this.checked);",
        "  });",
        "  $('#DanaDesa').on('click', function(){",
        "    Shiny.setInputValue('DanaDesa', this.checked);",
        "  });",
        "  $('#CRS').on('click', function(){",
        "    Shiny.setInputValue('CRS', this.checked);",
        "  });",
        "  $('#SubmerModal').on('click', function(){",
        "    Shiny.setInputValue('SubmerModal', this.checked);",
        "  });",
        "  $('#ModalAwal').on('click', function(){",
        "    Shiny.setInputValue('ModalAwal', this.checked);",
        "  });",
        "  $('#PerusahaanListrik').on('click', function(){",
        "    Shiny.setInputValue('PerusahaanListrik', this.checked);",
        "  });",
        "  $('#PerusahaanListrik2').on('click', function(){",
        "    Shiny.setInputValue('PerusahaanListrik2', this.checked);",
        "  });",
        "  $('#PerusahaanListrik3').on('click', function(){",
        "    Shiny.setInputValue('PerusahaanListrik2', this.checked);",
        "  });",
        "  $('#BantuanDesa').on('click', function(){",
        "    Shiny.setInputValue('BantuanDesa', this.checked);",
        "  });",
        "  $('#BantuanDesa2').on('click', function(){",
        "    Shiny.setInputValue('BantuanDesa2', this.checked);",
        "  });",
        "  }",
        "}"
      ),
      columnDefs = list(
        list(orderable = FALSE, className = 'select-checkbox-pendanaan', targets = 0)
      ),
      select = list(style = 'multi', selector = 'td:first-child'),
      scrollX = TRUE, 
      autoWidth = TRUE
    ), selection = 'none', rownames = FALSE)
  })
  
  
  # Dana Pengembangan Usaha
  output$pieChartPengembanganUsaha <- renderPlot({
    tahu_dana_desa_data <- data_pendanaan %>%
      rename(`PengembanganUsaha` = Apakah.BapakIbu.tahu.mengenai.pendanaan.untuk.mengembangkan.usaha)
    
    tahu_dana_desa_data <- tahu_dana_desa_data %>%
      mutate(PengembanganUsaha = ifelse(PengembanganUsaha == 1, "Tahu", "Tidak Tahu"))
    
    tahu_dana_desa_data <- tahu_dana_desa_data %>%
      count(PengembanganUsaha) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(PengembanganUsaha, "\n", round(percentage, 1), "%"))
    
    ggplot(tahu_dana_desa_data, aes(x = "", y = n, fill = PengembanganUsaha)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
  
  output$analysisPengembanganUsaha <- renderText({
    tahu_dana_desa_data <- data_pendanaan %>%
      rename(`PengembanganUsaha` = Apakah.BapakIbu.tahu.mengenai.pendanaan.untuk.mengembangkan.usaha)
    
    
    tahu_dana_desa_data <- tahu_dana_desa_data %>%
      mutate(PengembanganUsaha = ifelse(PengembanganUsaha == 1, "Tahu", "Tidak Tahu"))
    
    tahu_dana_desa_data <- tahu_dana_desa_data %>%
      count(PengembanganUsaha) %>%
      mutate(percentage = n / sum(n) * 100)
    
    tahu_count <- tahu_dana_desa_data %>%
      filter(PengembanganUsaha == "Tahu") %>%
      pull(n)
    
    tidak_tahu_count <- tahu_dana_desa_data %>%
      filter(PengembanganUsaha == "Tidak Tahu") %>%
      pull(n)
    
    tahu_percentage <- tahu_dana_desa_data %>%
      filter(PengembanganUsaha == "Tahu") %>%
      pull(percentage)
    
    tidak_tahu_percentage <- tahu_dana_desa_data %>%
      filter(PengembanganUsaha == "Tidak Tahu") %>%
      pull(percentage)
    
    # Kesimpulan dan Solusi
    if (tahu_percentage > tidak_tahu_percentage) {
      conclusion <- "Dari hasil survey, banyak masyarakat sudah mengerti tentang pendanaan untuk mengembangkan usaha."
      solution <- "Pemerintah desa dapat melanjutkan program edukasi dan informasi tentang pendanaan untuk mengembangkan usaha untuk mempertahankan tingkat pemahaman masyarakat."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat belum mengerti tentang pendanaan untuk mengembangkan usaha."
      solution <- "Pemerintah desa harus meningkatkan program edukasi dan informasi tentang pendanaan untuk mengembangkan usaha untuk meningkatkan tingkat pemahaman masyarakat."
    }
    
    analysis <- paste(
      "Dari hasil survey, sebanyak", tahu_count, "orang (", round(tahu_percentage, 1), "%) mengetahui mengenai pendanaan untuk mengembangkan usaha.",
      "Sementara itu, sebanyak", tidak_tahu_count, "orang (", round(tidak_tahu_percentage, 1), "%) tidak mengetahui mengenai pendanaan untuk mengembangkan usaha.",
      conclusion,
      solution
    )
    
    analysis
  })
  
  # Dana Desa
  output$pieChartDanaDesa <- renderPlot({
    tahu_dana_desa_data <- data_pendanaan %>%
      rename(`DanaDesa` = Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.desa)
    
    tahu_dana_desa_data <- tahu_dana_desa_data %>%
      mutate(DanaDesa = ifelse(DanaDesa == 1, "Tahu", "Tidak Tahu"))
    
    tahu_dana_desa_data <- tahu_dana_desa_data %>%
      count(DanaDesa) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(DanaDesa, "\n", round(percentage, 1), "%"))
    
    ggplot(tahu_dana_desa_data, aes(x = "", y = n, fill = DanaDesa)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
  
  
  
  output$analysisPengetahuanDanaDesa <- renderText({
    tahu_dana_desa_data <- data_pendanaan %>%
      rename(`DanaDesa` = Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.desa)
    
    
    tahu_dana_desa_data <- tahu_dana_desa_data %>%
      mutate(DanaDesa = ifelse(DanaDesa == 1, "Tahu", "Tidak Tahu"))
    
    tahu_dana_desa_data <- tahu_dana_desa_data %>%
      count(DanaDesa) %>%
      mutate(percentage = n / sum(n) * 100)
    
    tahu_count <- tahu_dana_desa_data %>%
      filter(DanaDesa == "Tahu") %>%
      pull(n)
    
    tidak_tahu_count <- tahu_dana_desa_data %>%
      filter(DanaDesa == "Tidak Tahu") %>%
      pull(n)
    
    tahu_percentage <- tahu_dana_desa_data %>%
      filter(DanaDesa == "Tahu") %>%
      pull(percentage)
    
    tidak_tahu_percentage <- tahu_dana_desa_data %>%
      filter(DanaDesa == "Tidak Tahu") %>%
      pull(percentage)
    
    # Kesimpulan dan Solusi
    if (tahu_percentage > tidak_tahu_percentage) {
      conclusion <- "Dari hasil survey, banyak masyarakat sudah mengerti tentang Dana Desa."
      solution <- "Pemerintah desa dapat melanjutkan program edukasi dan informasi tentang Dana Desa untuk mempertahankan tingkat pemahaman masyarakat."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat belum mengerti tentang pendanaan untuk Dana Desa."
      solution <- "Pemerintah desa harus meningkatkan program edukasi dan informasi tentang Dana Desa untuk meningkatkan tingkat pemahaman masyarakat."
    }
    
    analysis <- paste(
      "Dari hasil survey, sebanyak", tahu_count, "orang (", round(tahu_percentage, 1), "%) mengetahui mengenai Dana Desa.",
      "Sementara itu, sebanyak", tidak_tahu_count, "orang (", round(tidak_tahu_percentage, 1), "%) tidak mengetahui mengenai Dana Desa.",
      conclusion,
      solution
    )
    
    analysis
  })
  
  # CRS
  output$pieChartCRS <- renderPlot({
    tahu_dana_desa_data <- data_pendanaan %>%
      rename(`CRS` = Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.CSR.Coorporate.Social.Responsibility)
    
    tahu_dana_desa_data <- tahu_dana_desa_data %>%
      mutate(CRS = ifelse(CRS == 1, "Tahu", "Tidak Tahu"))
    
    tahu_dana_desa_data <- tahu_dana_desa_data %>%
      count(CRS) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(CRS, "\n", round(percentage, 1), "%"))
    
    ggplot(tahu_dana_desa_data, aes(x = "", y = n, fill = CRS)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5))+
      theme(legend.position = "bottom") +
      guides(fill = guide_legend(title = "Keterangan : "))
  })
  
  
  output$analysisPengetahuanCRS <- renderText({
    tahu_dana_desa_data <- data_pendanaan %>%
      rename(`CRS` = Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.CSR.Coorporate.Social.Responsibility)
    
    
    tahu_dana_desa_data <- tahu_dana_desa_data %>%
      mutate(CRS = ifelse(CRS == 1, "Tahu", "Tidak Tahu"))
    
    tahu_dana_desa_data <- tahu_dana_desa_data %>%
      count(CRS) %>%
      mutate(percentage = n / sum(n) * 100)
    
    tahu_count <- tahu_dana_desa_data %>%
      filter(CRS == "Tahu") %>%
      pull(n)
    
    tidak_tahu_count <- tahu_dana_desa_data %>%
      filter(CRS == "Tidak Tahu") %>%
      pull(n)
    
    tahu_percentage <- tahu_dana_desa_data %>%
      filter(CRS == "Tahu") %>%
      pull(percentage)
    
    tidak_tahu_percentage <- tahu_dana_desa_data %>%
      filter(CRS == "Tidak Tahu") %>%
      pull(percentage)
    
    # Kesimpulan dan Solusi
    if (tahu_percentage > tidak_tahu_percentage) {
      conclusion <- "Dari hasil survey, banyak masyarakat sudah mengerti tentang Dana Desa."
      solution <- "Pemerintah desa dapat melanjutkan program edukasi dan informasi tentang Dana Coorporate Social Responsibility untuk mempertahankan tingkat pemahaman masyarakat."
    } else {
      conclusion <- "Dari hasil survey, banyak masyarakat belum mengerti tentang pendanaan untuk Dana Coorporate Social Responsibility"
      solution <- "Pemerintah desa harus meningkatkan program edukasi dan informasi tentang Dana Coorporate Social Responsibility untuk meningkatkan tingkat pemahaman masyarakat."
    }
    
    analysis <- paste(
      "Dari hasil survey, sebanyak", tahu_count, "orang (", round(tahu_percentage, 1), "%) mengetahui mengenai Dana Coorporate Social Responsibility",
      "Sementara itu, sebanyak", tidak_tahu_count, "orang (", round(tidak_tahu_percentage, 1), "%) tidak mengetahui mengenai Dana Coorporate Social Responsibility",
      conclusion,
      solution
    )
    
    analysis
  })
  
  
  # Modal Usaha Table
  output$sumberModalUsahaTable <- DT::renderDataTable({
    all_categories <- c("Modal sendiri", "Pinjam saudara", "Pinjam bank", "Bantuan desa", "Bantuan dana CSR", "Lainnya")
    
    sumber_modal_data <- data_pendanaan %>%
      rename(`ModalUsaha` = Modal.Usaha.Bapak.Ibu.diperoleh.dari)
    
    modal_data <- sumber_modal_data %>%
      mutate(ModalUsaha = recode(ModalUsaha,
                                 `1` = "Modal sendiri",
                                 `2` = "Pinjam saudara",
                                 `3` = "Pinjam bank",
                                 `4` = "Bantuan desa",
                                 `5` = "Bantuan dana CSR",
                                 `6` = "Lainnya")) %>%
      count(ModalUsaha) %>%
      complete(ModalUsaha = all_categories, fill = list(n = 0))
    
    # Rename columns
    modal_data <- modal_data %>%
      rename(Indikator = ModalUsaha, Keterangan = n)
    
    # Calculate average
    avg_count <- round(mean(modal_data$Keterangan), 2)
    
    # Add the average row
    modal_data <- rbind(modal_data, data.frame(Indikator = "Rata-rata", Keterangan = avg_count))
    
    # Create datatable
    datatable(modal_data, options = list(autoWidth = TRUE, dom="t", ordering = FALSE, selection = 'none'), rownames = FALSE)
  })
  
  # Bar Chart Modal Awal
  output$barChartModalAwal <- renderPlot({
    Modal.awal_data <- data_pendanaan %>%
      rename(`ModalAwal` = Modal.awal)
    
    # Create data frame for plotting
    Modal.awal_data <- Modal.awal_data %>%
      filter(!is.na(ModalAwal))
    
    # Count the number of respondents for each modal awal
    Modal.awal_count <- Modal.awal_data %>%
      group_by(ModalAwal) %>%
      summarise(count = n()) %>%
      mutate(ModalAwal = as.character(ModalAwal))
    
    # Generate color palette with unique colors for each bar
    num_bars <- nrow(Modal.awal_count)
    colors <- scales::hue_pal()(num_bars)
    
    ggplot(Modal.awal_count, aes(x = ModalAwal, y = count, fill = ModalAwal)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      labs(title = "Distribusi Modal Awal Usaha", x = "Modal Awal (Rp)", y = "Jumlah Responden") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Bar Chart Modal Awal dan Sumber
  output$barChartModalCombined <- renderPlot({
    modal_combined_data <- data_pendanaan %>%
      rename(`ModalAwal` = Modal.awal, `ModalUsaha` = Modal.Usaha.Bapak.Ibu.diperoleh.dari)
    
    # Create data frame for plotting
    modal_combined_data <- modal_combined_data %>%
      filter(!is.na(ModalAwal) & !is.na(ModalUsaha)) %>%
      mutate(ModalAwal = as.character(ModalAwal),
             ModalUsaha = recode(ModalUsaha,
                                 `1` = "Modal sendiri",
                                 `2` = "Pinjam saudara",
                                 `3` = "Pinjam bank",
                                 `4` = "Bantuan desa",
                                 `5` = "Bantuan dana CSR",
                                 `6` = "Lainnya"))
    
    # Summarize data for plotting
    modal_combined_count <- modal_combined_data %>%
      group_by(ModalUsaha, ModalAwal) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Generate color palette with unique colors for each bar
    num_bars <- nrow(modal_combined_count)
    colors <- brewer.pal(min(num_bars, 8), "Dark2")
    
    ggplot(modal_combined_count, aes(x = ModalAwal, y = count, fill = ModalUsaha)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      labs(title = "Distribusi Modal Awal Usaha Berdasarkan Sumber", x = "Modal Awal (Rp)", y = "Jumlah Responden") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  # Keberadaan Perusahaan Listrik
  
  output$barChartTahuBantuan <- renderPlot({
    modal_combined_data <- data_pendanaan %>%
      rename(KeberadaanPerusahaanListrik = Apakah.Bapak.Ibu.mengetahui.adanya.perusahaan.listrik, 
             Bantuan = Apakah.perusahaan.memberikan.bantuan.buat.masyarakat.desa)
    
    # Filter and recode data
    modal_combined_data <- modal_combined_data %>%
      filter(!is.na(KeberadaanPerusahaanListrik) & !is.na(Bantuan)) %>%
      mutate(KeberadaanPerusahaanListrik = recode(KeberadaanPerusahaanListrik,
                                                  `2`="Tidak Tahu", 
                                                  `1`="Tahu"), 
             Bantuan = recode(Bantuan,
                              `2`="Tidak Tahu", 
                              `1`="Tahu"))
    
    # Convert to factors
    modal_combined_data$KeberadaanPerusahaanListrik <- as.factor(modal_combined_data$KeberadaanPerusahaanListrik)
    modal_combined_data$Bantuan <- as.factor(modal_combined_data$Bantuan)
    
    # Create data frame for plotting
    modal_combined_count <- modal_combined_data %>%
      group_by(KeberadaanPerusahaanListrik, Bantuan) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Define number of bars and colors
    num_bars <- nrow(modal_combined_count)
    colors <- RColorBrewer::brewer.pal(min(num_bars, 8), "Dark2")
    
    # Plot the data
    ggplot(modal_combined_count, aes(x = KeberadaanPerusahaanListrik, y = count, fill = Bantuan)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      labs(title = "Distribusi Bentuk Bantuan Berdasarkan Pengetahuan Tentang Perusahaan Listrik",
           x = "Pengetahuan Keberadaan Perusahaan Listrik",
           y = "Jumlah Responden",
           fill = "Apakah Perusahaan Memberikan Bantuan?") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, max(modal_combined_count$count), by = 1)) # Adding manual breaks for y-axis
  })
  
  output$analysisText <- renderText({
    modal_combined_data <- data_pendanaan %>%
      rename(KeberadaanPerusahaanListrik = Apakah.Bapak.Ibu.mengetahui.adanya.perusahaan.listrik, 
             Bantuan = Apakah.perusahaan.memberikan.bantuan.buat.masyarakat.desa)
    
    # Filter and recode data
    modal_combined_data <- modal_combined_data %>%
      filter(!is.na(KeberadaanPerusahaanListrik) & !is.na(Bantuan)) %>%
      mutate(KeberadaanPerusahaanListrik = recode(KeberadaanPerusahaanListrik,
                                                  `2`="Tidak Tahu", 
                                                  `1`="Tahu"), 
             Bantuan = recode(Bantuan,
                              `2`="Tidak Tahu", 
                              `1`="Tahu"))
    
    # Summary of the analysis
    total_responden <- nrow(modal_combined_data)
    tahu_dan_dapat_bantuan <- modal_combined_data %>%
      filter(KeberadaanPerusahaanListrik == "Tahu" & Bantuan == "Tahu") %>%
      nrow()
    tidak_tahu_tidak_dapat_bantuan <- modal_combined_data %>%
      filter(KeberadaanPerusahaanListrik == "Tidak Tahu" & Bantuan == "Tidak Tahu") %>%
      nrow()
    proporsi_tahu <- mean(modal_combined_data$KeberadaanPerusahaanListrik == "Tahu")
    proporsi_dapat_bantuan <- mean(modal_combined_data$Bantuan == "Tahu")
    
    paste0(
      "Dari total ", total_responden, " responden, ",
      round(proporsi_tahu * 100, 2), "% responden mengetahui adanya perusahaan listrik di desa mereka, ",
      "dan ", round(proporsi_dapat_bantuan * 100, 2), "% responden menyatakan bahwa perusahaan memberikan bantuan kepada masyarakat desa.\n",
      "Sebanyak ", tahu_dan_dapat_bantuan, " responden (",
      round(tahu_dan_dapat_bantuan / total_responden * 100, 2), "%) mengetahui adanya perusahaan listrik dan juga menerima bantuan.\n",
      "Sebaliknya, ", tidak_tahu_tidak_dapat_bantuan, " responden (",
      round(tidak_tahu_tidak_dapat_bantuan / total_responden * 100, 2), "%) tidak mengetahui adanya perusahaan listrik dan juga tidak menerima bantuan."
    )
  })
  
  output$pie_chartJenisBantuanPerusahaan <- renderPlot({
    bantuan_perusahaan <- data_pendanaan %>%
      count(dalam.bentuk) %>%
      mutate(dalam.bentuk=recode(dalam.bentuk,
                                 `1`="Sumbangan tidak mengikat (hibah)",
                                 `2`="Sumbangan bergulir",
                                 `3`="Pelatihan",
                                 `4`="Bimbingan teknis usaha",
                                 `5`="Lainnya",
                                 .default ="- \n Tidak Mengisi",
                                 .missing = "- \n Tidak Mengisi")
             ,percentage = n / sum(n) * 100,
             label = paste0(dalam.bentuk, "\n", round(percentage, 1), "%"))
    
    ggplot(bantuan_perusahaan, aes(x = "", y = n, fill = dalam.bentuk)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
      labs(fill = "Bentuk Bantuan Dari perusahaan", title = "Bentuk Bantuan Dari perusahaan")
  })
  
  output$analysisTextBantuanPerusahaan <- renderText({
    analysisTextBantuanPerusahaan <- "Masyarakat Desa Mekarsari mengharapkan bantuan dari perusahaan berupa bantuan sesuai dengan kebutuhan yang diajukan dalam proposal, santunan untuk anak yatim, serta pelatihan dan penyuluhan untuk meningkatkan wawasan dan sumber daya manusia (SDM)."
    analysisTextBantuanPerusahaan
  })
  
  
  output$barChartTahuBantuanDesa <- renderPlot({
    modal_combined_data <- data_pendanaan %>%
      rename(BantuanDesa  = Apakah.pemerintah.desa.memberikan.bantuan.buat.masyarakat, 
             Bantuan = Jika.ya.dalam.bentuk)
    
    # Recode data
    modal_combined_data <- modal_combined_data %>%
      mutate(BantuanDesa = recode(BantuanDesa,
                                  `2`="Tidak Tahu", 
                                  `1`="Tahu"), 
             Bantuan = recode(Bantuan,
                              `1`="Sumbangan tidak mengikat (hibah)",
                              `2`="Sumbangan bergulir",
                              `3`="Pelatihan",
                              `4`="Bimbingan teknis usaha",
                              `5`="Lainnya",
                              .default = "-",
                              .missing = "-"))
    
    # Filter out rows with NA after recoding
    modal_combined_data <- modal_combined_data %>%
      mutate(Bantuan = ifelse(is.na(Bantuan) & BantuanDesa == "Tidak Tahu", "-", Bantuan))
    
    # Convert to factors
    modal_combined_data$BantuanDesa <- factor(modal_combined_data$BantuanDesa, levels = c("Tahu", "Tidak Tahu"))
    modal_combined_data$Bantuan <- as.factor(modal_combined_data$Bantuan)
    
    # Create data frame for plotting
    modal_combined_count <- modal_combined_data %>%
      group_by(BantuanDesa, Bantuan) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Define number of bars and colors
    num_bars <- nrow(modal_combined_count)
    colors <- RColorBrewer::brewer.pal(min(num_bars * 10, 4), "Dark2")
    
    # Plot the data
    ggplot(modal_combined_count, aes(x = BantuanDesa, y = count, fill = Bantuan)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      theme_minimal() +
      scale_fill_manual(values = colors) +
      labs(title = "Distribusi Bentuk Bantuan Berdasarkan Pengetahuan Adanya Dana Desa",
           x = "Pengetahuan Adanya Dana Desa",
           y = "Jumlah Responden",
           fill = "Bentuk Bantuan") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_y_continuous(breaks = seq(0, max(modal_combined_count$count, na.rm = TRUE), by = 1)) # Adding manual breaks for y-axis
  })
  
  output$analysisTextBantuanDesa <- renderText({
    analysisTextBantuanDesa <- "Masyarakat Desa Mekarsari mengharapkan bantuan dari Pemerintahan Desa berupa BLT setiap bulan, sembako, koperasi untuk modal usaha, pelatihan untuk menambah keterampilan, pembangunan infrastruktur, serta subsidi harga pupuk dan bibit."
    analysisTextBantuanDesa
  })
}

render_server_pendanaan()