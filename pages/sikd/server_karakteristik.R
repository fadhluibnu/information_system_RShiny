render_server_karakteristik <- function() {
  data <- read_csv("data/Karakteristik.csv")
  data <- data %>%
    mutate(jenis.kelamin = ifelse(jenis.kelamin == 1, "Perempuan", "Laki-Laki"),
           Pendidikan = recode(Pendidikan,
                               `1` = "SD",
                               `2` = "SMP",
                               `3` = "SMA",
                               `4` = "Diploma",
                               `5` = "S1",
                               `6` = "S2",
                               `7` = "S3"),
           skala.usaha = recode(skala.usaha,
                                `1` = "Mikro/home industry",
                                `2` = "Kecil",
                                `3` = "Menengah",
                                `4` = "Besar"))
  
  output$data_table <- renderDT({
    data_display <- data %>%
      rename(
        `Jenis Kelamin` = jenis.kelamin, 
        `Skala Usaha` = skala.usaha) 
    
    datatable(data_display, options = list(
      headerCallback = JS(
        "function(thead, data, start, end, display){",
        "  if (!$('#karakteristik-checkbox').length) {",
        "  $(thead).closest('thead').prepend(`
      <tr id=\"karakteristik-checkbox\" style=\"position: relative;top: 10px;\"> 
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
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"toggle-age-chart\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pendidikan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pekerjaan-utama\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"pekerjaan-sampingan\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"memulai-usaha\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"jenis-usaha\">
        </th> 
        <th style=\"border: none; padding: 0px 10px 0px 14px;\">
          <input style=\"transform: scale(1.2);\" type=\"checkbox\" id=\"skala-usaha\">
        </th>
      </tr>`);",
        "  $('#toggle-age-chart').on('click', function(){",
        "    Shiny.setInputValue('show_age_chart', this.checked);",
        "  });",
        "  $('#jenis-kelamin').on('click', function(){",
        "    Shiny.setInputValue('jenis_kelamin', this.checked);",
        "  });",
        "  $('#pendidikan').on('click', function(){",
        "    Shiny.setInputValue('pendidikan', this.checked);",
        "  });",
        "  $('#pekerjaan-utama').on('click', function(){",
        "    Shiny.setInputValue('pekerjaan_utama', this.checked);",
        "  });",
        "  $('#pekerjaan-sampingan').on('click', function(){",
        "    Shiny.setInputValue('pekerjaan_sampingan', this.checked);",
        "  });",
        "  $('#memulai-usaha').on('click', function(){",
        "    Shiny.setInputValue('memulai_usaha', this.checked);",
        "  });",
        "  $('#jenis-usaha').on('click', function(){",
        "    Shiny.setInputValue('jenis_usaha', this.checked);",
        "  });",
        "  $('#skala-usaha').on('click', function(){",
        "    Shiny.setInputValue('skala_usaha', this.checked);",
        "  });",
        "  }",
        "}"
      ),
      columnDefs = list(list(orderable = FALSE, className = 'select-checkbox-katakteristik', targets = 0)),
      select = list(style = 'multi', selector = 'td:first-child')
    ), selection = 'none', escape = FALSE)
  })
  
  
  observeEvent(input$toggle_age_chart, {
    if (input$toggle_age_chart) {
      shinyjs::show("age_bar_chart_box")
    } else {
      shinyjs::hide("age_bar_chart_box")
    }
  })
  
  # Render pie chart
  output$pie_chart_jenis_kelamin <- renderPlot({
    gender_count <- data %>%
      count(jenis.kelamin) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(jenis.kelamin, "\n", round(percentage, 1), "%"))
    
    ggplot(gender_count, aes(x = "", y = n, fill = jenis.kelamin)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
      labs(fill = "Jenis Kelamin", title = "Distribusi Jenis Kelamin")
  })
  
  
  output$pie_chart_pendidikan <- renderPlot({
    gender_count <- data %>%
      count(Pendidikan) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(Pendidikan, "\n", round(percentage, 1), "%"))
    
    ggplot(gender_count, aes(x = "", y = n, fill = Pendidikan)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
      labs(fill = "Pendidikan", title = "Pendidikan")
  })
  
  output$pie_chart_skala_usaha <- renderPlot({
    gender_count <- data %>%
      count(skala.usaha) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(skala.usaha, "\n", round(percentage, 1), "%"))
    
    ggplot(gender_count, aes(x = "", y = n, fill = skala.usaha)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
      labs(fill = "Pendidikan", title = "Pendidikan")
  })
  
  # Render bar chart for age distribution
  output$age_bar_chart <- renderPlot({
    age_count <- data %>%
      count(Usia)
    
    # Define a custom color palette
    custom_colors <- c("red", "green", "blue", "orange", "purple", "yellow", "pink", "brown", "cyan", "magenta")
    # Shuffle the colors
    set.seed(42) # For reproducibility
    shuffled_colors <- sample(custom_colors, length(unique(age_count$Usia)), replace = TRUE)
    
    ggplot(age_count, aes(x = factor(Usia), y = n, fill = factor(Usia))) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = shuffled_colors) + # Use custom shuffled colors
      scale_y_continuous(breaks = seq(0, max(age_count$n), by = 1)) + # Ensure integer breaks
      theme_minimal() +
      labs(x = "Usia", y = "Jumlah", title = "Distribusi Usia") +
      theme(legend.position = "none") # Remove legend
  })
  
  output$bar_chart_pekerjaan_utama <- renderPlot({
    pekerjaan_count <- data %>%
      count(Pekerjaan.Utama) %>%
      arrange(desc(n))
    
    ggplot(pekerjaan_count, aes(x = reorder(Pekerjaan.Utama, -n), y = n, fill = Pekerjaan.Utama)) + 
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Pekerjaan Utama", y = "Jumlah", title = "Distribusi Pekerjaan Utama") +
      theme(legend.position = "none") # Remove legend
  })
  
  output$bar_chart_pekerjaan_sampingan <- renderPlot({
    pekerjaan_count <- data %>%
      count(Pekerjaan.Sampingan) %>%
      arrange(desc(n))
    
    ggplot(pekerjaan_count, aes(x = reorder(Pekerjaan.Sampingan, -n), y = n, fill = Pekerjaan.Sampingan)) + 
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Pekerjaan Sampingan", y = "Jumlah", title = "Distribusi Pekerjaan Sampingan") +
      theme(legend.position = "none") # Remove legend
  })
  
  output$bar_chart_memulai_usaha <- renderPlot({
    pekerjaan_count <- data %>%
      count(Memulai.Usaha) %>%
      arrange(desc(n))
    
    ggplot(pekerjaan_count, aes(x = reorder(Memulai.Usaha, -n), y = n, fill = Memulai.Usaha)) + 
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Memulai Usaha", y = "Jumlah", title = "Memulai Usaha") +
      theme(legend.position = "none") # Remove legend
  })
  
  output$bar_chart_jenis_usaha <- renderPlot({
    pekerjaan_count <- data %>%
      count(Jenis.Usaha) %>%
      arrange(desc(n))
    
    ggplot(pekerjaan_count, aes(x = reorder(Jenis.Usaha, -n), y = n, fill = Jenis.Usaha)) + 
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = "Jenis Usaha", y = "Jumlah", title = "Jenis Usaha") +
      theme(legend.position = "none") # Remove legend
  })
}

render_server_karakteristik()