tabItemAspekEkonomi <- tabItem(
  tabName = "aspekEkonomi",
  tags$script(HTML("
                  Shiny.addCustomMessageHandler('form_update_aspek', function(message) {
                    Shiny.setInputValue('form_update_aspek', '0');
                  });
                  Shiny.addCustomMessageHandler('selected_id_aspek', function(message) {
                    Shiny.setInputValue('selected_id_aspek', message);
                  });
                ")),
  conditionalPanel(
    condition = "input.form_update_aspek == '1'",
    fluidRow(
      box(
        title = h3(tags$b("Update Data")),
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        
        # Identitas
        box(
          status = "primary",
          width = 12,
          h4(tags$b("Aspek Ekonomi")),
          selectInput(
            "Apakah.Anda.atau.anggota.keluarga.Anda.memiliki.usaha.sendiri.di.desa.ini", 
            "Apakah Anda atau anggota keluarga Anda memiliki usaha sendiri di desa ini? ",
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            "Berapa.pendapatan.bulanan.Anda.atau.anggota.keluarga.Anda.secara.keseluruhan.Kisaran.jumlah", 
            "Berapa pendapatan bulanan Anda atau anggota keluarga Anda secara keseluruhan? (Kisaran jumlah)",
            c(
              "500.000 - 1.500.000"="1",
              "1.500.001 - 3.000.000"="2",
              "3.000.001 - 4.500.000"="3",
              "4.500.001 - ke atas"="4"
            )
          )
          ,
          selectInput(
            "Apakah.Anda.atau.anggota.keluarga.Anda.menerima.bantuan.sosial", 
            "Apakah Anda atau anggota keluarga Anda menerima bantuan sosial?",
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          textInput(
            "Jika.ya.sebutkan.aspek.ekonomi", 
            "Jika ya, sebutkan"
          ),
          selectInput(
            "Bagaimana.Anda.menilai.stabilitas.harga.pangan.saat.ini", 
            "Bagaimana Anda menilai stabilitas harga pangan saat ini?",
            c(
              "Sangat stabil"="4",
              "Stabil"="3",
              "Tidak stabil"="2",
              "Sangat tidak stabil"="1"
            )
          ),
          selectInput(
            "Apakah.Anda.memiliki.simpanan.atau.investasi.di.luar.penghasilan.bulanan.Anda", 
            "Apakah Anda memiliki simpanan atau investasi di luar penghasilan bulanan Anda?",
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            "Apakah.Anda.merasa.terdapat.peluang.ekonomi.yang.cukup.di.desa.ini", 
            "Apakah Anda merasa terdapat peluang ekonomi yang cukup di desa ini?",
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            "Bagaimana.Anda.menilai.aksesibilitas.dan.ketersediaan.lapangan.pekerjaan.di.desa.ini", 
            "Bagaimana Anda menilai aksesibilitas dan ketersediaan lapangan pekerjaan di desa ini?",
            c(
              "Sangat terbuka"="4",
              "Terbuka"="3",
              "Tidak terbuka"="2",
              "Sangat tidak terbuka"="1"
            )
          ),
          selectInput(
            "Apakah.Anda.merasa.terdapat.kesenjangan.ekonomi.di.antara.penduduk.di.desa.ini", 
            "Apakah Anda merasa terdapat kesenjangan ekonomi di antara penduduk di desa ini?",
            c(
              "Sangat terasa"="4",
              "Terasa"="3",
              "Tidak terasa"="2",
              "Sangat tidak terasa"="1"
            )
          ),
          actionButton("updateAspekEkonomi", "Update"),
          actionButton("cancelAspekEkonomi", "Cancel")
        )
      )
    )
  ),
  fluidRow(
    box(title = "Aspek Ekonomi", width = 12, 
        DTOutput("data_table_apekEkonomi"))
  ),
)