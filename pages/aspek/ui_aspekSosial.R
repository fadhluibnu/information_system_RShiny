tabItemAspekSosial <- tabItem(
  tabName = "aspekSosial",
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
          h4(tags$b("Aspek Sosial")),
          selectInput(
            'Apakah.Anda.tinggal.di.desa.ini.sejak.lahir', 
            'Apakah Anda tinggal di desa ini sejak lahir?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          textInput(
            'Jika.tidak.sebutkan.asal.tempat', 'Jika tidak, sebutkan asal tempat',
            
          ),
          selectInput(
            'Apakah.Anda.aktif.dalam.kegiatan.sosial.kemasyarakatan.di.desa.ini', 'Apakah Anda aktif dalam kegiatan sosial kemasyarakatan di desa ini?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            'Seberapa.sering.Anda.berpartisipasi.dalam.pertemuan.warga.atau.komunitas.di.desa', 'Seberapa sering Anda berpartisipasi dalam pertemuan warga atau komunitas di desa?',
            
            c(
              'Sering'='4',
              'Kadang-kadang'='3',
              'Jarang'='2',
              'Tidak Pernah'='1'
            )
          ),
          selectInput(
            'Apakah.anda.merasa.adanya.dukungan.sosial.yang.memadai.di.desa.ini', 'Apakah anda merasa adanya dukungan sosial yang memadai di desa ini?',
            
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
        ),
        actionButton("updateAspekSosial", "Update"),
        actionButton("cancelAspekSosial", "Cancel")
      ),
    )
  ),
  
  fluidRow(
    box(title = "Aspek Sosial", width = 12, 
        DTOutput("data_table_apekSosial"))
  ),
)