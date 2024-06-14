tabItemIdentitas <- tabItem(
  tabName = "identitas",
  
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
          h4(tags$b("Identitas Responden")),
          textInput(
            "Nama_Identitas", "Nama"
            
          ),
          selectInput(
            "Jenis.kelamin", "Jenis kelamin",
            c(
              "Perempuan"="1",
              "Laki-laki"="2"
            ),
          ),
          numericInput(
            "Usia.tahun", "Usia (tahun)",
            value = 0
            
          ),
          selectInput(
            "Status.perkawinan", "Status perkawinan",
            c(
              'Belum Menikah'='1',
              'Menikah'='2',
              'Duda/ Janda'='3',
              'Lainnya'='4'
            )
          ),
          selectInput(
            "Apakah.memiliki.anak", "Apakah memiliki anak?",
            c(
              'Ya'='1',
              'Tidak'='2',
              'Belum tahu'='3'
            )
          ),
          numericInput(
            "Jumlah.anak.orang", "Jumlah anak (orang)",
            value = 0
            
          ),
          selectInput(
            "Tingkat.pendidikan", "Tingkat pendidikan ",
            c(
              'Tidak lulus SD'='1',
              'SD'='2',
              'SMP'='3',
              'SMA'='4',
              'Diploma'='5',
              'Sarjana'='6',
              'Pasca Sarjana'='7',
              'Lainnya'='8'
            )
          ),
          selectInput(
            "Apakah.anda.bekerja.saat.ini", "Apakah anda bekerja saat ini?",
            c(
              'Ya'='1',
              'Tidak'='2',
              'Belum tahu'='3'
            )
          ),
          textInput(
            "Jika.bekerja.apa.pekerjaan.anda.saat.ini", "Jika bekerja, apa pekerjaan anda saat ini?"
            
          )
        ),
        actionButton("updateIdentitas", "Update"),
        actionButton("cancelIdentitas", "Cancel")
      )
    )
  ),
  fluidRow(
    box(title = "Identitas Responden", width = 12, 
        DTOutput("data_table_identitas"))
  ),
)