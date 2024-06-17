tabItemAspekSpatial <- tabItem(
  tabName = 'aspekSpatial',
  
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
          h4(tags$b("Aspek Temporal")),
          numericInput(
            'Seberapa.jauh.rumah.Anda.dari.pusat.desa.atau.kota.terdekat.Kec.Sagaranten.KM', 'Seberapa jauh rumah Anda dari pusat desa atau kota terdekat? (Kec. Sagaranten) .... KM',
            value = 0
          ),
          selectInput(
            'Bagaimana.Anda.biasanya.mengakses.fasilitas.umum.seperti.pasar.sekolah.dan.rumah.sakit.di.desa.ini', 'Bagaimana Anda biasanya mengakses fasilitas umum seperti pasar, sekolah, dan rumah sakit di desa ini?',
            c(
              'Jalan Kaki'='1',
              'Sepeda'='2',
              'Sepeda Motor'='3',
              'Mobil'='4',
              'Angkutan Umum'='5',
              'Lainnya'='6'
            )
          ),
          selectInput(
            'Apakah.transportasi.umum.cukup.memadai.di.desa.ini', 'Apakah transportasi umum cukup memadai di desa ini?',
            c(
              'Sangat memadai'='4',
              'Cukup memadai'='3',
              'Tidak cukup'='2',
              'Tidak ada'='1'
            )
          ),
          selectInput(
            'Bagaimana.kondisi.infrastruktur.jalan.di.desa.ini', 'Bagaimana kondisi infrastruktur jalan di desa ini? ',
            c(
              'Baik'='4',
              'Cukup baik'='3',
              'Tidak cukup baik'='2',
              'Tidak ada'='1'
            )
          ),
          selectInput(
            'Apakah.terdapat.masalah.terkait.sanitasi.di.desa.ini.seperti.akses.ke.fasilitas.toilet.yang.memadai.atau.masalah.limbah', 'Apakah terdapat masalah terkait sanitasi di desa ini, seperti akses ke fasilitas toilet yang memadai atau masalah limbah?',
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            'Bagaimana.pasokan.air.bersih.di.desa.ini', 'Bagaimana pasokan air bersih di desa ini? ',
            c(
              'Baik'='4',
              'Cukup baik'='3',
              'Tidak cukup baik'='2',
              'Tidak ada'='1'
            )
          ),
          selectInput(
            'Apakah.Anda.menghadapi.masalah.terkait.air.bersih', 'Apakah Anda menghadapi masalah terkait air bersih?',
            c(
              'Sering'='4',
              'Kadang-kadang'='3',
              'Jarang'='2',
              'Tidak Pernah'='1'
            )
          ),
          selectInput(
            'Apakah.terdapat.masalah.terkait.polusi.di.desa.ini.seperti.polusi.udara.atau.pencemaran.lingkungan.lainnya', 'Apakah terdapat masalah terkait polusi di desa ini, seperti polusi udara atau pencemaran lingkungan lainnya?',
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          ),
          selectInput(
            'Apakah.Anda.merasa.bahwa.infrastruktur.dan.fasilitas.umum.di.desa.ini.memadai.untuk.memenuhi.kebutuhan.sehari.hari.penduduk', 'Apakah Anda merasa bahwa infrastruktur dan fasilitas umum di desa ini memadai untuk memenuhi kebutuhan sehari-hari penduduk? ',
            c(
              "Ya"="1",
              "Tidak"="2",
              "Belum tahu"="3"
            )
          )
        ),
        actionButton("updateAspekSpatial", "Update"),
        actionButton("cancelAspekSpatial", "Cancel")
      )
    )
  ),
  fluidRow(
    box(title = "Aspek Spatial", width = 12, 
        DTOutput("data_table_apekSpatial"))
  ),
)