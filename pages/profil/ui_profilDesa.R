tabItemProfilDesa <- tabItem(
  tabName = "dashboard",
  fluidRow(
    box(
      solidHeader = TRUE,
      status = "primary",
      width= 12,
      h2(tags$b("Selamat Datang")),
      h2(tags$b(textOutput("selamatDatang"))),
    ),
  ),
  fluidRow(
    box(
      solidHeader = TRUE,
      status = "warning",
      h3(tags$b("Sejarah Desa")),
      h4(tags$p(textOutput("sejarahDesa")))
    ),
    box(
      solidHeader = TRUE,
      status = "danger",
      h3(tags$b("Batas Wilayah Desa")),
      h4(tags$b("Batas Timur : ") , tags$p(textOutput("batasTimur"))),
      h4(tags$b("Batas Barat : ") , tags$p(textOutput("batasBarat"))),
      h4(tags$b("Batas Selatan : ") , tags$p(textOutput("batasSelatan"))),
      h4(tags$b("Batas Utara : ") , tags$p(textOutput("batasUtara"))),
    )
  ),
  fluidRow(
    box(
      title = h3(tags$b("Persebaran Laki-laki dan Perempuan")), 
      solidHeader = TRUE, 
      width = 12,
      fluidRow(
        box(
          solidHeader = TRUE, 
          width = 8,
          plotOutput("lakiPerempuan")),
        box(
          solidHeader = TRUE, 
          width = 4,
          h4(tags$b("kepala Desa Saat Ini : ") , textOutput("kepalaDesa"))),
        box(
          solidHeader = TRUE, 
          width = 4,
          h4(tags$b("Total Kepala Keluarga : ") , textOutput("kepalaKeluarga")))
      )
    )
  )
)