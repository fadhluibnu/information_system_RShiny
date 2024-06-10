tabItemPeningkatanProgramWisata <- tabItem(
  tabName = "programwisata",
  conditionalPanel(
    condition = "input.DesaWisata == true || input.CRSWisata == true",
    fluidRow(
      box(
        title = "Pengaruh Dana Desa dan CSR Pada Wisata",
        status="primary",
        width = 12,
        plotOutput("barPlotProgramWisata"),
        h4(tags$b("Hasil Analisis")),
        fluidRow(
          box(
            title = h4('Terdapat tempat wisata yang dikelola dengan menggunakan dana desa'),
            width = 6,
            status="success",
            textOutput("analysisPeranDanaDesaPadaWisata"),
          ),
          
          box(
            title = h4('Terdapat tempat wisata yang dikelola dengan menggunakan dana CSR'),
            width = 6,
            status="warning",
            textOutput("analysisPeranDanaCSRPadaWisata"),
          ),
        )
      )
    )
  ),
  fluidRow(
    box(title = "Peningkatan Program Wisata", width = 12, 
        DTOutput("data_table_PeningkatanProgramWisata"))
  ),
)