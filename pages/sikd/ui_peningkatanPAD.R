tabItemPeningkatanPAD <- tabItem(
  tabName = "PeningkatanPAD",
  h1("PAD"),
  conditionalPanel(
    condition = "input.DesaPembagunan == true || input.DesaInfrastruktur == true || input.DesaBumdes == true",
    fluidRow(
      box(
        title = "Pengaruh Dana Desa",
        status="primary",
        width = 12,
        plotOutput("barPlotPeningkatanPADDanaDesa"),
        h4(tags$b("Hasil Analisis")),
        fluidRow(
          box(
            title = h4('Dana desa digunakan untuk membentuk kegiatan pembangunan desa (termasuk membangun usaha)'),
            width = 4,
            status="success",
            textOutput("analysisDanaKegiatanPembangunanDesa"),
          ),
          
          box(
            title = h4('Dana desa digunakan untuk membangun Infrastruktur desa (misalnya : jalan)'),
            width = 4,
            status="warning",
            textOutput("analysisDanaPembagunanInfrastruktur"),
          ),
          
          box(
            title = h4('Dana desa membantu permodalan bagi kegiatan BUMDes'),
            width = 4,
            status="danger",
            textOutput("analysisDanaPemodalanBUMDes"),
          )
        )
      )
    ),
  ),
  conditionalPanel(
    condition = "input.CSRPembangunan == true || input.CSRInfrastruktur == true || input.CSRBumdes == true",
    fluidRow(
      box(
        title = "Pengaruh Dana CSR",
        status="primary",
        width = 12,
        plotOutput("barPlotPeningkatanPADCSR"),
        h4(tags$b("Hasil Analisis")),
        fluidRow(
          box(
            title = h4('Dana CSR digunakan untuk membentuk kegiatan pembangunan desa (termasuk membangun usaha)'),
            width = 4,
            status="success",
            textOutput("analysisDanaCSRKegiatanPembangunanDesa"),
          ),
          
          box(
            title = h4('Dana CSR digunakan untuk membangun Infrastruktur desa (misalnya : jalan)'),
            width = 4,
            status="warning",
            textOutput("analysisDanaCSRPembagunanInfra"),
          ),
          
          box(
            title = h4('Dana CSR membantu permodalan bagi kegiatan BUMDes'),
            width = 4,
            status="danger",
            textOutput("analysisDanaCSRPemodalanBUMDes"),
          )
        )
      )
    )
  ),
  fluidRow(
    box(title = "Peningkatan PAD", width = 12, 
        DTOutput("data_table_PeningkatanPAD"))
  ),
)