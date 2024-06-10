tabItemPeningkatanPerekonomian <- tabItem(
  tabName = "PeningkatanPerekonomian",
  conditionalPanel(
    condition = "input.DesaEkonomi == true || input.DesaPengahasilan == true || input.DesaModal == true",
    fluidRow(
      box(
        title = "Pengaruh Dana Desa",
        status="primary",
        width = 12,
        plotOutput("barPlotPeningkatanPerekonomianDanaDesa"),
        h4(tags$b("Hasil Analisis")),
        fluidRow(
          box(
            title = h4('Terbukanya usaha ekonomi rakyat, karena adanya dana desa'),
            width = 4,
            status="success",
            textOutput("analysisPeranDanaDesaUntukUsahaRakyat"),
          ),
          
          box(
            title = h4('Dana desa menambah penghasilan masyarakat'),
            width = 4,
            status="warning",
            textOutput("analysisPeranDanaDesaDalamMenambahPenghasilanRakyat"),
          ),
          
          box(
            title = h4('Adanya Dana desa membantu mengembangkan modal untuk rakyat'),
            width = 4,
            status="danger",
            textOutput("analysisPeranDanaDesaUntukModalRakyat"),
          )
        )
      )
    )
  ),
  conditionalPanel(
    condition = "input.CSREkonomi == true || input.CSRPenghasilan == true || input.CSRModal == true",
    fluidRow(
      box(
        title = "Pengaruh Dana CSR",
        status="primary",
        width = 12,
        plotOutput("barPlotPeningkatanPerekonomianCSR"),
        h4(tags$b("Hasil Analisis")),
        fluidRow(
          box(
            title = h4('Terbukanya usaha ekonomi rakyat, karena adanya dana CSR'),
            width = 4,
            status="success",
            textOutput("analysisPeranDanaCSRUntukUsahaRakyat"),
          ),
          
          box(
            title = h4('Dana CSR menambah penghasilan masyarakat'),
            width = 4,
            status="warning",
            textOutput("analysisPeranDanaCSRDalamMenambahPenghasilanRakyat"),
          ),
          
          box(
            title = h4('Adanya Dana CSR membantu mengembangkan modal untuk rakyat'),
            width = 4,
            status="danger",
            textOutput("analysisPeranDanaCSRUntukModalRakyat"),
          )
        )
      )
    )
  ),
  fluidRow(
    box(title = "Peningkatan Perekonomian Desa", width = 12, 
        DTOutput("data_table_PeningkatanPerekonomian"))
  ),
)