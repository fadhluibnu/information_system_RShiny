tabItemPendanaan <- tabItem(tabName = "pendanaan",
                            fluidRow(
                              
                              conditionalPanel(
                                condition = "input.PengembanganUsaha == true",
                                box(title = "Pendanaan Pengembangan Usaha", status = "primary", solidHeader = TRUE, 
                                    width = 4,
                                    h5("Apakah Bapak/Ibu tahu mengenai pendanaan untuk mengembangkan usaha?"),
                                    plotOutput("pieChartPengembanganUsaha"),
                                    box(title = "Analisis Pengetahuan Pengembangan Usaha", 
                                        status = "primary", 
                                        solidHeader = TRUE, 
                                        width = 12, 
                                        textOutput("analysisPengembanganUsaha"))),
                              ),
                              
                              conditionalPanel(
                                condition = "input.DanaDesa == true",
                                box(title = "Pengetahuan Dana Desa", status = "warning", solidHeader = TRUE, 
                                    width = 4, 
                                    h5("Apakah Bapak/Ibu tahu yang dimaksud dengan dana desa?"),
                                    plotOutput("pieChartDanaDesa"),
                                    box(title = "Analisis Pengetahuan Dana Desa", 
                                        status = "warning", 
                                        solidHeader = TRUE, 
                                        width = 12, 
                                        textOutput("analysisPengetahuanDanaDesa"))),
                              
                              ),
                              
                              conditionalPanel(
                                condition = "input.CRS == true",
                                box(title = "Dana Coorporate Social Responsibility", status = "danger", solidHeader = TRUE, 
                                    width = 4, 
                                    h5("Apakah Bapak/Ibu tahu yang dimaksud dengan dana CSR (Coorporate Social Responsibility)?"),
                                    plotOutput("pieChartCRS"),
                                    box(title = "Analisis Pengetahuan Dana Coorporate Social Responsibility", 
                                        status = "danger", 
                                        solidHeader = TRUE, 
                                        width = 12, 
                                        textOutput("analysisPengetahuanCRS"))),
                              )
                            ),
                            
                            conditionalPanel(
                              condition = "input.SubmerModal == true || input.ModalAwal == true",
                              box(title = "Modal Usaha",
                                width = 12,
                                fluidRow(
                                  width = 12,
                                  
                                  conditionalPanel(
                                    condition = "input.SubmerModal == true",
                                    box(title = "Sumber Modal Usaha", status = "primary", 
                                        solidHeader = TRUE, 
                                        width = 5, 
                                        DTOutput("sumberModalUsahaTable")),
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.ModalAwal == true",
                                    box(title = "Diagram Batang Modal Awal", 
                                        status = "primary", 
                                        solidHeader = TRUE, 
                                        width = 7, 
                                        plotOutput("barChartModalAwal")),
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.SubmerModal == true && input.ModalAwal == true",
                                    box(title = "Analisis Modal Awal dan Sumber Modal", 
                                        status = "primary", 
                                        solidHeader = TRUE, 
                                        width = 12, 
                                        plotOutput("barChartModalCombined")),
                                  )
                                )
                            )
                            ),
                            
                            conditionalPanel(
                              condition = "input.PerusahaanListrik == true || input.PerusahaanListrik2 == true || input.PerusahaanListrik3 == true",
                              box(title = "Perusahan Listrik",
                                  width = 12,
                                  fluidRow(
                                    box(title = "Keberadaan Perusahan Listrik", 
                                        status = "primary", solidHeader = TRUE, 
                                        width = 12,
                                        h5(tags$b("Apakah Bapak/Ibu mengetahui adanya perusahaan listrik?")),
                                        plotOutput("barChartTahuBantuan"),
                                        box(
                                          title = "Hasil Analisis",
                                          width = 12,
                                          textOutput("analysisText")
                                        ),
                                        h5(tags$b("Jenis Bantuan")),
                                        plotOutput("pie_chartJenisBantuanPerusahaan"),
                                        box(
                                          title = "Hasil Analisis",
                                          width = 12,
                                          textOutput("analysisTextBantuanPerusahaan")
                                        ),
                                      ),
                                    ),
                              ),
                            ),
                            
                            conditionalPanel(
                              condition = "input.BantuanDesa == true || input.BantuanDesa2 == true",
                              box(title = "Bantuan Pemerintah Desa",
                                  width = 12,
                                  fluidRow(
                                    box(title = "Keberadaan Perusahan Listrik", 
                                        status = "primary", solidHeader = TRUE, 
                                        width = 12,
                                        h5(tags$b("Apakah pemerintah desa memberikan bantuan buat masyarakat?")),
                                        plotOutput("barChartTahuBantuanDesa"),
                                        box(
                                          title = "Hasil Analisis",
                                          width = 12,
                                          textOutput("analysisTextBantuanDesa")
                                        ),
                                    ),
                                  ),
                              ),
                            ),
                            tags$script(
                              HTML(
                                "
      Shiny.addCustomMessageHandler('form_update_false', function(message) {
        Shiny.setInputValue('form_update', '0');
      });
      Shiny.addCustomMessageHandler('selected_id_handler', function(message) {
        Shiny.setInputValue('selected_id', message);
      });
    "
                              )
                            ), 
                            conditionalPanel(
                              condition = "input.form_update == '1'", 
                              fluidRow(
                                box(
                                  title = h3(tags$b("Update Data")),
                                  status = "primary",
                                  width = 12,
                                  solidHeader = TRUE,
                                  box(
                                    status = "success",
                                    width = 12,
                                    h4(tags$b("Pendanaan")),
                                    selectInput("Apakah.BapakIbu.tahu.mengenai.pendanaan.untuk.mengembangkan.usaha", "Apakah Bapak/Ibu tahu mengenai pendanaan untuk mengembangkan usaha? :",
                                                c(
                                                  "Tahu"="1",
                                                  "Tidak Tahu"="2"
                                                ),
                                    ),
                                    selectInput("Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.desa", "Apakah Bapak/Ibu tahu yang dimaksud dengan dana desa? :",
                                                c(
                                                  "Tahu"="1",
                                                  "Tidak Tahu"="2"
                                                ),
                                    ),
                                    selectInput("Apakah.Bapak.Ibu.tahu.yang.dimaksud.dengan.dana.CSR.Coorporate.Social.Responsibility", "Apakah Bapak/Ibu tahu yang dimaksud dengan dana CSR (Coorporate Social Responsibility)? :",
                                                c(
                                                  "Tahu"="1",
                                                  "Tidak Tahu"="2"
                                                ),
                                    ),
                                    selectInput("Modal.Usaha.Bapak.Ibu.diperoleh.dari", "Modal Usaha Bapak/Ibu diperoleh dari :",
                                                c(
                                                  "Modal sendiri"="1",
                                                  "Pinjam saudara"="2",
                                                  "Pinjam bank"="3",
                                                  "Bantuan desa"="4",
                                                  "Bantuan dana CSR"="5",
                                                  "Lainnya"="6"
                                                ),
                                    ),
                                    textInput("Modal.awal", "Modal awal (Rp)"),
                                    selectInput("Apakah.Bapak.Ibu.mengetahui.adanya.perusahaan.listrik", "Apakah Bapak/Ibu mengetahui adanya perusahaan listrik? :",
                                                c(
                                                  "Tahu"="1",
                                                  "Tidak Tahu"="2"
                                                ),
                                    ),
                                    textInput("Jika.tahu.sudah.berapa.lama.perusahaan.beraktifitas.tahun", "Jika tahu, sudah berapa lama perusahaan beraktifitas? (tahun)"),
                                    selectInput("Apakah.perusahaan.memberikan.bantuan.buat.masyarakat.desa", "Apakah perusahaan memberikan bantuan buat masyarakat desa? :",
                                                c(
                                                  "Tahu"="1",
                                                  "Tidak Tahu"="2"
                                                ),
                                    ),
                                    selectInput("dalam.bentuk", "Jika ya, dalam bentuk :",
                                                c(
                                                  "Modal sendiri"="1",
                                                  "Pinjam saudara"="2",
                                                  "Pinjam bank"="3",
                                                  "Bantuan desa"="4",
                                                  "Bantuan dana CSR"="5",
                                                  "Lainnya"="6"
                                                ),
                                    ),
                                    selectInput("Apakah.pemerintah.desa.memberikan.bantuan.buat.masyarakat", "Apakah pemerintah desa memberikan bantuan buat masyarakat? :",
                                                c(
                                                  "Tahu"="1",
                                                  "Tidak Tahu"="2"
                                                ),
                                    ),
                                    selectInput("Jika.ya.dalam.bentuk", "Jika ya, dalam bentuk :",
                                                c(
                                                  "Modal sendiri"="1",
                                                  "Pinjam saudara"="2",
                                                  "Pinjam bank"="3",
                                                  "Bantuan desa"="4",
                                                  "Bantuan dana CSR"="5",
                                                  "Lainnya"="6"
                                                ),
                                    )
                                    
                                  ),
                                  actionButton("updatePendanaan", "Update"),
                                  actionButton("cancelPendanaan", "Cancel")
                                )
                              )
                            ),
                            fluidRow(
                              box(title = "Pendanaan Desa", width = 12, 
                                  DTOutput("data_table_pendanaan"))
                            )
                            
)