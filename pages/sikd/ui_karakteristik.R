tabItemKarakteristik <- tabItem(tabName = "karakteristik",
                                fluidRow(
                                  conditionalPanel(
                                    condition = "input.jenis_kelamin == true",
                                    box(title = "Jenis Kelamin", width = 6, 
                                        plotOutput("pie_chart_jenis_kelamin"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.pendidikan == true",
                                    box(title = "Pendidikan", width = 6,
                                        plotOutput("pie_chart_pendidikan"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.show_age_chart == true",
                                    box(id = "age_bar_chart_box", title = "Distribusi Usia", width = 12, 
                                        plotOutput("age_bar_chart"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.pekerjaan_utama == true",
                                    box(title = "Pekerjaan Utama", width = 12, 
                                        plotOutput("bar_chart_pekerjaan_utama"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.pekerjaan_sampingan == true",
                                    box(title = "Pekerjaan Sampingan", width = 6, 
                                        plotOutput("bar_chart_pekerjaan_sampingan"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.memulai_usaha == true",
                                    box(title = "Memulai Usaha", width = 6, 
                                        plotOutput("bar_chart_memulai_usaha"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.jenis_usaha == true",
                                    box(title = "Jenis Usaha", width = 6, 
                                        plotOutput("bar_chart_jenis_usaha"))
                                  ),
                                  conditionalPanel(
                                    condition = "input.skala_usaha == true",
                                    box(title = "Skala Usaha", width = 6, 
                                        plotOutput("pie_chart_skala_usaha"))
                                  )
                                ),
                                fluidRow(
                                  box(title = "Karakteristik Desa", width = 12, 
                                      DTOutput("data_table"))
                                )
)