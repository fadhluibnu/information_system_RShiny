data_profil_desa <- read_csv("data/profil_desa.csv")
print(data_profil_desa)

output$selamatDatang <- renderText({
  selamatDatang <- paste("Di ", data_profil_desa$nama, ", ", data_profil_desa$alamat)
  selamatDatang
})

output$sejarahDesa <- renderText({
  sejarahDesa <- paste(data_profil_desa$sejarah)
  sejarahDesa
})

output$batasTimur <- renderText({
  batasTimur <- paste( data_profil_desa$timur)
  batasTimur
})

output$batasSelatan <- renderText({
  batasSelatan <- paste( data_profil_desa$selatan)
  batasSelatan
})

output$batasUtara <- renderText({
  batasUtara <- paste( data_profil_desa$utara)
  batasUtara
})

output$batasBarat <- renderText({
  batasBarat <- paste(data_profil_desa$barat)
  batasBarat
})

output$lakiPerempuan <- renderPlot({
  df <- data.frame(
    Gender = c("Laki-laki", "Perempuan"),
    Count = c(data_profil_desa$laki_laki, data_profil_desa$perempuan)
  )
  
  ggplot(df, aes(x = Gender, y = Count, fill = Gender)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Laki-laki" = "blue", "Perempuan" = "pink")) +
    theme_minimal() +
    labs(title = "Jumlah Penduduk Laki-laki dan Perempuan", x = "Gender", y = "Jumlah")
})

output$kepalaKeluarga <- renderText({
  kepalaKeluarga <- paste(data_profil_desa$kepala_keluarga, "Kepala Keluarga")
  kepalaKeluarga
})

output$kepalaDesa <- renderText({
  kepalaDesa <- paste(data_profil_desa$kepala_desa)
  kepalaDesa
})