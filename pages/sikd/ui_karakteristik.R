library(shiny)
library(shinydashboard)
library(readr)
library(DT)
library(ggplot2)
library(dplyr)
library(viridis) 


ui_karakteristik <- function(id) {
  ns <- NS(id)
    tabItem(tabName = "karakteristik",
            fluidRow(
              box(title = "Distribusi Usia", width = 6, 
                  plotOutput("age_bar_chart")),
              box(title = "Jenis Kelamin", width = 6, 
                  plotOutput("pie_chart"))
            ),
            fluidRow(
              box(title = "Karakteristik Desa", width = 12, 
                  DTOutput("data_table"))
            )
    )
}

server_karakteristik <- function(input, output, session, data) {
  ns <- session$ns
  
  print("Server Karakteristik called")
  print(str(data))
  
  output[[ns("data_table")]] <- renderDT({
    datatable(data)
  })
  
  # Render pie chart
  output[[ns("pie_chart")]] <- renderPlot({
    gender_count <- data %>%
      count(jenis_kelamin)
    
    ggplot(gender_count, aes(x = "", y = n, fill = jenis_kelamin)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(fill = "Jenis Kelamin", title = "Distribusi Jenis Kelamin")
  })
  
  # Render bar chart for age distribution
  output[[ns("age_bar_chart")]] <- renderPlot({
    age_count <- data %>%
      count(Usia)
    
    # Define a custom color palette
    custom_colors <- c("red", "green", "blue", "orange", "purple", "yellow", "pink", "brown", "cyan", "magenta")
    # Shuffle the colors
    set.seed(42) # For reproducibility
    shuffled_colors <- sample(custom_colors, length(unique(age_count$Usia)), replace = TRUE)
    
    ggplot(age_count, aes(x = factor(Usia), y = n, fill = factor(Usia))) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = shuffled_colors) + # Use custom shuffled colors
      scale_y_continuous(breaks = seq(0, max(age_count$n), by = 1)) + # Ensure integer breaks
      theme_minimal() +
      labs(x = "Usia", y = "Jumlah", title = "Distribusi Usia") +
      theme(legend.position = "none") # Remove legend
  })
}