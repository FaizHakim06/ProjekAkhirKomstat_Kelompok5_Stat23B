# Install package jika belum:
# install.packages(c("shiny", "shinydashboard", "ggplot2", "plotly", "DT", "dplyr"))
# coba editt
# coba

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "STATIFY"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Tentang Uji Statistik", tabName = "info", icon = icon("book")),
      menuItem("Upload & Pengaturan", tabName = "upload", icon = icon("upload")),
      menuItem("Visualisasi", tabName = "visual", icon = icon("chart-bar")),
      menuItem("Hasil Uji", tabName = "hasil", icon = icon("check-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
    /* === LATAR BELAKANG === */
    .content-wrapper, .right-side {
      background-image: url('bg_Rshiny7.jpg');
      background-size: cover;
      background-repeat: no-repeat;
      background-attachment: fixed;
      background-position: center;
    }

    /* === FONT PUTIH === */
    body, label, h1, h2, h3, h4, h5, h6, .box, .info-box, .small-box, .content-wrapper, .right-side {
      color: white !important;
      text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.6); 
      font-weight: bold;
      font-family: 'Segoe UI', sans-serif
    }

    /* === TABEL DATA TRANSPARAN === */
    .dataTables_wrapper {
      background-color: rgba(0, 0, 0, 0.3) !important;
      padding: 15px;
      border-radius: 10px;
    }

    table.dataTable {
      background-color: rgba(255, 255, 255, 0.1) !important;
      color: white !important;
    }

    th {
      background-color: rgba(255, 255, 255, 0.2) !important;
      color: white !important;
    }

    /* === TAB AKTIF === */
    .nav-tabs-custom > .nav-tabs > li.active > a {
      background-color: rgba(255, 255, 255, 0.2);
      color: white !important;
      border-color: transparent;
    }

    /* === AREA PLOT === */
    .plotly {
      background-color: rgba(0, 0, 0, 0.15);
      padding: 10px;
      border-radius: 12px;
    }

    /* === HEADER DAN SIDEBAR GELAP === */
    .skin-blue .main-header .logo {
      background-color: #212121;
      color: white;
      font-weight: bold;
    }

    .skin-blue .main-sidebar {
      background-color: #121212;
    }

    .skin-blue .main-header .navbar {
      background-color: #1c1c1c;
    }
  "))
    ),
    tags$div(
      id = "splash-screen",
      style = "position: fixed; top: 0; left: 0; width: 100%; height: 100%;
           background-color: #121212; color: white; z-index: 9999;
           display: flex; justify-content: center; align-items: center;
           flex-direction: column; font-family: 'Segoe UI', sans-serif;",
      tags$style(HTML("
    @keyframes fadeOut {
      0%   { opacity: 1; }
      100% { opacity: 0; visibility: hidden; }
    }
  ")),
      tags$h1("📊 Selamat Datang di STATIFY", style = "font-size: 36px; text-align: center;"),
      tags$h4("Aplikasi Analisis Uji Nonparametrik", style = "font-weight: normal;"),
      tags$script(HTML("
    setTimeout(function() {
      var splash = document.getElementById('splash-screen');
      splash.style.animation = 'fadeOut 1s forwards';
    }, 2500); // muncul selama 2.5 detik
  "))
    ),
    
    tabItems(
      tabItem(tabName = "beranda",
              h1("Selamat Datang di STATIFY 📊"),
              p("Aplikasi interaktif untuk uji statistik nonparametrik: Median, Fisher, dan Chi-Square."),
              br(),
              fluidRow(
                box(title = "Apa itu STATIFY?", status = "primary", solidHeader = TRUE, width = 6,
                    p("STATIFY adalah dashboard interaktif berbasis R Shiny yang dikembangkan untuk kebutuhan analisis statistik nonparametrik. Anda dapat mengunggah data, memilih jenis uji, memvisualisasikan hasil, dan mendapatkan keputusan statistik secara otomatis."),
                    p("Cocok untuk mahasiswa, peneliti, dan praktisi.")
                ),
                box(title = "Panduan Penggunaan", status = "success", solidHeader = TRUE, width = 6,
                    tags$ul(
                      tags$li("📁 Unggah data CSV"),
                      tags$li("🧪 Pilih jenis uji yang diinginkan"),
                      tags$li("📊 Tampilkan visualisasi data"),
                      tags$li("✅ Lihat hasil dan keputusan uji")
                    )
                )
              )
      ),
      tabItem(tabName = "info",
              h2("Penjelasan Uji Statistik"),
              h3("1. Uji Median (Wilcoxon Signed-Rank)"),
              p("Uji ini digunakan untuk mengetahui apakah nilai tengah (median) dari data berbeda dari nilai tertentu."),
              tags$ul(
                tags$li("Hipotesis nol (H₀): median = nilai tertentu"),
                tags$li("Cocok untuk data numerik satu variabel")
              ),
              h5("Contoh Data:"),
              verbatimTextOutput("contoh_median"),
              
              h3("2. Uji Fisher (Fisher’s Exact Test)"),
              p("Uji ini digunakan untuk melihat hubungan antara dua variabel kategorik pada tabel kontingensi kecil."),
              tags$ul(
                tags$li("Hipotesis nol (H₀): tidak ada hubungan antara kategori"),
                tags$li("Cocok untuk tabel kecil, sel dengan frekuensi < 5")
              ),
              h5("Contoh Data:"),
              verbatimTextOutput("contoh_fisher"),
              
              h3("3. Uji Chi-Square"),
              p("Uji ini digunakan untuk melihat hubungan antara dua variabel kategorik pada data besar."),
              tags$ul(
                tags$li("Hipotesis nol (H₀): tidak ada hubungan"),
                tags$li("Frekuensi sel sebaiknya ≥ 5")
              ),
              h5("Contoh Data:"),
              verbatimTextOutput("contoh_chisq")
      ),
      
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Unggah Data CSV", width = 6, status = "primary", solidHeader = TRUE,
                    fileInput("datafile", "Pilih File (.csv)", accept = ".csv"),
                    radioButtons("uji", "Pilih Jenis Uji:",
                                 choices = c("Uji Median" = "median", 
                                             "Uji Fisher" = "fisher",
                                             "Uji Chi-Square" = "chisq")),
                    conditionalPanel(
                      condition = "input.uji == 'median'",
                      selectInput("var_median", "Pilih Variabel Numerik:", choices = NULL),
                      numericInput("alpha1", "Signifikansi α", value = 0.05)
                    ),
                    conditionalPanel(
                      condition = "input.uji == 'fisher' || input.uji == 'chisq'",
                      selectInput("var_kat1", "Variabel Kategori 1:", choices = NULL),
                      selectInput("var_kat2", "Variabel Kategori 2:", choices = NULL),
                      numericInput("alpha2", "Signifikansi α", value = 0.05)
                    )
                ),
                box(title = "Preview Data", width = 6, DTOutput("tabel_data"))
              )
      ),
      
      tabItem(tabName = "visual",
              h3("Visualisasi Data"),
              plotlyOutput("plot")
      ),
      
      tabItem(tabName = "hasil",
              h3("Hasil Uji Statistik"),
              verbatimTextOutput("output_uji"),
              br(),
              h4("Keputusan:"),
              textOutput("keputusan")
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Contoh untuk halaman informasi
  output$contoh_median <- renderPrint({
    data.frame(Nilai = c(83, 65, 97, 73, 80))
  })
  
  output$contoh_fisher <- renderPrint({
    data.frame(Obat = c("A", "A", "B", "B"),
               Status = c("Ya", "Tidak", "Ya", "Tidak"))
  })
  
  output$contoh_chisq <- renderPrint({
    data.frame(Pekerjaan = c("Guru", "Guru", "Dokter", "Insinyur"),
               Kopi = c("Hitam", "Latte", "Cappuccino", "Hitam"))
  })
  
  # Data upload dan dinamis input
  dataInput <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })
  
  observe({
    df <- dataInput()
    updateSelectInput(session, "var_median", choices = names(df))
    updateSelectInput(session, "var_kat1", choices = names(df))
    updateSelectInput(session, "var_kat2", choices = names(df))
  })
  
  output$tabel_data <- renderDT({
    req(dataInput())
    datatable(dataInput(), options = list(pageLength = 5))
  })
  
  output$plot <- renderPlotly({
    df <- dataInput()
    if (input$uji == "median" && input$var_median != "") {
      p <- ggplot(df, aes_string(y = input$var_median)) +
        geom_boxplot(fill = "lightblue") +
        labs(title = "Boxplot Uji Median", y = input$var_median)
      ggplotly(p)
    } else if (input$uji %in% c("fisher", "chisq") && input$var_kat1 != "" && input$var_kat2 != "") {
      tab <- table(df[[input$var_kat1]], df[[input$var_kat2]])
      df_tab <- as.data.frame(tab)
      names(df_tab) <- c("X", "Y", "Freq")
      p <- ggplot(df_tab, aes(x = X, y = Freq, fill = Y)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "Barplot Tabel Kontingensi", y = "Frekuensi")
      ggplotly(p)
    }
  })
  
  output$output_uji <- renderPrint({
    df <- dataInput()
    if (input$uji == "median" && input$var_median != "") {
      wilcox.test(df[[input$var_median]], mu = 0)
    } else if (input$uji == "fisher" && input$var_kat1 != "" && input$var_kat2 != "") {
      tab <- table(df[[input$var_kat1]], df[[input$var_kat2]])
      fisher.test(tab)
    } else if (input$uji == "chisq" && input$var_kat1 != "" && input$var_kat2 != "") {
      tab <- table(df[[input$var_kat1]], df[[input$var_kat2]])
      chisq.test(tab)
    }
  })
  
  output$keputusan <- renderText({
    df <- dataInput()
    if (input$uji == "median" && input$var_median != "") {
      hasil <- wilcox.test(df[[input$var_median]], mu = 0)
      if (hasil$p.value < input$alpha1) {
        paste("Tolak H₀. p-value =", round(hasil$p.value, 4))
      } else {
        paste("Gagal tolak H₀. p-value =", round(hasil$p.value, 4))
      }
    } else if (input$uji == "fisher" && input$var_kat1 != "" && input$var_kat2 != "") {
      tab <- table(df[[input$var_kat1]], df[[input$var_kat2]])
      hasil <- fisher.test(tab)
      if (hasil$p.value < input$alpha2) {
        paste("Tolak H₀. p-value =", round(hasil$p.value, 4))
      } else {
        paste("Gagal tolak H₀. p-value =", round(hasil$p.value, 4))
      }
    } else if (input$uji == "chisq" && input$var_kat1 != "" && input$var_kat2 != "") {
      tab <- table(df[[input$var_kat1]], df[[input$var_kat2]])
      hasil <- chisq.test(tab)
      if (hasil$p.value < input$alpha2) {
        paste("Tolak H₀. p-value =", round(hasil$p.value, 4))
      } else {
        paste("Gagal tolak H₀. p-value =", round(hasil$p.value, 4))
      }
    }
  })
}

shinyApp(ui, server)
