library(shiny)
library(bs4Dash)
library(highcharter)
library(dplyr)

ui <- dashboardPage(
  header = dashboardHeader(
    title = dashboardBrand(
      title = "",
      color = NULL,
      href = "https://adminlte.io/themes/v3",
      image = "https://adminlte.io/themes/v3/dist/img/AdminLTELogo.png"
    )
  ),
  sidebar = dashboardSidebar(disable = T),
  body = dashboardBody(
    fluidRow(
      column(3, bs4Card(
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        title = "Komoditas",
        footer = "Komoditas Diteliti",
        uiOutput("card_1")
      )),
      column(3, bs4Card(
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        title = "Pasar",
        footer = "Pasar Dikunjungi",
        uiOutput("card_2")
      )),
      column(3, bs4Card(
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        title = "Harga Tertinggi",
        footer = uiOutput("card_max_price_item"),
        uiOutput("card_max_price_rp")
      )),
      column(3, bs4Card(
        width = 12,
        closable = FALSE,
        collapsible = FALSE,
        headerBorder = FALSE,
        title = "Harga Terendah",
        footer = uiOutput("card_min_price_item"),
        uiOutput("card_min_price_rp")
      ))
    ),
    fluidRow(
      column(
        width = 6,
        bs4Card(
          width = 12,
          closable = FALSE,
          collapsible = FALSE,
          headerBorder = FALSE,
          footer = uiOutput("plot_animated_slider"),
          highchartOutput("plot_animated")
        )
      ),
      column(
        width = 6,
        bs4Card(
          width = 12,
          closable = FALSE,
          collapsible = FALSE,
          headerBorder = FALSE,
          highchartOutput("map_1")
        ),
        bs4Card(
          width = 12,
          closable = FALSE,
          collapsible = FALSE,
          headerBorder = FALSE,
          title = textOutput("map_1_detail_header"),
          highchartOutput("map_1_detail")
        )
      )
    )
  ),
  controlbar = dashboardControlbar()
)

server <- function(input, output, session) {
  commodity_prices <- reactive({
    df <- read.csv2("commodity_prices.csv")
    df$tanggal <- as.Date(df$tanggal, "%d/%m/%Y")
    df
  })
  
  provinces <- reactive({
    read.csv2("provinces.csv")
  })
  
  markets <- reactive({
    read.csv2("markets.csv")
  })
  
  highchart_map_id <- reactive({
    read.csv("highchart_map_id.csv")
  })
  
  highchart_map_data <- reactive({
    df <- as.data.frame(list())
    withProgress(message = 'Sedang membaca data highchart', value = 0, {
      commodity_prices <- commodity_prices() %>% 
        group_by(Komoditas..Rp., province) %>% 
        slice_max(tanggal)
      
      provinces <- provinces()
      highchart_map_id <- highchart_map_id()
      
      df <- commodity_prices %>% 
        left_join(provinces, by = c("province" = "id")) %>% 
        left_join(highchart_map_id, by = c("label" = "provinsi"))
    })
    df
  })
  
  output$card_1 <- renderUI(h2(commodity_prices() %>% select(Komoditas..Rp.) %>% n_distinct()))
  output$card_2 <- renderUI(h2(markets() %>% nrow()))
  
  commodity_max_price <- reactive({
    commodity_prices() %>% slice_max(harga)
  })
  
  output$card_max_price_rp <- renderUI(h2(paste("Rp.", commodity_max_price() %>% select(harga) %>% unique())))
  output$card_max_price_item <- renderUI(paste(commodity_max_price() %>% select(Komoditas..Rp.) %>% unique(), collapase = ", "))
  
  commodity_min_price <- reactive({
    commodity_prices() %>% slice_min(harga)
  })
  
  output$card_min_price_rp <- renderUI(h2(paste("Rp.", commodity_min_price() %>% select(harga) %>% unique())))
  output$card_min_price_item <- renderUI(paste(commodity_min_price() %>% select(Komoditas..Rp.) %>% unique(), collapase = ", "))
  
  output$plot_animated_slider <- renderUI({
    sliderInput("tanggal", "Tanggal",
                min = min(commodity_prices()$tanggal), max = max(commodity_prices()$tanggal),
                value = min(commodity_prices()$tanggal), animate = TRUE)
  })
  
  output$plot_animated <- renderHighchart({
    req(input$tanggal)
    
    commodity_prices() %>%
      filter(tanggal <= input$tanggal) %>%
      hchart("line", hcaes(x = tanggal, y = harga, group = Komoditas..Rp.))
  })
  
  output$map_1 <- renderHighchart({
    data <- highchart_map_data() %>% group_by(province) %>% slice_max(harga)
    
    hcmap(
      "https://code.highcharts.com/mapdata/countries/id/id-all.js",
      download_map_data = TRUE,
      data = data,
      value = "harga",
      joinBy = c("fips", "highchart_prov_id"),
      name = "Harga komoditas",
      dataLabels = list(enabled = TRUE, format = "{point.label}"),
      borderColor = "#FAFAFA", borderWidth = 0.05,
      tooltip = list(valueDecimals = 0, valuePrefix = "Rp.", valueSuffix = ""),
      events = list(click = JS("
        function(e) {
          Shiny.onInputChange(\"map_1_on_click\", JSON.stringify({
            'Komoditas..Rp.': e.point['Komoditas..Rp.'],
            'highchart_prov_id': e.point['highchart_prov_id'],
            'province': e.point['province'],
            'regency': e.point['regency'],
          }))
        }
      "))
    )
  })
  
  map_1_on_click <- reactive({
    jsonlite::fromJSON(input$map_1_on_click)
  })
  
  output$map_1_detail_header <- renderText({
    req(input$map_1_on_click)
    
    data <- highchart_map_data() %>%
      filter(province == map_1_on_click()$province[1])
    
    paste("Harga komoditas Prov. ", paste(unique(data$label), collapse = " "))
  })
  
  output$map_1_detail <- renderHighchart({
    req(input$map_1_on_click)
    
    highchart_map_data() %>% 
      filter(province == map_1_on_click()$province[1]) %>% 
      hchart("line", hcaes(x = tanggal, y = harga, group = Komoditas..Rp.))
  })
}

shinyApp(ui, server)