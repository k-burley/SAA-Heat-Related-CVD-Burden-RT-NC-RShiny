# Install and load required packages
# webr::install("shiny")
# webr::install("plotly")
# webr::install("ggtext")
# webr::install("readr")
# webr::install("bslib")

library(shiny)
library(plotly)
library(ggtext)
library(readr)
library(bslib)

df_plot_long <- read.csv('https://raw.githubusercontent.com/datadrivenenvirolab/smogstrippes_web/main/data/data_shiny_daily.csv')
df_plot_long$City = df_plot_long$city
df_plot_long$WHO <-   factor(df_plot_long$WHO,
                                 levels = c ("Below Recommended Value of 5µg/m^3", # 0-5
                                             "5-10µg/m^3 (1st WHO Interim Target)", 
                                             "10-15µg/m^3 (2nd WHO Interim Target)",
                                             "Within 3rd Interim Target of 25µg/m^3", 
                                             "Within 4th Interim Target of 35µg/m^3",
                                             "Exceeding All Recommended Guidelines"))

WHO_palette <- c("Below Recommended Value of 5µg/m^3"= "#00E400", 
                      "5-10µg/m^3 (1st WHO Interim Target)" = "#FB6A4A", 
                      "10-15µg/m^3 (2nd WHO Interim Target)"="#EF3B2C", 
                      "Within 3rd Interim Target of 25µg/m^3" = "#CB181D", 
                      "Within 4th Interim Target of 35µg/m^3" = "#A50F15", 
                      "Exceeding All Recommended Guidelines" = "#67000D")

df_plot_long$Concentration <- factor(df_plot_long$Concentration,
                                     levels = c("< 1 µg/m^3",
                                                "1 µg/m^3 - 28 µg/m^3",
                                                "28 µg/m^3 - 45 µg/m^3",
                                                "45 µg/m^3 - 71 µg/m^3",
                                                ">71 µg/m^3"))

Concentration_palette <- c("< 1 µg/m^3"="#dad085",
                           "1 µg/m^3 - 28 µg/m^3" = "#ebcc7b",
                           "28 µg/m^3 - 45 µg/m^3"="#e19947",
                           "45 µg/m^3 - 71 µg/m^3"="#c24221",
                           ">71 µg/m^3" = "#99082b")

df_plot_long$Anomaly <- factor(df_plot_long$Anomaly,
                               levels = c("-2sd to -0.5sd",
                                          "-0.5sd to 0sd",
                                          "0sd to 0.5sd", 
                                          "0.5sd to 2sd",
                                          ">2sd"))

Anomaly_palette <- c("-2sd to -0.5sd" = "#c1d9ed",
                     "-0.5sd to 0sd" = "#fee0d2",
                     "0sd to 0.5sd" = "#fb6a4a", 
                     "0.5sd to 2sd" = "#f2503e",
                     ">2sd" = "#69000d"
                     )

# UI"
ui <- page_navbar(
  selected = "Daily Smogstripes",
  collapsible = TRUE,
  window_title = "Daily Smogstripes",
  lang  = "en",
  theme = bslib::bs_theme(),
  sidebar = sidebar(
    title = "",
    open = "closed",
    dateRangeInput('dateRange',
                   label = 'Date range input:',
                   start = "2023-01-01",
                   end = "2024-01-01",
                   format = "yyyy-mm-dd",
                   min = min(df_plot_long$date),
                   max = max(df_plot_long$date)
    ),
    selectizeInput("cities_select", "Select Cities:", choices = unique(df_plot_long$city), 
                                       multiple = TRUE,
                                       options = list(maxItems = 2),
                                       selected = df_plot_long[df_plot_long$date >= as.Date("2023-01-01") & df_plot_long$date <= as.Date("2024-01-01"), "city"] %>% 		                                                           unique() %>% sample(1)
    ),
    radioButtons(
      choices = c('WHO', 'Anomaly', 'Concentration'),
      selected = 'WHO',
      width = "100%",
      inputId = "scale_select",
      label = "Select Graph Scale"
    )
  ),
  nav_panel(
    title = "Daily Smogstripes",
    plotlyOutput(outputId = "main_plot")
  )
)


# Server
server <- function(input, output) {
  output$main_plot <- renderPlotly({
    filtered_data <- df_plot_long[df_plot_long$city %in% input$cities_select & df_plot_long$date > input$dateRange[1] & df_plot_long$date < input$dateRange[2], ]
    
    # Your ggplot code
    aq_stripe <- ggplot(filtered_data,
                        aes(x = date, y = 1, fill = get(input$scale_select), label=pm25))+ 
      geom_tile() +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_manual(values = get(paste0(input$scale_select,'_palette'))) +
      guides(fill = guide_colorbar(barwidth = 1))+
      guides(fill = guide_legend(title = paste0(input$scale_select))) +
      labs(title = paste0("Daily PM25 Concentration between ",input$dateRange[1], " to ", input$dateRange[2]))+
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.title = element_blank(),
            panel.grid.major = element_blank(),
            legend.title = element_blank(),
            legend.position = 'top',
            legend.box = "horizontal",
            #axis.text.x = element_text(vjust = 3),
            axis.text.x = element_blank(),
            # axis.text.x = element_text(angle=90),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_textbox_simple(size = 10),
            plot.margin = margin(1, 0, 0, 0, "cm"),
            strip.text = ggtext::element_markdown(),
            strip.text.x = element_text(size=11, face="bold")
      )+# Adjust theme as needed
      facet_wrap(~City, ncol = 1)
    
    # Convert ggplot object to plotly
    p <- ggplotly(aq_stripe, tooltip= "all")
      # layout(title = list(text = paste0("1998-2022 AIR QUALITY BY WHO GUIDELINES (", unique(filtered_data$who_year),")") ,
      #                     x = 0, y = 1,
      #                     pad = list(b = 60, l = 0, r = 0 )))
    p <- layout(p, legend = list(orientation = 'h'))
    p
  })
}

# Run the app
shinyApp(ui, server)


