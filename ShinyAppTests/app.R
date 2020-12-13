# Poverty, Inequality, Social Structure, Services - data visualization

library(shiny)
library(tidyverse)
library(leaflet)
library(sf)


# data inequalities project
ineq_bern <- read_csv("data/Hack4SocialGood_data_collapsed.csv")

ineq_bern <- ineq_bern %>% 
  mutate(sozsich_sh = round(sozsich_sh, 2),
         Anzahl_Angebote_Arbeitsintegration_Pro_100 = round(Anzahl_Angebote_Arbeitsintegration / bev_total * 100, 3),
         Anzahl_Angebote_Kita_Pro_100 = round(Anzahl_Angebote_Kita / bev_total * 100, 3),
         Anzahl_Angebote_Tagesschule_Pro_100 = round(Anzahl_Angebote_Tagesschule / bev_total * 100, 3),
         Personen_Altersgruppe_0_4_2019_Perc = round(Personen_Altersgruppe_0_4_2019 / bev_total * 100, 2),
         Personen_Altersgruppe_5_9_2019_Perc = round(Personen_Altersgruppe_5_9_2019 / bev_total * 100, 2))

am.integrations.daten <- read.csv("data/am.integration.csv", encoding = "latin1") %>% 
  select(lon, lat, Name_Institution) %>% 
  drop_na()

# shape data Generalisierte Gemeindegrenzen 2015
shape <- read_sf("map_data/g1g19.shp", stringsAsFactors=FALSE) %>% 
  rename(bfsid = GMDNR)

# inner join with inequalities data
shape <- shape %>% 
  inner_join(., ineq_bern, by = "bfsid")

shape_leaflet <- st_transform(shape, "+proj=longlat +ellps=WGS84 +datum=WGS84")

# setup indicators inequalitiy
# scale_limits can be used for plotting
# i. e., if scale_limits is NA, defaults are used
indicator_ineq <- tribble(
  ~var_name, ~desc, ~suffix, ~round, ~scale_limits,
  "sozsich_sh", "Sozialhilfequote", "%", 1, c(min(shape$sozsich_sh), 
                                              max(shape$sozsich_sh)),
  "Jahresdurchschnitt_Arbeitslose_2019", "Jahresdurchschnitt Arbeitslose 19", NA, NA, NA,                                                                                                 
  "Jahresdurchschnitt_Arbeitslosenquote_2019", "Jahresdurchschnitt_Arbeitslosenquote 19", NA, NA, NA,
  "Personen_Altersgruppe_0_4_2019", "Personen Altersgruppe 0-4 (2019)", NA, NA, NA,
  "Personen_Altersgruppe_0_4_2019_Perc", "Personen Altersgruppe 0-4 (2019) in %", "%", NA, NA,
  "Personen_Altersgruppe_5_9_2019", "Personen Altersgruppe 5-9 (2019)", NA, NA, NA,
  "Personen_Altersgruppe_5_9_2019_Perc", "Personen Altersgruppe 5-9 (2019) in %", "%", NA, NA,
  "gini_stbetr", "Gini: Einnahmen aus der dBSt", NA, 2, c(0, 1),
  "gini_steink", "Gini: Steuerbares Einkommens", NA, 2, c(0, 1),
  "gini_steinka", "Gini: Steuerbares Aequivalenzeinkommen", NA, 2, c(0, 1),
  "gini_reink", "Gini: Reineinkommen", NA, 2, c(0, 1),
  "gini_reinka", "Gini: Reines Aequivalenzeinkommen", NA, 2, c(0, 1)
)

# setup indicators services
# scale_limits can be used for plotting
# i. e., if scale_limits is NA, defaults are used
indicator_service <- tribble(
  ~var_name, ~desc, ~suffix, ~round, ~scale_limits,
  "Anzahl_Angebote_Arbeitsintegration_Pro_100", "Anzahl Angebote Arbeitsintegration pro 100 Personen", NA, NA, NA,
  "Anzahl_Angebote_Kita_Pro_100", "Anzahl Angeobte Kita pro 100 Personen", NA, NA, NA,
  "Anzahl_Angebote_Tagesschule_Pro_100", "Anzahl Angeobte Tagesschule pro 100 Personen", NA, NA, NA,
  "Anzahl_Angebote_Arbeitsintegration", "Anzahl Angeobte Arbeitsintegration", NA, NA, NA,
  "Anzahl_Angebote_Kita", "Anzahl Angeobte Kita", NA, NA, NA,
  "Anzahl_Angebote_Tagesschule", "Anzahl Angeobte Tagesschule", NA, NA, NA
)



# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Angebote vor der Haustür? Exploration von Sozial- und Angebotsdaten auf Gemeinedeebene"),

  fluidRow(
    h4("Experimenteller Prototyp. Ergebnisse ohne Gewaehr!"),
  ),    

  # First Row
  # Map with inequality/poverty indicator
  fluidRow(
    h3("Verteilung Sozialindikator"),
    selectInput("indicator_ineq", "Indikator", 
                setNames(indicator_ineq$var_name, as.character(indicator_ineq$desc)),
                width = "100%"),
    leafletOutput("sf_leaflet_ineq"),
    p()
  ),    
  
  # Second Row
  # Map with service indicator
  fluidRow(
    h3("Verteilung Angebotsindikator"),
    selectInput("indicator_service", "Indikator:",
                setNames(indicator_service$var_name, as.character(indicator_service$desc)),
                width = "100%"),
    checkboxInput("plot_markers", "Angebote Arbeitsintegration anzeigen (Demo)", value = FALSE, width = "100%"),
    leafletOutput("sf_leaflet_service"),
    p()
  )
  
  #     
  # # Third Row
  # # Map with inequality/poverty indicator
  # fluidRow(
  #     h3("Verteilung Armuts-/Ungleichheitsindikator - Statische Karte"),
  #     plotOutput("sf_plot_ineq")
  # ),
  # 
  # # Forth Row
  # # Map with service indicator
  # fluidRow(
  #     h3("Verteilung Angebotsindikator"),
  #     plotOutput("sf_plot_service"),
  # )
)

# Define server
server <- function(input, output) {
  output$sf_leaflet_ineq <- renderLeaflet({
    pal <- colorNumeric(
      palette = "viridis",
      domain = shape_leaflet[[input$indicator_ineq]])
    
    indicator <- indicator_ineq %>% 
      filter(var_name == input$indicator_ineq) 
    
    legend_title <- indicator %>%  
      .$desc %>% 
      .[[1]]
    
    value_suffix <- indicator %>%  
      .$suffix %>% 
      .[[1]]
    
    if(is.na(value_suffix)) {value_suffix <- ""}
    
    shape_leaflet <- shape_leaflet %>% 
      mutate(tooltip_ineq = str_c(GMDNAME, 
                                  ": ",
                                  get(input$indicator_ineq), 
                                  value_suffix))
    
    leaflet(shape_leaflet) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(opacity = 0.35)) %>% 
      setView(7.3705, 46.8991, 8) %>%
      addPolygons(weight = 1.5,
                  color = "white",
                  fillOpacity = 0.8,
                  fillColor = ~pal(get(input$indicator_ineq)),
                  highlight = highlightOptions(weight = 5,
                                               color = "red", 
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = ~tooltip_ineq) %>%
      addLegend("bottomright", pal = pal, values = ~get(input$indicator_ineq),
                title = legend_title,
                labFormat = labelFormat(suffix = value_suffix),
                opacity = 1) 

  })
  
  output$sf_leaflet_service <- renderLeaflet({
    pal <- colorNumeric(
      palette = "magma",
      domain = shape_leaflet[[input$indicator_service]]
    )
    
    indicator <- indicator_service %>% 
      filter(var_name == input$indicator_service) 
    
    legend_title <- indicator %>%  
      .$desc %>% 
      .[[1]]
    
    value_suffix <- indicator %>%  
      .$suffix %>% 
      .[[1]]
    
    if(is.na(value_suffix)) {value_suffix <- ""}
    
    shape_leaflet <- shape_leaflet %>% 
      mutate(tooltip_service = str_c(GMDNAME, 
                                     ": ",
                                     get(input$indicator_service), 
                                     value_suffix))
    
    
    laef_services <- leaflet(shape_leaflet) %>%
      #addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(opacity = 0.35)) %>% 
      setView(7.3705, 46.8991, 8) %>%
      addPolygons(weight = 1.5,
                  color = "white",
                  fillOpacity = 0.8,
                  fillColor = ~pal(get(input$indicator_service)),
                  highlight = highlightOptions(weight = 5,
                                               color = "red", 
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = ~tooltip_service) %>% 
      addLegend("bottomright", pal = pal, values = ~get(input$indicator_service),
                title = legend_title,
                labFormat = labelFormat(suffix = value_suffix),
                opacity = 1
      )
    
    if(input$plot_markers){     
      laef_services <- laef_services %>% 
        addMarkers(lng = am.integrations.daten$lon, 
                   lat = am.integrations.daten$lat, 
                   label = am.integrations.daten$Name_Institution)
    }
    
    laef_services
  })
  
  
  # # render map with inequality/poverty indicator
  # output$sf_plot_ineq <- renderPlot({
  #     # get limits of the ineq scale
  #     limits_ineq <- indicator_ineq %>% 
  #         filter(var_name == input$indicator_ineq) %>%  
  #         .$scale_limits %>% .[[1]] 
  #     if(length(limits_ineq) != 2) {
  #         limits_ineq <- waiver()
  #     }
  #     
  #     ggplot(shape) + 
  #         geom_sf(aes(fill = get(input$indicator_ineq))) +
  #         scale_fill_viridis_c(name = input$indicator_ineq,
  #                              limits = limits_ineq) +
  #         theme_void()
  # })
  
  # # render map with service indicator
  # output$sf_plot_service <- renderPlot({
  #     # get limits of the service scale
  #     limits_service <- indicator_service %>% 
  #         filter(var_name == input$indicator_service) %>%  
  #         .$scale_limits %>% unlist(.) 
  #     if(length(limits_service) != 2) {
  #         limits_service <- NULL
  #     }
  #     
  #     ggplot(shape) + 
  #         geom_sf(aes(fill = get(input$indicator_service))) +
  #         scale_fill_viridis_c(name = input$indicator_service,
  #                              limits = limits_service,
  #                              option = "magma") +
  #         theme_void()
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
