library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(psych)
library(DT)
library(shinycssloaders)
library(leaflet)
library(tigris)
library(sf)
library(scales)

# Load and clean data
data <- read_csv("KansasPopulation.csv")
mydata <- data[-c(106, 212, 318, 424, 530, 636, 742, 848, 954), ]

# Load Kansas county shapefiles
ks_counties <- counties(state = "KS", cb = TRUE, class = "sf")

# Prepare population by county (latest year)
latest_year <- max(data$Year)
county_pop <- data %>%
  filter(Year == latest_year) %>%
  group_by(County) %>%
  summarise(Population = sum(Population))

# Merge shapefile and population data
ks_counties$NAME <- toupper(ks_counties$NAME)
county_pop$County <- toupper(county_pop$County)
map_data <- left_join(ks_counties, county_pop, by = c("NAME" = "County"))

# UI
ui <- fluidPage(    
  titlePanel("Kansas Population Dataset Analysis"),
  
  fluidRow(
    column(
      width = 3,
      selectInput("sel_County", "Choose County:", choices = unique(data$County)),
      tags$p("Select a county to view yearly population trends.", style = "color: #7f8c8d;"),
      h4("Descriptive Statistics"),
      withSpinner(DTOutput("data"))
    ),
    column(
      width = 9,
      
      h3("Kansas County Population Map"),
      leafletOutput("map", height = "400px"),
      br(),
      
      fluidRow(
        column(width = 6, plotOutput("plot", height = "300px")),
        column(width = 6, plotOutput("barplot", height = "300px"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  newData <- reactive({
    req(input$sel_County)
    data %>%
      filter(County == input$sel_County) %>%
      group_by(Year) %>%
      summarise(Population = sum(Population), .groups = "drop")
  })
  
  output$plot <- renderPlot({
    df <- newData()
    ggplot(df, aes(x = Year, y = Population)) +
      geom_bar(stat = 'identity', fill = "#2980B9") +
      theme_minimal() +
      labs(
        title = paste("Population Trend in", input$sel_County, "County (2010–2018)"),
        x = "Year", y = "Population"
      ) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(breaks = df$Year) +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14)
      )
  })
  
  output$data <- renderDT({
    county_data <- newData()
    
    pop_change_county <- county_data$Population[county_data$Year == 2018] -
      county_data$Population[county_data$Year == 2010]
    percent_change_county <- (pop_change_county / county_data$Population[county_data$Year == 2010]) * 100
    
    data_2018 <- data %>%
      filter(Year == 2018) %>%
      group_by(County) %>%
      summarise(Population = sum(Population), .groups = "drop") %>%
      arrange(desc(Population)) %>%
      mutate(Rank = row_number())
    
    sel_county <- input$sel_County
    rank_2018 <- data_2018 %>% filter(County == sel_county) %>% pull(Rank)
    pop_2018 <- data_2018 %>% filter(County == sel_county) %>% pull(Population)
    
    arrow_change <- if (pop_change_county < 0) "🔻" else "🔺"
    arrow_percent <- if (percent_change_county < 0) "🔻" else "🔺"
    
    stats <- data.frame(
      Statistic = c("County Population Ranking", "Population (2018)",
                    "Population Change (2010–2018)", "Percent Population Change (2010–2018)"),
      County = c(
        formatC(rank_2018, format = "f", digits = 0),
        formatC(pop_2018, format = "f", digits = 0, big.mark = ","),
        paste0(formatC(pop_change_county, format = "f", digits = 2, big.mark = ","), " ", arrow_change),
        paste0(formatC(percent_change_county, format = "f", digits = 2), "%", " ", arrow_percent)
      )
    )
    
    colnames(stats)[2] <- paste(input$sel_County, "County")
    datatable(stats, options = list(dom = 't', autoWidth = TRUE), rownames = FALSE)
  })
  
  output$barplot <- renderPlot({
    data_2018 <- data %>%
      filter(Year == 2018) %>%
      group_by(County) %>%
      summarise(Population = sum(Population), .groups = "drop") %>%
      arrange(desc(Population)) %>%
      mutate(Rank = row_number())
    
    sel_county <- input$sel_County
    top10 <- data_2018 %>% slice(1:10)
    plot_data <- if (!(sel_county %in% top10$County)) {
      bind_rows(top10, data_2018 %>% filter(County == sel_county))
    } else {
      top10
    }
    
    plot_data$Highlight <- ifelse(plot_data$County == sel_county, "Selected", "Top 10")
    plot_data <- plot_data %>%
      arrange(desc(Population)) %>%
      mutate(CountyLabel = paste0(Rank, ". ", County))
    
    ggplot(plot_data, aes(x = reorder(CountyLabel, Population), y = Population, fill = Highlight)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("Selected" = "#E74C3C", "Top 10" = "#2980B9")) +
      theme_minimal() +
      labs(title = paste("Top 10 Most Populous Counties and \n", input$sel_County, "County (2018)"),
           x = "County (Ranked)", y = "Population") +
      scale_y_continuous(labels = comma) +
      theme(
        plot.title = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14)
      )
  })
  
  output$map <- renderLeaflet({
    sel_county <- toupper(input$sel_County)
    
    pal <- colorNumeric("YlGnBu", domain = map_data$Population)
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(Population),
        weight = 1,
        color = "#555",
        fillOpacity = 0.7,
        label = ~paste(NAME, "<br>", "Population:", formatC(Population, big.mark = ",")),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addPolygons(
        data = map_data %>% filter(NAME == sel_county),
        fillColor = "transparent",
        color = "red",
        weight = 3,
        fillOpacity = 0,
        label = ~paste(NAME, "(Selected)"),
        highlightOptions = highlightOptions(weight = 3, color = "red", bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, values = map_data$Population,
                title = "Population (2018)", labFormat = labelFormat(big.mark = ","))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
