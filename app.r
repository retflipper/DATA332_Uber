library(shiny)
library(ggplot2)
library(bslib)
library(dplyr)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(hrbrthemes)
library(viridis)


setwd('C:/Users/retai/Documents/r_projects/uber')

apr_data <- read.csv("data/uber-raw-data-apr14.csv")
may_data <- read.csv("data/uber-raw-data-may14.csv")
jun_data <- read.csv("data/uber-raw-data-jun14.csv")
jul_data <- read.csv("data/uber-raw-data-jul14.csv")
aug_data <- read.csv("data/uber-raw-data-aug14.csv")
sep_data <- read.csv("data/uber-raw-data-sep14.csv")

uber_data <- full_join(apr_data, may_data)
uber_data <- full_join(uber_data, jun_data)
uber_data <- full_join(uber_data, jul_data)
uber_data <- full_join(uber_data, aug_data)
uber_data <- full_join(uber_data, sep_data)

uber_data <- uber_data %>%
  mutate(Date.Time = mdy_hms(Date.Time),
         Month = month(Date.Time),
         Day = day(Date.Time),
         Year = year(Date.Time),
         Hour = hour(Date.Time),
         Day_of_Week = wday(Date.Time, label = TRUE, abbr = FALSE),
         Week_of_Month = week(Date.Time) - week(floor_date(Date.Time, unit = "month")) + 1)

uber_data <- uber_data %>%
  mutate(Month = factor(Month, 
                        levels = 4:9, 
                        labels = c("April", "May", "June", "July", "August", "September")))



uber_trips_hours <- uber_data %>%
  group_by(Hour) %>%
  summarize(count = n())

uber_trips_hour_and_month <- uber_data %>%
  group_by(Month, Hour) %>%
  summarize(count = n())

uber_trips_month_and_day <- uber_data %>%
  group_by(Month, Day) %>%
  summarize(count = n())

uber_trips_month_and_dayname <- uber_data %>%
  group_by(Month, Day_of_Week) %>%
  summarize(count = n())

uber_trips_base_by_month <- uber_data %>%
  group_by(Base, Month) %>%
  summarize(count = n())

uber_trips_hour_day <- uber_data %>%
  group_by(Day, Hour) %>%
  summarize(count = n())

uber_trips_week_month <- uber_data %>%
  group_by(Month, Week_of_Month) %>%
  summarize(count = n())

uber_trips_base_by_weekday <- uber_data %>%
  group_by(Base, Day_of_Week) %>%
  summarize(count = n())

ui <- fluidPage(
  titlePanel(title = "New York Uber Rides Analysis"),
  
  navset_card_underline(
    
    nav_panel("Trips by Hour",
              fluidRow(
                column(2, tableOutput("trips_by_hour_table")),
                column(4, plotOutput("trips_by_hour_chart"), 
                       h5("This chart plots trips per hour of the day over the 
                          months that the data is collected. As can be seen in the
                          trend uber use in New York at the given period is most 
                          common during evening and night hours when you would
                          expect people to be drinking or to just to have outside of
                          work free time where they may be traveling to places not
                          as easily served by public transit infrastructure.")),
                column(6, plotOutput("trips_by_hour_and_month_chart"), 
                       h5("This chart plots trips per hour of the day grouped by
                          month, as can be seen all months follow a similar trend
                          however rides skew earlier in the day in april and later
                          in the day in september."))
              )),
    
    nav_panel("Trips by Month",
              fluidRow(column(2, selectInput("selected_month", "Choose a Month:", 
                                             choices = levels(uber_data$Month))
              )),
              fluidRow(
                column(2, tableOutput("trips_per_day_in_month")),
                column(4, plotOutput("trips_by_day_of_month")),
                column(6, plotOutput("trips_by_weekday_in_month"))
                
              ),
              h5("Both charts on this page show that there are no real consistent
                 trends over months on what days people in New York utilized uber
                 during the examinded time period.")),
    
    nav_panel("Trip totals by Bases and Months",
              fluidRow(
                column(2, plotOutput("trips_by_month"), 
                       h5("This chart displays a stead inscrease in uber use per
                          month in the period examing with each months uber use 
                          being greater than the last, this could be due to conditions
                          in the each month requiring greater uber use, but
                          it could also be that this data is from 2014 and is
                          actualy showing an increase in adoption of Uber by the
                          general populace of NYC.")),
                column(6, plotOutput("trips_by_month_and_bases"),
                       h5("This chart plots trips per uber based divided by month,
                          and shows a great increase in the trips in the B02617 in
                          the three later months completely dominating use of that
                          base in the earlier three months while the other bases
                          are generally equal from month to month, with the exception
                          of B02764 which spikes in the last two months and especially
                          September."))
              )),
    
    nav_panel("Trip Density Map",
              leafletOutput("map", height = "600px")),
    
    nav_panel("Heatmaps",
              fluidRow(
                column(6, plotOutput("hour_day_heat_map"),
                       h5("This heat map reinforces what seen earlier with
                          the large spike in use during the night and evening
                          in comparison to earlier in the day.")),
                column(6, plotOutput("month_day_heat_map"), 
                       h5("This heat map shows no real consistent pattern, tracking
                          with the fact that day numbers for each month are random
                          overtime with the values and rituals of that day for the
                          most part as day of week are a better view."))
              ),
              fluidRow(
                column(6, plotOutput("week_month_heat_map"), 
                       h5("This heat map shows that the first week of the month
                          typically has the lowest uber use likely due to 
                          regular users likely having just paid their rent and utilites,
                          reducing their desire and ability to spend on Uber.")),
                column(6, plotOutput("base_weekday_heat_map"),
                       h5("This heat map shows consistent higher use of Uber regardless
                          of base on Thursdays and Fridays near the end of the week
                          when people are more likely and willing to go out."))
              )
              
    ))
)

server <- function(input, output) {
  output$trips_by_hour_table <- renderTable(uber_trips_hours, striped = TRUE)
  
  output$trips_by_hour_chart <- renderPlot({
    ggplot(uber_trips_hours, aes(x = Hour, y = count)) + 
      geom_bar(stat = "identity", position = "dodge") +
      theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  output$trips_by_hour_and_month_chart <- renderPlot({
    ggplot(uber_trips_hour_and_month, aes(x = Hour, y = count, fill = Month)) + 
      geom_bar(stat = "identity", position = "dodge") +
      theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  filtered_data_month_and_day <- reactive({
    uber_trips_month_and_day %>% filter(Month == input$selected_month)
  })
  
  filtered_data_month_and_dayname <- reactive({
    uber_trips_month_and_dayname %>% filter(Month == input$selected_month)
  })
  
  output$trips_per_day_in_month <- renderTable(filtered_data_month_and_day(), striped = TRUE)
  
  output$trips_by_day_of_month <- renderPlot({
    ggplot(filtered_data_month_and_day(), aes(x = Day, y = count)) + 
      geom_bar(stat = "identity", position = "dodge") +
      theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  output$trips_by_weekday_in_month <- renderPlot({
    ggplot(filtered_data_month_and_dayname(), aes(x = Day_of_Week, y = count)) + 
      geom_bar(stat = "identity", position = "dodge") +
      theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  output$trips_by_month_and_bases <- renderPlot({
    ggplot(uber_trips_base_by_month, aes(x = Base, y = count, fill = Month)) + 
      geom_bar(stat = "identity", position = "dodge") +
      theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  output$trips_by_month <- renderPlot({
    ggplot(uber_trips_base_by_month, aes(x = Month, y = count)) + 
      geom_bar(stat = "identity", position = "stack") +
      theme(axis.text = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  
  output$map <- renderLeaflet({
    kde <- bkde2D(uber_data[, c("Lon", "Lat")], bandwidth = c(0.01, 0.01), gridsize = c(500, 500))
    
    density_grid <- expand.grid(Lon = kde$x1, Lat = kde$x2)
    density_grid$Density <- as.vector(kde$fhat)
    
    # **Exclude placeholder grid points**
    density_grid <- density_grid %>% filter(!is.na(Density) & Density > 5)
    
    tile_size <- diff(kde$x1)[1]  # Calculate tile size dynamically
    pal <- colorNumeric("YlOrRd", domain = density_grid$Density)
    
    leaflet(density_grid) %>%
      addTiles() %>%
      addRectangles(
        lng1 = ~Lon, lat1 = ~Lat,
        lng2 = ~Lon + tile_size, lat2 = ~Lat + tile_size,
        fillColor = ~pal(Density),
        fillOpacity = 0.7, stroke = FALSE
      ) %>%
      addLegend(pal = pal, values = density_grid$Density, title = "Density Levels", position = "bottomright")
  })
  
  output$hour_day_heat_map <- renderPlot({
    ggplot(uber_trips_hour_day, aes(Hour, Day, fill = count)) + 
      geom_tile() +
      scale_fill_viridis(discrete=FALSE) +
      theme_ipsum()
  })
  
  output$month_day_heat_map <- renderPlot({
    ggplot(uber_trips_month_and_day, aes(Month, Day, fill = count)) + 
      geom_tile() +
      scale_fill_viridis(discrete=FALSE) +
      theme_ipsum()
  })
  
  output$week_month_heat_map <- renderPlot({
    ggplot(uber_trips_week_month, aes(Month, Week_of_Month, fill = count)) + 
      geom_tile() +
      scale_fill_viridis(discrete=FALSE) +
      theme_ipsum()
  })
  
  output$base_weekday_heat_map <- renderPlot({
    ggplot(uber_trips_base_by_weekday, aes(Base, Day_of_Week, fill = count)) + 
      geom_tile() +
      scale_fill_viridis(discrete=FALSE) +
      theme_ipsum()
  })
}

shinyApp(ui=ui, server=server)