# DATA322 April to September 2014 Rides Analysis
## Data Wrangling

```
apr_url <- getURL("https://raw.githubusercontent.com/retflipper/DATA332_Uber/refs/heads/main/data/uber-raw-data-apr14.csv")
apr_data <- read.csv(text = apr_url)

may_url <- getURL("https://raw.githubusercontent.com/retflipper/DATA332_Uber/refs/heads/main/data/uber-raw-data-may14.csv")
may_data <- read.csv(text = may_url)

jun_url <- getURL("https://raw.githubusercontent.com/retflipper/DATA332_Uber/refs/heads/main/data/uber-raw-data-jun14.csv")
jun_data <- read.csv(text = jun_url)

jul_url <- getURL("https://raw.githubusercontent.com/retflipper/DATA332_Uber/refs/heads/main/data/uber-raw-data-jul14.csv")
jul_data <- read.csv(text = jul_url)

aug_url <- getURL("https://raw.githubusercontent.com/retflipper/DATA332_Uber/refs/heads/main/data/uber-raw-data-aug14.csv")
aug_data <- read.csv(text = aug_url)

sep_url <- getURL("https://raw.githubusercontent.com/retflipper/DATA332_Uber/refs/heads/main/data/uber-raw-data-sep14.csv")
sep_data <- read.csv(text = sep_url)

uber_data <- full_join(apr_data, may_data)
uber_data <- full_join(uber_data, jun_data)
uber_data <- full_join(uber_data, jul_data)
uber_data <- full_join(uber_data, aug_data)
uber_data <- full_join(uber_data, sep_data)
```
- Wrangled the data into the shiny app by using the getUrl function pulling from this git repository, upload limits my classmates expierenced attempting to use this same method
were avoided by using git bash to upload the files rather than the browser GUI.

## Data Cleaning
```
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
```
- Cleaned and adjusted date data to a schema for easier analysis along with replacing month numbers with Month names and adding weekday name.

## Interesting code
- Most code is pretty standard gg plot code so the only thing I will spotlight is my leaflet code.
```
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
```
- In this code I created a large desnity grid with moderatley sized grid elements in the bandwidth and then filtered out with a desnity minumimum of greater than 5 all ultra low and no density fillers
  in the grid. I used this method rather than points as its a greater analysis of what was actually going on with the rides than just plotting 4.5 million points.

## Shiny App
https://lfaugustana.shinyapps.io/NYC_2014_UberRides_Analysis/
