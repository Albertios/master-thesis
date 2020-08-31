leaflet(states) %>% addPolygons(fillColor = states@data[["Woche3"]]) %>%  addProviderTiles(providers$Stamen.TonerLite)  


result_map <- leaflet(states)

states$Woche3 <- as.numeric(states$Woche3)

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$Woche3, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>%g people /",
  states$name_2, states$Woche3
) %>% lapply(htmltools::HTML)

result_map %>% addPolygons(
  fillColor = ~pal(states$Woche3),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto"))

library(geojsonio)

counties <- readLines("/Users/alf/Documents/GitHub/Covid-19/RKI_COVID19_georeference/landkreise-in-germany.geojson")


states <- geojson_read("/Users/alf/Documents/GitHub/Covid-19/RKI_COVID19_georeference/landkreise-in-germany.geojson", what = "sp")
spdf_fortified <- geojsonio::geojson_read("/Users/alf/Documents/GitHub/Covid-19/RKI_COVID19_georeference/landkreise-in-germany.geojson", what = "sp")
class(states)







lankreis_data <- lankreis_data %>% 
  rename(
    LATITUDE = Lat,
    LONGITUDE = Lon
  )


week_3_conties_id <- merge(lankreis_data, covid_data_nine_weeks, by=c('LATITUDE','LONGITUDE'),all.x=T) #%>% na.omit()


week_3_conties_id <- week_3_conties_id %>% rename(
  cca_2 = Landkreis.ID
)

states@data[["cca_2"]] <- as.numeric(states@data[["cca_2"]])



chloropleth_map <- merge( states@data, week_3_conties_id, by=c('cca_2')) 

chloropleth_map <- chloropleth_map[,c(2,3,4,5,6,7,1,8,9,10,11,12,13,14)]

#chloropleth_map[is.na(chloropleth_map)] <- 0

states@data <- chloropleth_map

ggplot() +
  geom_polygon(data = states) 





