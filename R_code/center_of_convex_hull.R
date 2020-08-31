source("/Users/alf/Documents/GitHub/Master/R_code/sourceData.R")

Congo_EM_Convex_Hull <- function() {
konvex_hull_centroid <- matrix(nrow = 30 , ncol = 2)

line_data <- data.frame(nrow = NA , ncol = NA)

line_data <- data.frame( "Latitude"=line_data$nrow, "Longitude"=line_data$ncol)
line_data$ID <- NA

line_data_EM <- data.frame(nrow = NA , ncol = NA)

line_data_EM  <- data.frame( "Latitude"=line_data_EM$nrow, "Longitude"=line_data_EM$ncol)
line_data_EM$ID <- NA

html <- list()
for (i in 1:length(years_Congo)) {
  df <- get_year_Congo(years_Congo[i])
  
  year <- as.numeric(years_Congo[i])
  
  df <- df[which(df$YEAR == year),]
  konvex_hull_centroid[1, ] <- c(NA, NA) 
  df_as_temporarily <-
    data.frame("LONGITUDE" = df$LONGITUDE,
               "LATITUDE" = df$LATITUDE)
  uniqueData <- unique(df_as_temporarily)
  
  if (nrow(uniqueData) > 2) {
    df <-  df_as_temporarily
    
    mod <- Mclust(df_as_temporarily, G = 2)
    
    df$type <-  c(mod[["classification"]])
    
    
    x <- df$LONGITUDE
    y <- df$LATITUDE
    w <- convexhull.xy(x, y)
    
    
    
    type_one <- which(df$type == "1")
    egg <-df[type_one, ]
    unique_egg <- unique(egg)
    if (nrow(unique_egg) > 2) {
      egg_x <- egg$LONGITUDE
      egg_y <- egg$LATITUDE
      egg_convex_hull <- convexhull.xy(egg_x,egg_y)
      
      
      # Create a palette that maps factor levels to colors
      area_size <- area(w)/100*area(egg_convex_hull)
      
      if(area_size>50){
        type_one <- which(df$type == "2")
        egg <- df[type_one,]
        unique_egg <- unique(egg)
        egg_x <- egg$LONGITUDE
        egg_y <- egg$LATITUDE
        egg_convex_hull <- convexhull.xy(egg_x, egg_y)
        konvex_hull_centroid[i + 1, ] <- cbind(egg_convex_hull[["bdry"]][[1]][["x"]], egg_convex_hull[["bdry"]][[1]][["y"]]) %>% centroid()
        pal <- colorFactor(c("red", "navy"), domain = c("1", "2"))
        
        
      }else{
        pal <- colorFactor(c("navy", "red"), domain = c("1", "2"))
        konvex_hull_centroid[i + 1, ] <- cbind(egg_convex_hull[["bdry"]][[1]][["x"]], egg_convex_hull[["bdry"]][[1]][["y"]]) %>% centroid()
      }
      
      line_data[1,] <- c(konvex_hull_centroid[i, ], 1)
      line_data[2,] <- c(konvex_hull_centroid[i + 1, ], 1)
      
      
      
      blue_marker_one <- makeIcon(
        iconUrl = "https://cdn1.iconfinder.com/data/icons/Map-Markers-Icons-Demo-PNG/256/Map-Marker-Marker-Outside-Azure.png",
        iconWidth = 38, iconHeight = 55,
        iconAnchorX = 22, iconAnchorY = 54,
      )
      blue_marker_two <- makeIcon(
        iconUrl = "https://i.ya-webdesign.com/images/map-marker-png-8.png",
        iconWidth = 38, iconHeight = 55,
        iconAnchorX = 22, iconAnchorY = 54,
      )
      
      html <- c(html,
                list(
                  h3(paste0("Congo Year ", year)),
                  leaflet(df) %>% addTiles() %>%
                    addPolygons(
                      lng = w[["bdry"]][[1]][["x"]],
                      lat = w[["bdry"]][[1]][["y"]],
                      color = "green"
                    ) %>%
                    addPolygons(
                      lng = egg_convex_hull[["bdry"]][[1]][["x"]],
                      lat = egg_convex_hull[["bdry"]][[1]][["y"]],
                      color = "purple"
                    )%>%
                    addCircleMarkers(
                      data = df,
                      lat = ~ LATITUDE,
                      lng = ~ LONGITUDE,
                      radius = ~ ifelse(type == "1", 6, 6),
                      color = ~ pal(type),
                      stroke = FALSE,
                      fillOpacity = 0.5
                    )%>%
                    addMarkers(konvex_hull_centroid[i, 1], konvex_hull_centroid[i, 2], icon = blue_marker_two) %>%
                    addMarkers(konvex_hull_centroid[i+1, 1], konvex_hull_centroid[i+1, 2],  icon = blue_marker_one ) %>%
                    addPolylines(data=line_data,  lng = line_data$Latitude, lat = line_data$Longitude, group=line_data$ID )
                ))
    }
  }
}
return(tagList(html))
}




Sudan_EM_Convex_Hull <- function() {
  konvex_hull_centroid <- matrix(nrow = 30 , ncol = 2)
  
  line_data <- data.frame(nrow = NA , ncol = NA)
  
  line_data <- data.frame( "Latitude"=line_data$nrow, "Longitude"=line_data$ncol)
  line_data$ID <- NA
  
  line_data_EM <- data.frame(nrow = NA , ncol = NA)
  
  line_data_EM  <- data.frame( "Latitude"=line_data_EM$nrow, "Longitude"=line_data_EM$ncol)
  line_data_EM$ID <- NA
  
  html <- list()
  for (i in 1:length(years_Sudan)) {
    df <- get_year_Sudan(years_Sudan[i])
    
    year <- as.numeric(years_Sudan[i])
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    uniqueData <- unique(df_as_temporarily)
    
    if (nrow(uniqueData) > 2) {
      df <-  df_as_temporarily
      
      mod <- Mclust(df_as_temporarily, G = 2)
      
      df$type <-  c(mod[["classification"]])
      
      
      x <- df$LONGITUDE
      y <- df$LATITUDE
      w <- convexhull.xy(x, y)
      
      
      
      type_one <- which(df$type == "1")
      egg <-df[type_one, ]
      unique_egg <- unique(egg)
      if (nrow(unique_egg) > 2) {
        egg_x <- egg$LONGITUDE
        egg_y <- egg$LATITUDE
        egg_convex_hull <- convexhull.xy(egg_x,egg_y)
        
        
        # Create a palette that maps factor levels to colors
        area_size <- area(w)/100*area(egg_convex_hull)
        
        if(area_size>50){
          type_one <- which(df$type == "2")
          egg <- df[type_one,]
          unique_egg <- unique(egg)
          egg_x <- egg$LONGITUDE
          egg_y <- egg$LATITUDE
          egg_convex_hull <- convexhull.xy(egg_x, egg_y)
          konvex_hull_centroid[i + 1, ] <- cbind(egg_convex_hull[["bdry"]][[1]][["x"]], egg_convex_hull[["bdry"]][[1]][["y"]]) %>% centroid()
          pal <- colorFactor(c("red", "navy"), domain = c("1", "2"))
          
          
        }else{
          pal <- colorFactor(c("navy", "red"), domain = c("1", "2"))
          konvex_hull_centroid[i + 1, ] <- cbind(egg_convex_hull[["bdry"]][[1]][["x"]], egg_convex_hull[["bdry"]][[1]][["y"]]) %>% centroid()
        }
        
        line_data[1,] <- c(konvex_hull_centroid[i, ], 1)
        line_data[2,] <- c(konvex_hull_centroid[i + 1, ], 1)
        
        
        
        blue_marker_one <- makeIcon(
          iconUrl = "https://cdn1.iconfinder.com/data/icons/Map-Markers-Icons-Demo-PNG/256/Map-Marker-Marker-Outside-Azure.png",
          iconWidth = 38, iconHeight = 55,
          iconAnchorX = 22, iconAnchorY = 54,
        )
        blue_marker_two <- makeIcon(
          iconUrl = "https://i.ya-webdesign.com/images/map-marker-png-8.png",
          iconWidth = 38, iconHeight = 55,
          iconAnchorX = 22, iconAnchorY = 54,
        )
        
        html <- c(html,
                  list(
                    h3(paste0("Sudan Year ", year)),
                    leaflet(df) %>% addTiles() %>%
                      addPolygons(
                        lng = w[["bdry"]][[1]][["x"]],
                        lat = w[["bdry"]][[1]][["y"]],
                        color = "green"
                      ) %>%
                      addPolygons(
                        lng = egg_convex_hull[["bdry"]][[1]][["x"]],
                        lat = egg_convex_hull[["bdry"]][[1]][["y"]],
                        color = "purple"
                      )%>%
                      addCircleMarkers(
                        data = df,
                        lat = ~ LATITUDE,
                        lng = ~ LONGITUDE,
                        radius = ~ ifelse(type == "1", 6, 6),
                        color = ~ pal(type),
                        stroke = FALSE,
                        fillOpacity = 0.5
                      )%>%
                      addMarkers(konvex_hull_centroid[i, 1], konvex_hull_centroid[i, 2], icon = blue_marker_two) %>%
                      addMarkers(konvex_hull_centroid[i+1, 1], konvex_hull_centroid[i+1, 2],  icon = blue_marker_one ) %>%
                      addPolylines(data=line_data,  lng = line_data$Latitude, lat = line_data$Longitude, group=line_data$ID )
                  ))
      }
    }
  }
  return(tagList(html))
}



Boko_Haram_EM_Convex_Hull <- function() {
  konvex_hull_centroid <- matrix(nrow = 30 , ncol = 2)
  
  line_data <- data.frame(nrow = NA , ncol = NA)
  
  line_data <- data.frame( "Latitude"=line_data$nrow, "Longitude"=line_data$ncol)
  line_data$ID <- NA
  
  line_data_EM <- data.frame(nrow = NA , ncol = NA)
  
  line_data_EM  <- data.frame( "Latitude"=line_data_EM$nrow, "Longitude"=line_data_EM$ncol)
  line_data_EM$ID <- NA
  
  html <- list()
  for (i in 1:length(years_Boko_Haram)) {
    df <- get_year_Boko_Haram(years_Boko_Haram[i])
    
    year <- as.numeric(years_Boko_Haram[i])
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    uniqueData <- unique(df_as_temporarily)
    
    if (nrow(uniqueData) > 2) {
      df <-  df_as_temporarily
      
      mod <- Mclust(df_as_temporarily, G = 2)
      
      df$type <-  c(mod[["classification"]])
      
      
      x <- df$LONGITUDE
      y <- df$LATITUDE
      w <- convexhull.xy(x, y)
      
      
      
      type_one <- which(df$type == "1")
      egg <-df[type_one, ]
      unique_egg <- unique(egg)
      if (nrow(unique_egg) > 2) {
        egg_x <- egg$LONGITUDE
        egg_y <- egg$LATITUDE
        egg_convex_hull <- convexhull.xy(egg_x,egg_y)
        
        
        # Create a palette that maps factor levels to colors
        area_size <- area(w)/100*area(egg_convex_hull)
        
        if(area_size>50){
          type_one <- which(df$type == "2")
          egg <- df[type_one,]
          unique_egg <- unique(egg)
          egg_x <- egg$LONGITUDE
          egg_y <- egg$LATITUDE
          egg_convex_hull <- convexhull.xy(egg_x, egg_y)
          konvex_hull_centroid[i + 1, ] <- cbind(egg_convex_hull[["bdry"]][[1]][["x"]], egg_convex_hull[["bdry"]][[1]][["y"]]) %>% centroid()
          pal <- colorFactor(c("red", "navy"), domain = c("1", "2"))
          
          
        }else{
          pal <- colorFactor(c("navy", "red"), domain = c("1", "2"))
          konvex_hull_centroid[i + 1, ] <- cbind(egg_convex_hull[["bdry"]][[1]][["x"]], egg_convex_hull[["bdry"]][[1]][["y"]]) %>% centroid()
        }
        
        line_data[1,] <- c(konvex_hull_centroid[i, ], 1)
        line_data[2,] <- c(konvex_hull_centroid[i + 1, ], 1)
        
        
        
        blue_marker_one <- makeIcon(
          iconUrl = "https://cdn1.iconfinder.com/data/icons/Map-Markers-Icons-Demo-PNG/256/Map-Marker-Marker-Outside-Azure.png",
          iconWidth = 38, iconHeight = 55,
          iconAnchorX = 22, iconAnchorY = 54,
        )
        blue_marker_two <- makeIcon(
          iconUrl = "https://i.ya-webdesign.com/images/map-marker-png-8.png",
          iconWidth = 38, iconHeight = 55,
          iconAnchorX = 22, iconAnchorY = 54,
        )
        
        html <- c(html,
                  list(
                    h3(paste0("Boko Haram Year ", year)),
                    leaflet(df) %>% addTiles() %>%
                      addPolygons(
                        lng = w[["bdry"]][[1]][["x"]],
                        lat = w[["bdry"]][[1]][["y"]],
                        color = "green"
                      ) %>%
                      addPolygons(
                        lng = egg_convex_hull[["bdry"]][[1]][["x"]],
                        lat = egg_convex_hull[["bdry"]][[1]][["y"]],
                        color = "purple"
                      )%>%
                      addCircleMarkers(
                        data = df,
                        lat = ~ LATITUDE,
                        lng = ~ LONGITUDE,
                        radius = ~ ifelse(type == "1", 6, 6),
                        color = ~ pal(type),
                        stroke = FALSE,
                        fillOpacity = 0.5
                      )%>%
                      addMarkers(konvex_hull_centroid[i, 1], konvex_hull_centroid[i, 2], icon = blue_marker_two) %>%
                      addMarkers(konvex_hull_centroid[i+1, 1], konvex_hull_centroid[i+1, 2],  icon = blue_marker_one ) %>%
                      addPolylines(data=line_data,  lng = line_data$Latitude, lat = line_data$Longitude, group=line_data$ID )
                  ))
      }
    }
  }
  return(tagList(html))
}

