##################################################
## Project: Master Thesis
## Script purpose: combine EM & convex hull
## Date:  16.04.2020
## Author: Albert Hamzin
##################################################

source("/Users/alf/Documents/GitHub/Master/R_code/sourceData.R")
source("/Users/alf/Documents/GitHub/Master/R_code/Discrete_Curve_Evolution.R")

Boko_Haram_EM_Convex_Hull <- function() {
  html <- list()
  for (i in 1:length(years_Boko_Haram)) {
    df <- get_year_Boko_Haram(years_Boko_Haram[i])
    
    year <- as.numeric(years_Boko_Haram[i])
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    uniqueData <- unique(df_as_temporarily)
    
    dce_coord <- c()
    
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
        area_size <- spatstat::area(w)/100*spatstat::area(egg_convex_hull)
        
        if(area_size>50){
          type_one <- which(df$type == "2")
          egg <- df[type_one,]
          unique_egg <- unique(egg)
          egg_x <- egg$LONGITUDE
          egg_y <- egg$LATITUDE
          egg_convex_hull <- convexhull.xy(egg_x, egg_y)
          
          pal <- colorFactor(c("red", "navy"), domain = c("1", "2"))
        }else{
          pal <- colorFactor(c("navy", "red"), domain = c("1", "2"))
        }
        # 
        # if (length(unique(egg_convex_hull[["bdry"]][[1]][["x"]])) > 7 ) {
        # 
        #   A <- cbind(unique(egg_convex_hull[["bdry"]][[1]][["x"]]), unique(egg_convex_hull[["bdry"]][[1]][["y"]]))
        #   A <- rbind(A, A[1,])
        # 
        #   dce_coord <-discrete_curve_evolution(A, 7)# %>% na.omit() 
        #   
        #   dce_coord <- coordinates(dce_coord)
        #   dce_coord <-  st_polygon(list(as.matrix(dce_coord)))
        #   dce_coord_buf = st_buffer(dce_coord, .4, endCapStyle = "SQUARE", joinStyle = "MITRE",  mitreLimit = 5)
        #   
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
                      # addPolygons(
                      #   lng =dce_coord_buf[[1]][,2], 
                      #   lat =dce_coord_buf[[1]][,1], 
                      #   color = "red"
                      # )%>%
                      addCircleMarkers(
                        data = df,
                        lat = ~ LATITUDE,
                        lng = ~ LONGITUDE,
                        radius = ~ ifelse(type == "1", 6, 6),
                        color = ~ pal(type),
                        stroke = FALSE,
                        fillOpacity = 0.5
                      )
                  ))
       # }
      }
      
    }
  }
  return(tagList(html))
  
}


#################################################################################################################

#################################################################################################################

#################################################################################################################


# Sudan


Sudan_EM_Convex_Hull <- function() {
  html <- list()
  for (i in 1:length(years_Sudan)) {
    df <- get_year_Sudan(years_Sudan[i])
    
    year <- as.numeric(years_Sudan[i])
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    uniqueData <- unique(df_as_temporarily)
    
    dce_coord <- c()
    
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
        area_size <- spatstat::area(w)/100*spatstat::area(egg_convex_hull)
        
        if(area_size>50){
          type_one <- which(df$type == "2")
          egg <- df[type_one,]
          unique_egg <- unique(egg)
          egg_x <- egg$LONGITUDE
          egg_y <- egg$LATITUDE
          egg_convex_hull <- convexhull.xy(egg_x, egg_y)
          
          pal <- colorFactor(c("red", "navy"), domain = c("1", "2"))
        }else{
          pal <- colorFactor(c("navy", "red"), domain = c("1", "2"))
        }
        
        # if (length(unique(egg_convex_hull[["bdry"]][[1]][["x"]])) > 7 ) {
        #   A <- cbind(unique(egg_convex_hull[["bdry"]][[1]][["x"]]), unique(egg_convex_hull[["bdry"]][[1]][["y"]]))
        #   A <- rbind(A, A[1,])
        #   dce_coord <-discrete_curve_evolution(A, 7)# %>% na.omit() 
        #   
        #   dce_coord <- coordinates(dce_coord)
        #   dce_coord <-  st_polygon(list(as.matrix(dce_coord)))
        #   dce_coord_buf = st_buffer(dce_coord, .4, endCapStyle = "SQUARE", joinStyle = "MITRE",  mitreLimit = 5)
      
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
                    # addPolygons(
                    #   lng =dce_coord_buf[[1]][,2], 
                    #   lat =dce_coord_buf[[1]][,1], 
                    #   color = "red"
                    # )%>%
                    addCircleMarkers(
                      data = df,
                      lat = ~ LATITUDE,
                      lng = ~ LONGITUDE,
                      radius = ~ ifelse(type == "1", 6, 6),
                      color = ~ pal(type),
                      stroke = FALSE,
                      fillOpacity = 0.5
                    )
                ))
        #}
      }
    }
  }
  return(tagList(html))
}


#################################################################################################################

#################################################################################################################

#################################################################################################################

#Congo 

Congo_EM_Convex_Hull <- function() {
  html <- list()
  for (i in 1:length(years_Congo)) {
    df <- get_year_Congo(years_Congo[i])
    
    year <- as.numeric(years_Congo[i])
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    uniqueData <- unique(df_as_temporarily)
    
    dce_coord <- c()
    
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
        area_size <- spatstat::area(w)/100*spatstat::area(egg_convex_hull)
        
        if(area_size>50){
          type_one <- which(df$type == "2")
          egg <- df[type_one,]
          unique_egg <- unique(egg)
          egg_x <- egg$LONGITUDE
          egg_y <- egg$LATITUDE
          egg_convex_hull <- convexhull.xy(egg_x, egg_y)
          
          pal <- colorFactor(c("red", "navy"), domain = c("1", "2"))
        }else{
          pal <- colorFactor(c("navy", "red"), domain = c("1", "2"))
        }
        
        
        # if (length(unique(egg_convex_hull[["bdry"]][[1]][["x"]])) > 7 ) {
        #   A <- cbind(unique(egg_convex_hull[["bdry"]][[1]][["x"]]), unique(egg_convex_hull[["bdry"]][[1]][["y"]]))
        #   A <- rbind(A, A[1,])
        #   dce_coord <-discrete_curve_evolution(A, 7)# %>% na.omit() 
        #   
        #   dce_coord <- coordinates(dce_coord)
        #   dce_coord <-  st_polygon(list(as.matrix(dce_coord)))
        #   dce_coord_buf = st_buffer(dce_coord, .4, endCapStyle = "SQUARE", joinStyle = "MITRE",  mitreLimit = 5)
      
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
                    # addPolygons(
                    #   lng =dce_coord_buf[[1]][,2], 
                    #   lat =dce_coord_buf[[1]][,1], 
                    #   color = "red"
                    # )%>%
                    addCircleMarkers(
                      data = df,
                      lat = ~ LATITUDE,
                      lng = ~ LONGITUDE,
                      radius = ~ ifelse(type == "1", 6, 6),
                      color = ~ pal(type),
                      stroke = FALSE,
                      fillOpacity = 0.5
                    )
                ))
       # }
      }
    }
  }
  return(tagList(html))
}






#################################################################################################################

#################################################################################################################

#################################################################################################################

html <- list()
shape_input <- c()
for (i in 6:length(years_Sudan)) {
  df <- get_year_Boko_Haram(years_Boko_Haram[i])
  
  year <- as.numeric(years_Boko_Haram[i])
  
  df <- df[which(df$YEAR == year),]
  
  df_as_temporarily <-
    data.frame("LONGITUDE" = df$LONGITUDE,
               "LATITUDE" = df$LATITUDE)
  uniqueData <- unique(df_as_temporarily)
  
  dce_coord <- c()
  
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
      area_size <- spatstat::area(w)/100*spatstat::area(egg_convex_hull)
      
      if(area_size>50){
        type_one <- which(df$type == "2")
        egg <- df[type_one,]
        unique_egg <- unique(egg)
        egg_x <- egg$LONGITUDE
        egg_y <- egg$LATITUDE
        egg_convex_hull <- convexhull.xy(egg_x, egg_y)
        
        pal <- colorFactor(c("red", "navy"), domain = c("1", "2"))
      }else{
        pal <- colorFactor(c("navy", "red"), domain = c("1", "2"))
      }
      
      if (length(unique(egg_convex_hull[["bdry"]][[1]][["x"]])) > 7 )  {
        if (i!=8) {
    
        unique_shape <- cbind(unique(egg_convex_hull[["bdry"]][[1]][["x"]]), unique(egg_convex_hull[["bdry"]][[1]][["y"]]))
        #closed_unique_shape <- rbind(unique_shape, unique_shape[1,])
        shape_input <- rbind(shape_input, unique_shape)
        
        print(shape_input)
        
        }
        if (i == 8) {
          shape_input <- convexhull.xy(shape_input[,1], shape_input[,2])
          #shape_input <- unique(shape_input)
          dce_shape_input <- cbind(unique(shape_input[["bdry"]][[1]][["x"]]), unique(shape_input[["bdry"]][[1]][["y"]]))
          dce_shape_input <- rbind(dce_shape_input, dce_shape_input[1,])
          print(dce_shape_input)
          dce_coord <-discrete_curve_evolution(dce_shape_input, 7)# %>% na.omit()

          dce_coord <- coordinates(dce_coord)
          dce_coord <-  st_polygon(list(as.matrix(dce_coord)))
          dce_coord_buf = st_buffer(dce_coord, .4, endCapStyle = "SQUARE", joinStyle = "MITRE",  mitreLimit = 5)
      
      # html <- c(html,
      #           list(
      #             h3(paste0("Sudan Year ", year)),
                  leaflet(egg) %>% addTiles() %>%
                    # addPolygons(
                    #   lng = w[["bdry"]][[1]][["x"]],
                    #   lat = w[["bdry"]][[1]][["y"]],
                    #   color = "green"
                    # ) %>%
                    addPolygons(
                      lng = egg_convex_hull[["bdry"]][[1]][["x"]],
                      lat = egg_convex_hull[["bdry"]][[1]][["y"]],
                      color = "purple"
                    )%>%
                    addPolygons(
                      lng =dce_coord_buf[[1]][,2],
                      lat =dce_coord_buf[[1]][,1],
                      color = "black"
                    )%>%
                    addCircleMarkers(
                      data = egg,
                      lat = ~ LATITUDE,
                      lng = ~ LONGITUDE,
                      radius = ~ ifelse(type == "1", 6, 6),
                      color = ~ pal(type),
                      stroke = FALSE,
                      fillOpacity = 0.5
                    ) %>%  addProviderTiles(providers$Stamen.TonerLite)
              #  ))
      }
    }
    }
  }
}


table(mod[["classification"]])
sum(table(mod[["classification"]]) )
#length(unique(egg_convex_hull[["bdry"]][[1]][["x"]]))

#library(geosphere)
A <- cbind(unique(egg_convex_hull[["bdry"]][[1]][["x"]]), unique(egg_convex_hull[["bdry"]][[1]][["y"]]))
length(A[,1])
areaPolygon(A) / 1000000 ## sq km

A <- cbind(unique(w[["bdry"]][[1]][["x"]]), unique(w[["bdry"]][[1]][["y"]]))

