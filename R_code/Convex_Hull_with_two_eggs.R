##################################################
## Project:
## Script purpose:
## Date:  30.04.2020
## Author: Albert Hamzin
##################################################



source("/Users/alf/Documents/GitHub/Master/R_code/sourceData.R")

Boko_Haram_EM_Convex_Hull_two_eggs <- function() {
  html <- list()
  for (i in 1:length(years_Boko_Haram)) {
    df <- get_year_Boko_Haram(years_Boko_Haram[i])
    
    year <- as.numeric(years_Boko_Haram[i])
    
    df <- df[which(df$YEAR == year), ]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    uniqueData <- unique(df_as_temporarily)
    
    if (nrow(uniqueData) > 2) {
      df <-  df_as_temporarily
      
      mod <- Mclust(df_as_temporarily, G = 3)
      
      df$type <-  c(mod[["classification"]])
      
      
      x <- df$LONGITUDE
      y <- df$LATITUDE
      w <- convexhull.xy(x, y)
      
      type_one <- which(df$type == "1")
      egg_one <- df[type_one,]
      unique_egg_one <- unique(egg_one)
      
      type_two <- which(df$type == "2")
      egg_two <- df[type_two,]
      unique_egg_two <- unique(egg_two)
      
      type_three <- which(df$type == "3")
      egg_three <- df[type_three,]
      unique_egg_three <- unique(egg_three)
      
      if (nrow(unique_egg_one) > 2 ) {
        egg_one_x <- egg_one$LONGITUDE
        egg_one_y <- egg_one$LATITUDE
        egg_one_convex_hull <- convexhull.xy(egg_one_x, egg_one_y)
        
        egg_two_x <- egg_two$LONGITUDE
        egg_two_y <- egg_two$LATITUDE
        egg_two_convex_hull <- convexhull.xy(egg_two_x, egg_two_y)
        
        egg_three_x <- egg_three$LONGITUDE
        egg_three_y <- egg_three$LATITUDE
        egg_three_convex_hull <- convexhull.xy(egg_three_x, egg_three_y)
        
        # Create a palette that maps factor levels to colors
        area_size_one <- (100 / area(w)) * area(egg_one_convex_hull)
        area_size_two <- (100 / area(w)) * area(egg_two_convex_hull)
        area_size_three <- 100 - (area_size_one + area_size_two)
        
        area_size <-
          c(area_size_one, area_size_two, area_size_three) %>% which.max() %>% toString()
        
        order_min_to_max <-
          c(area_size_one, area_size_two, area_size_three) %>% order()  %>% as.character()
        
        
        color_list <-   c("CYAN"=area_size_one, "red"=area_size_two, "navy"=area_size_three)
        
        if (area_size == "1") {
          pal <- colorFactor(c("navy", "red", "CYAN"), domain = c("1", "2","3"))
          convex_egg_one <- egg_two_convex_hull
          convex_egg_two <- egg_three_convex_hull

        }else if (area_size == "2") {
          pal <- colorFactor(c("red", "navy", "CYAN"), domain = c("1", "2","3"))
          convex_egg_one <- egg_one_convex_hull
          convex_egg_two <- egg_three_convex_hull

        }else{
          pal <- colorFactor(c("CYAN", "red", "navy"), domain = c("1", "2","3"))
          convex_egg_one <- egg_one_convex_hull
          convex_egg_two <- egg_two_convex_hull

        }
        
        
        egg_merge <-convexhull.xy(rbind(data.frame(convex_egg_one[["bdry"]][[1]]), data.frame(convex_egg_two[["bdry"]][[1]])))
        
        
        
        html <- c(
          html,
          list(
            h3(paste0("Boko_Haram Year ", year)),
            leaflet(df) %>% addTiles() %>%
              addPolygons(
                lng = w[["bdry"]][[1]][["x"]],
                lat = w[["bdry"]][[1]][["y"]],
                color = "green"
              ) %>%
              addPolygons(lng = convex_egg_one[["bdry"]][[1]][["x"]],
                          lat = convex_egg_one[["bdry"]][[1]][["y"]],
                          color = "purple") %>% 
              addPolygons(lng = convex_egg_two[["bdry"]][[1]][["x"]],
                          lat = convex_egg_two[["bdry"]][[1]][["y"]],
                          color = "purple") %>% 
              
              addPolygons(lng = egg_merge[["bdry"]][[1]][["x"]],
                          lat = egg_merge[["bdry"]][[1]][["y"]],
                          color = "yellow") %>% 
              addCircleMarkers(
                data = df,
                lat = ~ LATITUDE,
                lng = ~ LONGITUDE,
                radius = ~ ifelse(type == "1", 6, 6),
                color = ~ pal(type),
                stroke = FALSE,
                fillOpacity = 0.5
              )
          )
        )
      }
    }
  }
  return(tagList(html))
}




Sudan_EM_Convex_Hull_two_eggs <- function() {
  html <- list()
  for (i in 1:length(years_Sudan)) {
    df <- get_year_Sudan(years_Sudan[i])
    
    year <- as.numeric(years_Sudan[i])
    
    df <- df[which(df$YEAR == year), ]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    uniqueData <- unique(df_as_temporarily)
    
    if (nrow(uniqueData) > 3) {
      df <-  df_as_temporarily
      
      mod <- Mclust(df_as_temporarily, G = 3)
      
      df$type <-  c(mod[["classification"]])
      
      
      x <- df$LONGITUDE
      y <- df$LATITUDE
      w <- convexhull.xy(x, y)
      
      type_one <- which(df$type == "1")
      egg_one <- df[type_one,]
      unique_egg_one <- unique(egg_one)
      
      type_two <- which(df$type == "2")
      egg_two <- df[type_two,]
      unique_egg_two <- unique(egg_two)
      
      type_three <- which(df$type == "3")
      egg_three <- df[type_three,]
      unique_egg_three <- unique(egg_three)
      
      if (nrow(unique_egg_three) > 3 ) {
        egg_one_x <- egg_one$LONGITUDE
        egg_one_y <- egg_one$LATITUDE
        egg_one_convex_hull <- convexhull.xy(egg_one_x, egg_one_y)
        
        egg_two_x <- egg_two$LONGITUDE
        egg_two_y <- egg_two$LATITUDE
        egg_two_convex_hull <- convexhull.xy(egg_two_x, egg_two_y)
        
        egg_three_x <- egg_three$LONGITUDE
        egg_three_y <- egg_three$LATITUDE
        egg_three_convex_hull <- convexhull.xy(egg_three_x, egg_three_y)
        
        # Create a palette that maps factor levels to colors
        area_size_one <- (100 / area(w)) * area(egg_one_convex_hull)
        area_size_two <- (100 / area(w)) * area(egg_two_convex_hull)
        area_size_three <- 100 - (area_size_one + area_size_two)
        
        area_size <-
          c(area_size_one, area_size_two, area_size_three) %>% which.max() %>% toString()
        
        order_min_to_max <-
          c(area_size_one, area_size_two, area_size_three) %>% order()  %>% as.character()
        
        
        color_list <-   c("CYAN"=area_size_one, "red"=area_size_two, "navy"=area_size_three)
        
        if (area_size == "1") {
          pal <- colorFactor(c("navy", "red", "CYAN"), domain = c("1", "2","3"))
          convex_egg_one <- egg_two_convex_hull
          convex_egg_two <- egg_three_convex_hull

        }else if (area_size == "2") {
          pal <- colorFactor(c("red", "navy", "CYAN"), domain = c("1", "2","3"))
          convex_egg_one <- egg_one_convex_hull
          convex_egg_two <- egg_three_convex_hull

        }else{
          pal <- colorFactor(c("CYAN", "red", "navy"), domain = c("1", "2","3"))
          convex_egg_one <- egg_one_convex_hull
          convex_egg_two <- egg_two_convex_hull

        }
        
        egg_merge <-convexhull.xy(rbind(data.frame(convex_egg_one[["bdry"]][[1]]), data.frame(convex_egg_two[["bdry"]][[1]])))
        
        html <- c(
          html,
          list(
            h3(paste0("Sudan Year ", year)),
            leaflet(df) %>% addTiles() %>%
              addPolygons(
                lng = w[["bdry"]][[1]][["x"]],
                lat = w[["bdry"]][[1]][["y"]],
                color = "green"
              ) %>%
              addPolygons(lng = convex_egg_one[["bdry"]][[1]][["x"]],
                          lat = convex_egg_one[["bdry"]][[1]][["y"]],
                          color = "purple") %>% 
              addPolygons(lng = convex_egg_two[["bdry"]][[1]][["x"]],
                          lat = convex_egg_two[["bdry"]][[1]][["y"]],
                          color = "purple") %>% 
              
              addPolygons(lng = egg_merge[["bdry"]][[1]][["x"]],
                          lat = egg_merge[["bdry"]][[1]][["y"]],
                          color = "yellow") %>% 
              addCircleMarkers(
                data = df,
                lat = ~ LATITUDE,
                lng = ~ LONGITUDE,
                radius = ~ ifelse(type == "1", 6, 6),
                color = ~ pal(type),
                stroke = FALSE,
                fillOpacity = 0.5
              )
          )
        )
      }
    }
  }
  return(tagList(html))
}









Congo_EM_Convex_Hull_two_eggs <- function() {
html <- list()
for (i in 1:length(years_Congo)) {
  df <- get_year_Congo(years_Congo[i])
  
  year <- as.numeric(years_Congo[i])
  
  df <- df[which(df$YEAR == year), ]
  
  df_as_temporarily <-
    data.frame("LONGITUDE" = df$LONGITUDE,
               "LATITUDE" = df$LATITUDE)
  uniqueData <- unique(df_as_temporarily)
  
  if (nrow(uniqueData) > 2) {
    df <-  df_as_temporarily
    
    mod <- Mclust(df_as_temporarily, G = 3)
    
    df$type <-  c(mod[["classification"]])
    
    
    x <- df$LONGITUDE
    y <- df$LATITUDE
    w <- convexhull.xy(x, y)
    
    type_one <- which(df$type == "1")
    egg_one <- df[type_one,]
    unique_egg_one <- unique(egg_one)
    
    type_two <- which(df$type == "2")
    egg_two <- df[type_two,]
    unique_egg_two <- unique(egg_two)
    
    type_three <- which(df$type == "3")
    egg_three <- df[type_three,]
    unique_egg_three <- unique(egg_three)
    
    if (nrow(unique_egg_one) > 2 ) {
      egg_one_x <- egg_one$LONGITUDE
      egg_one_y <- egg_one$LATITUDE
      egg_one_convex_hull <- convexhull.xy(egg_one_x, egg_one_y)
      
      egg_two_x <- egg_two$LONGITUDE
      egg_two_y <- egg_two$LATITUDE
      egg_two_convex_hull <- convexhull.xy(egg_two_x, egg_two_y)
      
      egg_three_x <- egg_three$LONGITUDE
      egg_three_y <- egg_three$LATITUDE
      egg_three_convex_hull <- convexhull.xy(egg_three_x, egg_three_y)
      
      # Create a palette that maps factor levels to colors
      area_size_one <- (100 / area(w)) * area(egg_one_convex_hull)
      area_size_two <- (100 / area(w)) * area(egg_two_convex_hull)
      area_size_three <- 100 - (area_size_one + area_size_two)
      
      area_size <-
        c(area_size_one, area_size_two, area_size_three) %>% which.max() %>% toString()
      
      order_min_to_max <-
        c(area_size_one, area_size_two, area_size_three) %>% order()  %>% as.character()
      
      
      color_list <-   c("CYAN"=area_size_one, "red"=area_size_two, "navy"=area_size_three)
      
      if (area_size == "1") {
        pal <- colorFactor(c("navy", "red", "CYAN"), domain = c("1", "2","3"))
        convex_egg_one <- egg_two_convex_hull
        convex_egg_two <- egg_three_convex_hull

      }else if (area_size == "2") {
        pal <- colorFactor(c("red", "navy", "CYAN"), domain = c("1", "2","3"))
        convex_egg_one <- egg_one_convex_hull
        convex_egg_two <- egg_three_convex_hull

      }else{
        pal <- colorFactor(c("CYAN", "red", "navy"), domain = c("1", "2","3"))
        convex_egg_one <- egg_one_convex_hull
        convex_egg_two <- egg_two_convex_hull

      }
      
      egg_merge <-convexhull.xy(rbind(data.frame(convex_egg_one[["bdry"]][[1]]), data.frame(convex_egg_two[["bdry"]][[1]])))

      html <- c(
        html,
        list(
          h3(paste0("Congo Year ", year)),
          leaflet(df) %>% addTiles() %>%
            addPolygons(
              lng = w[["bdry"]][[1]][["x"]],
              lat = w[["bdry"]][[1]][["y"]],
              color = "green"
            ) %>%
            addPolygons(lng = convex_egg_one[["bdry"]][[1]][["x"]],
                        lat = convex_egg_one[["bdry"]][[1]][["y"]],
                        color = "purple") %>% 
            addPolygons(lng = convex_egg_two[["bdry"]][[1]][["x"]],
                        lat = convex_egg_two[["bdry"]][[1]][["y"]],
                        color = "purple") %>% 
            
            addPolygons(lng = egg_merge[["bdry"]][[1]][["x"]],
                        lat = egg_merge[["bdry"]][[1]][["y"]],
                        color = "yellow") %>% 
            addCircleMarkers(
              data = df,
              lat = ~ LATITUDE,
              lng = ~ LONGITUDE,
              radius = ~ ifelse(type == "1", 6, 6),
              color = ~ pal(type),
              stroke = FALSE,
              fillOpacity = 0.5
            )
        )
      )
    }
  }
}
return(tagList(html))
}