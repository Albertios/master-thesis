p <- ggplot(ellipse_array_egg_one_df, aes(X1, X2)) +
  geom_point() +
  stat_ellipse()
library(car)



p <- dataEllipse(convex_egg_one[["bdry"]][[1]][["x"]], convex_egg_one[["bdry"]][[1]][["y"]], levels=.999)
t <- dataEllipse(convex_egg_two[["bdry"]][[1]][["x"]], convex_egg_two[["bdry"]][[1]][["y"]], levels=0.99 )

pt <- rbind(p,t)
  
r <- concaveman(pt)  

discrete_curve_evolution(r,7)




plot(r)
points(t)
points(p)
points(ellipse_array_egg_one, col="red")
points(ellipse_array_egg_two, col="red")
points(egg_one_x, egg_one_y, col="blue")
points(egg_three_x, egg_three_y, col="blue")


pt <- rbind(p,t)

r <- concaveman(pt)  


plot(r)
points(t)
points(p)









ggplot(ellipse_array_egg_one_df)+
  geom_point(aes(X1, X2)) +
  stat_ellipse(aes(X1, X2))
  



#for (i in 1:length(years_Sudan)) {
  df <- get_year_Sudan(years_Sudan[19])
  
  year <- as.numeric(years_Sudan[19])
  
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
      print(paste0("blau ", length(egg_x)))
      # if (length(unique(egg_convex_hull[["bdry"]][[1]][["x"]])) > 7 ) {
      #   A <- cbind(unique(egg_convex_hull[["bdry"]][[1]][["x"]]), unique(egg_convex_hull[["bdry"]][[1]][["y"]]))
      #   A <- rbind(A, A[1,])
      #   dce_coord <-discrete_curve_evolution(A, 7)# %>% na.omit() 
      #   
      #   dce_coord <- coordinates(dce_coord)
      #   dce_coord <-  st_polygon(list(as.matrix(dce_coord)))
      #   dce_coord_buf = st_buffer(dce_coord, .4, endCapStyle = "SQUARE", joinStyle = "MITRE",  mitreLimit = 5)
      # 
      # html <- c(html,
      #           list(
      #             h3(paste0("Sudan Year ", year)),
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
                    #)%>%
                    addCircleMarkers(
                      data = df,
                      lat = ~ LATITUDE,
                      lng = ~ LONGITUDE,
                      radius = ~ ifelse(type == "1", 6, 6),
                      color = ~ pal(type),
                      stroke = FALSE,
                      fillOpacity = 0.5
                    ) %>%  addProviderTiles(providers$Stamen.TonerLite)  #%>%
                    #addProviderTiles(providers$Stamen.TonerLabels)  %>%
                 # addProviderTiles(providers$Stamen.TonerLines)
                 
                  
               # ))
    }
  }
#}

  table(mod[["classification"]])
  sum(table(mod[["classification"]]) )
#length(unique(egg_convex_hull[["bdry"]][[1]][["x"]]))

#library(geosphere)
A <- cbind(unique(egg_convex_hull[["bdry"]][[1]][["x"]]), unique(egg_convex_hull[["bdry"]][[1]][["y"]]))
length(A[,1])
areaPolygon(A) / 1000000 ## sq km

A <- cbind(unique(w[["bdry"]][[1]][["x"]]), unique(w[["bdry"]][[1]][["y"]]))

#sudan
256013.5 #2014 11
251241.5 #2015 13


#boko harama
117279.5 #2014 9
185629.9 #2015 10
163099.6 #2016 10





#congo
130717.3 #2013 8
161221.4 #2014 13
153968.7 #2015 9
162190.7 #2016 10




km <- c(130717.3, 161221.4, 153968.7, 162190.7)













