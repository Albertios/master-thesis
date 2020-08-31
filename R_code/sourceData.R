##################################################
## Project:
## Script purpose:
## Date:  08.04.2020
## Author: Albert Hamzin
##################################################

library(base)
library(stats)
library(ggplot2)
require(leaflet)
library(caTools)
library(plotly)

#library(MASS, quietly = TRUE)
library(Matrix, quietly = TRUE)
#library(EMCluster, quietly = TRUE)

library(lintr) # Checks adherence to a given style, syntax errors and possible semantic issues.
library(tidyverse) # is brimming with easy-to-pipe functions specifically built for common data manipulation tasks.


library(alphahull)

library(htmlwidgets)
library(htmltools)

library(raster)
library(mclust)
library(leaflet)
library(sp)
library(spatstat)
library(geosphere)
library(sf)


# Plot the alpha-convex hull ------------------------------------------------------

alpha_convex_calc <- function(dataFrame, alphaValue, year) {
  uniqueData <- unique(dataFrame)
  # Value of alpha
  alpha <- alphaValue
  # alpha-convex hull
  ahull.obj <- ahull(uniqueData, alpha = alpha)
  plot(
    ahull.obj,
    main = paste0(
      "alpha-convex hull year ",
      year,
      " with ",
      nrow(uniqueData) ,
      " points and alpha ",
      alphaValue
    )
  )#  ,xlab = "lon", ylab = "lat")# , xlim=c(0, 16), ylim=c(-20, 30))
  plot(
    ahull.obj,
    main = paste0(
      "alpha-convex hull in pink, alpha-shape in blue,sample points in black,
  voronoi diagram in green and Delaunay triangulation in red year ",
      year,
      " with ",
      nrow(uniqueData) ,
      " points and alpha ",
      alphaValue
    ),
    do.shape = TRUE,
    wlines = "both",
    col = c(6, 4, 1, 2, 3)
  )
}

alpha_convex_lim_calc <-
  function(dataFrame, alphaValue, year, xlim, ylim) {
    uniqueData <- unique(dataFrame)
    # Value of alpha
    alpha <- alphaValue
    # alpha-convex hull
    ahull.obj <- ahull(uniqueData, alpha = alpha)
    plot(
      ahull.obj,
      main = paste0(
        "alpha-convex hull year ",
        year,
        " with ",
        nrow(uniqueData) ,
        " points and alpha ",
        alphaValue
      )  ,
      xlab = "lon",
      ylab = "lat" ,
      xlim = xlim,
      ylim = ylim
    )
  }

# Plot the alpha-shape ------------------------------------------------------

alphaShapeCalc <- function(dataFrame, alphaValue, year) {
  uniqueData <- unique(dataFrame)
  # Value of alpha
  alpha <- alphaValue
  ashape.obj <- ashape(uniqueData, alpha = alpha)
  plot(
    ashape.obj,
    main = paste0(
      "alpha-shape", #year
      #year,
      " with ",
      nrow(uniqueData) ,
      " points and alpha ",
      alphaValue
    ) ,
    xlab = "lon", ylab = "lat")# , xlim=c(0, 16), ylim=c(-20, 30))
}

alphaShapeLimCalc <-
  function(dataFrame, alphaValue, year, xlim, ylim) {
    uniqueData <- unique(dataFrame)
    # Value of alpha
    alpha <- alphaValue
    ashape.obj <- ashape(uniqueData, alpha = alpha)
    plot(
      ashape.obj,
      main = paste0(
        "alpha-shape year ",
        year,
        " with ",
        nrow(uniqueData) ,
        " points and alpha ",
        alphaValue
      )  ,
      xlab = "lon",
      ylab = "lat" ,
      xlim = xlim,
      ylim = ylim
    )
  }


#################################################################################################################

#################################################################################################################

#################################################################################################################







# Einlesen der ACLED CSV Datei ------------------------------------------------------

#<https://en.wikipedia.org/wiki/Armed_Conflict_Location_and_Event_Data_Project>.


acledDataPath <-
  "/Users/alf/Documents/Uni/Masterarbeit/acled-data.csv"

acledData <-
  read.csv(acledDataPath, head = TRUE, sep = ",") # datei vorher uns UTF-8 umwandeln




# Pre-Processing Boko Haram ------------------------------------------------------

#EUR <- acledData[which( acledData$COUNTRY =="Cameroon" & grepl("Boko Haram", acledData$NOTES, acledData$ACTOR1, acledData$ACTOR2)), ]

indices <-
  which((
    acledData$COUNTRY == "Chad" |
    acledData$COUNTRY == "Niger" |
    acledData$COUNTRY == "Nigeria" | acledData$COUNTRY == "Cameroon"
  )
  &
    grepl(
      "Boko Haram",
      acledData$NOTES,
      acledData$ACTOR1,
      acledData$ACTOR2
    )
  ) #Filtern der Daten nach der Terrorgruppe "Boko Haram".

data_subset_Boko_Haram <-
  acledData[indices,] # nur Boko Haram relevanten Daten

filtered_data_Boko_Haram <-
  data.frame(
    "EVENT_ID_CNTY" = data_subset_Boko_Haram$EVENT_ID_CNTY,
    "EVENT_DATE" = data_subset_Boko_Haram$EVENT_DATE,
    "YEAR" = as.factor(data_subset_Boko_Haram$YEAR) ,
    "LATITUDE" = data_subset_Boko_Haram$LATITUDE,
    "LONGITUDE" = data_subset_Boko_Haram$LONGITUDE
  ) # extrahieren der vier Spalten



filtered_data_Boko_Haram$EVENT_DATE <-
  as.Date(filtered_data_Boko_Haram$EVENT_DATE, format = "%d/%m/%Y")

filtered_data_Boko_Haram$LATITUDE <-
  as.numeric(as.character(filtered_data_Boko_Haram$LATITUDE))

filtered_data_Boko_Haram$LONGITUDE <-
  as.numeric(as.character(filtered_data_Boko_Haram$LONGITUDE))

head(filtered_data_Boko_Haram, n = 4)

# split Boko Haram data in years ------------------------------------------------------




# get all years
years_Boko_Haram <-
  sort(as.character(unique(filtered_data_Boko_Haram$YEAR)))

# create data names
data_name_Boko_Haram <-
  paste0("filter_" , years_Boko_Haram, "_Boko_Haram")

# function create a data frame; input year as chr
get_year_Boko_Haram <-
  function(year_chr)
    filtered_data_Boko_Haram[which(grepl(year_chr, filtered_data_Boko_Haram$YEAR)),]


# creates multiple data frames
for (i in 1:length(years_Boko_Haram)) {
  #assign(data_name_Boko_Haram[i],get_year(years_Boko_Haram[i]))
}

#sum09_Boko_Haram <- data.frame("LONGITUDE"=filter09_Boko_Haram$LONGITUDE, "LATITUDE"=filter09_Boko_Haram$LATITUDE)




create_alpha_shape_plot_Boko_Haram <- function() {
  for (i in 1:length(years_Boko_Haram)) {
    
    df <- get_year_Boko_Haram(years_Boko_Haram[i])
    
    
    year <- as.numeric(years_Boko_Haram[i])
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    
    uniqueData <- unique(df_as_temporarily)
    
    if (nrow(uniqueData) > 2) {
      #ashape needs more than 2 points
      
      alphaShapeCalc(df_as_temporarily, 0.4, year)
      alpha_convex_calc(df_as_temporarily, 0.4, year)
    }
  }
  
}

create_Convex_plot_Boko_Haram <- function() {
  for (i in 1:length(years_Boko_Haram)) {
    
    df <- get_year_Boko_Haram(years_Boko_Haram[i])
    
    
    year <- as.numeric(years_Boko_Haram[i])
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    
    uniqueData <- unique(df_as_temporarily)
    
    if (nrow(uniqueData) > 2) {
      #ashape needs more than 2 points
      
      alphaShapeCalc(df_as_temporarily, 100, year)
      #alpha_convex_calc(df_as_temporarily, 100, year )
    }
  }
  
}


create_EM_plot_Boko_Haram <- function() {
  for (i in 1:length(years_Boko_Haram)) {
    df <- get_year_Boko_Haram(years_Boko_Haram[i])
    
    year <- as.numeric(years_Boko_Haram[i])
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    
    uniqueData <- unique(df_as_temporarily)
    
    if (nrow(uniqueData) > 2) {
      #ashape needs more than 2 points
      
      ret.em <-
        init.EM(df_as_temporarily,
                nclass = 2,
                method = "em.EM")
      
      
      
      plotem(
        ret.em,
        df_as_temporarily,
        main = paste0("year ", year , " with ", ret.em$n , " points") ,
        xlab = "lat",
        ylab = "lon"
      )
    }
  }
}

#create_EM_plot_Boko_Haram()




#################################################################################################################

#################################################################################################################

#################################################################################################################


# Pre-Processing Sudan & South Sudan ------------------------------------------------------

data_subset_Sudan <-
  acledData[which(acledData$COUNTRY == "Sudan" |
                    acledData$COUNTRY == "South Sudan"),]  # Filtern der Daten nach Sudan & South Sudan


filtered_data_Sudan <-
  data.frame(
    "EVENT_ID_CNTY" = data_subset_Sudan$EVENT_ID_CNTY,
    "EVENT_DATE" = data_subset_Sudan$EVENT_DATE,
    "YEAR" = as.factor(data_subset_Sudan$YEAR) ,
    "LATITUDE" = data_subset_Sudan$LATITUDE,
    "LONGITUDE" = data_subset_Sudan$LONGITUDE
  ) # extrahieren der vier Spalten



filtered_data_Sudan$EVENT_DATE <-
  as.Date(filtered_data_Sudan$EVENT_DATE, format = "%d/%m/%Y")

filtered_data_Sudan$LATITUDE <-
  as.numeric(as.character(filtered_data_Sudan$LATITUDE))

filtered_data_Sudan$LONGITUDE <-
  as.numeric(as.character(filtered_data_Sudan$LONGITUDE))

head(filtered_data_Sudan, n = 4)



# split Sudan data in years ------------------------------------------------------


# get all years
years_Sudan <- sort(as.character(unique(filtered_data_Sudan$YEAR)))

# create data names
data_name_sudan <-  paste0("filter_" , years_Sudan, "_Sudan")

# function create a data frame; input year as chr
get_year_Sudan <-
  function(year_chr)
    filtered_data_Sudan[which(grepl(year_chr, filtered_data_Sudan$YEAR)),]


#creates_df_plot_Sudan <- function(){
# creates multiple data frames
for (i in 1:length(years_Sudan)) {
  # assign(data_name_sudan[i],get_year(years_Sudan[i]))
  
  #df_temporarily <- get_year(years_Sudan[i])
  
  #year <- as.numeric(years_Sudan[i])
  #alphaShapeCalc(df_temporarily, 0.25, year )
}
#}


create_alpha_shape_plot_Sudan <- function() {
  for (i in 1:length(years_Sudan)) {
    
    df <- get_year_Sudan(years_Sudan[i])
    
    year <- as.numeric(years_Sudan[i])

    
    df <- df[which(df$YEAR == year),]

    
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    
    
    alphaShapeCalc(df_as_temporarily, 0.4, year)
    #alpha_convex_calc(df_as_temporarily, 0.4, year)
  }
  
}

#create_alpha_shape_plot()

# filtered_data_Sudan %>%
#   filter(YEAR == years_Sudan)




create_EM_plot_Sudan <- function() {
  for (i in 1:length(years_Sudan)) {
    df <- get_year_Sudan(years_Sudan[i])
    
    year <- as.numeric(years_Sudan[i])
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    
    uniqueData <- unique(df_as_temporarily)
    
    if (nrow(uniqueData) > 2) {
      #ashape needs more than 2 points
      
      ret.em <-
        init.EM(df_as_temporarily,
                nclass = 2,
                method = "em.EM")
      
      plotem(
        ret.em,
        df_as_temporarily,
        main = paste0("year ", year , " with ", ret.em$n , " points") ,
        xlim = c(20, 38),
        ylim = c(0, 28),
        xlab = "lat",
        ylab = "lon"
      )
    }
  }
}






#################################################################################################################

#################################################################################################################

#################################################################################################################






# Pre-Processing Democratic Republic of Congo ------------------------------------------------------

data_subset_Congo <-
  acledData[which(acledData$COUNTRY == "Democratic Republic of Congo"),]  # Filtern der Daten nach Democratic Republic of Congo


filtered_data_Congo <-
  data.frame(
    "EVENT_ID_CNTY" = data_subset_Congo$EVENT_ID_CNTY,
    "EVENT_DATE" = data_subset_Congo$EVENT_DATE,
    "YEAR" = as.factor(data_subset_Congo$YEAR) ,
    "LATITUDE" = data_subset_Congo$LATITUDE,
    "LONGITUDE" = data_subset_Congo$LONGITUDE
  ) # extrahieren der vier Spalten



filtered_data_Congo$EVENT_DATE <-
  as.Date(filtered_data_Congo$EVENT_DATE, format = "%d/%m/%Y")

filtered_data_Congo$LATITUDE <-
  as.numeric(as.character(filtered_data_Congo$LATITUDE))

filtered_data_Congo$LONGITUDE <-
  as.numeric(as.character(filtered_data_Congo$LONGITUDE))

head(filtered_data_Congo, n = 4)



# split Democratic Republic of Congo data in years ------------------------------------------------------


# get all years
years_Congo <- sort(as.character(unique(filtered_data_Congo$YEAR)))

# create data names
data_name_Congo <-  paste0("filter_" , years_Congo, "_Congo")

# function create a data frame; input year as chr
get_year_Congo <-
  function(year_chr)
    filtered_data_Congo[which(grepl(year_chr, filtered_data_Congo$YEAR)),]

creates_df_plot_Congo <- function() {
  # creates multiple data frames
  for (i in 1:length(years_Congo)) {
    assign(data_name_Congo[i], get_year(years_Congo[i]))
    
    df_temporarily <- get_year(years_Congo[i])
    
    year <- as.numeric(years_Congo[i])
    alphaShapeCalc(df_temporarily, 0.25, year)
    #alpha_convex_calc(df_temporarily, 0.25, year)
  }
}



create_alpha_shape_plot_Congo <- function() {
  for (i in 1:length(years_Congo)) {
    
    df <- get_year_Congo(years_Congo[i])
    
    year <- as.numeric(years_Congo[i])
    
    df <- df[which(df$YEAR == year),]

    
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    
    
    alphaShapeCalc(df_as_temporarily, 0.4, year)
    #alpha_convex_calc(df_as_temporarily, 0.4, year)
  }
  
}




create_EM_plot_Congo <- function() {
  for (i in 1:length(years_Congo)) {
    df <- get_year_Congo(years_Congo[i])
    
    year <- as.numeric(years_Congo[i])
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    
    uniqueData <- unique(df_as_temporarily)
    
    if (nrow(uniqueData) > 2) {
      #ashape needs more than 2 points
      
      ret.em <-
        init.EM(df_as_temporarily,
                nclass = 2,
                method = "em.EM")
      
      plotem(
        ret.em,
        df_as_temporarily,
        main = paste0("year ", year , " with ", ret.em$n , " points"),
        xlim = c(10, 35),
        ylim = c(-14, 6),
        xlab = "lat",
        ylab = "lon"
      )
    }
  }
}



#################################################################################################################

#################################################################################################################

#################################################################################################################





leaflet_chull_Boko <- function() {
  for (i in 1:length(years_Boko_Haram)) {
    df <- get_year_Boko_Haram(years_Boko_Haram[i])

    
    year <- as.numeric(years_Boko_Haram[i])
    
    my_title <- tags$p(tags$style("p {color: red; font-size:22px}"),
                       tags$b(year))
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    
    uniqueData <- unique(df_as_temporarily)
   
    if (nrow(uniqueData) > 3) {
      outline <-
        df_as_temporarily[chull(df_as_temporarily$LONGITUDE,
                                df_as_temporarily$LATITUDE), ]
      
      
   m <-  (leaflet(df_as_temporarily) %>% setView(lng = 7, lat = 10, zoom = 5) %>%
        # Base groups
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        # Overlay groups
        addCircles( ~ LONGITUDE,
                    ~ LATITUDE,
                    #stroke = F,
                    group = "Conflict") %>%
        addPolygons(
          data = outline,
          lng = ~ LONGITUDE,
          lat = ~ LATITUDE,
          #fill = F,
          weight = 2,
          color = "red",
          group = "Outline"
        ) %>%
        # Layers control
        addLayersControl(
          baseGroups = c("Toner (default)", "OSM", "Toner Lite"),
          overlayGroups = c("Conflict", "Outline"),
          options = layersControlOptions(collapsed = FALSE) 
        ) %>%
          addControl(my_title, position = "bottomright" )
   )  
   show(m)

    }
    
  }
}






#################################################################################################################

#################################################################################################################

#################################################################################################################





leaflet_chull_Congo <- function() {
  for (i in 1:length(years_Congo)) {
    df <- get_year_Congo(years_Congo[i])
    
    year <- as.numeric(years_Congo[i])
    my_title <- tags$p(tags$style("p {color: red; font-size:22px}"),
                       tags$b(year))
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    
    uniqueData <- unique(df_as_temporarily)
    
    if (nrow(uniqueData) > 3) {
      outline <-
        df_as_temporarily[chull(df_as_temporarily$LONGITUDE,
                                df_as_temporarily$LATITUDE), ]
      
      
      m <-  (leaflet(df_as_temporarily) %>% setView(lng = 25, lat = -3, zoom = 5) %>%
               # Base groups
               addTiles(group = "OSM (default)") %>%
               addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
               addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
               # Overlay groups
               addCircles( ~ LONGITUDE,
                           ~ LATITUDE,
                           #stroke = F,
                           group = "Conflict") %>%
               addPolygons(
                 data = outline,
                 lng = ~ LONGITUDE,
                 lat = ~ LATITUDE,
                 #fill = F,
                 weight = 2,
                 color = "red",
                 group = "Outline"
               ) %>%
               # Layers control
               addLayersControl(
                 baseGroups = c("Toner (default)", "OSM", "Toner Lite"),
                 overlayGroups = c("Conflict", "Outline"),
                 options = layersControlOptions(collapsed = FALSE)
               )%>%
               addControl(my_title, position = "bottomright" ))
      show(m)
    }
    
  }
}



#################################################################################################################

#################################################################################################################

#################################################################################################################





leaflet_chull_Sudan <- function() {
  for (i in 1:length(years_Sudan)) {
    df <- get_year_Sudan(years_Sudan[i])
    
    
    year <- as.numeric(years_Sudan[i])
    
    my_title <- tags$p(tags$style("p {color: red; font-size:22px}"),
                       tags$b(year))
    
    df <- df[which(df$YEAR == year),]
    
    df_as_temporarily <-
      data.frame("LONGITUDE" = df$LONGITUDE,
                 "LATITUDE" = df$LATITUDE)
    
    uniqueData <- unique(df_as_temporarily)
    
    if (nrow(uniqueData) > 3) {
      outline <-
        df_as_temporarily[chull(df_as_temporarily$LONGITUDE,
                                df_as_temporarily$LATITUDE), ]
      
      
      m <-  (leaflet(df_as_temporarily) %>% setView(lng = 31, lat = 12, zoom = 5) %>%
               # Base groups
               addTiles(group = "OSM (default)") %>%
               addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
               addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
               # Overlay groups
               addCircles( ~ LONGITUDE,
                           ~ LATITUDE,
                           #stroke = F,
                           group = "Conflict") %>%
               addPolygons(
                 data = outline,
                 lng = ~ LONGITUDE,
                 lat = ~ LATITUDE,
                 #fill = F,
                 weight = 2,
                 color = "red",
                 group = "Outline"
               ) %>%
               # Layers control
               addLayersControl(
                 baseGroups = c("Toner (default)", "OSM", "Toner Lite"),
                 overlayGroups = c("Conflict", "Outline"),
                 options = layersControlOptions(collapsed = FALSE) 
               ) %>%
               addControl(my_title, position = "bottomright" )
      )  
      show(m)
      
    }
    
  }
}
















