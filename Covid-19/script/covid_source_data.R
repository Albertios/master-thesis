##################################################
## Project: Master
## Script purpose: import convert Covid-19 data
## Date:  07.05.2020
## Author: Albert Hamzin
##################################################

#library(lintr) # Checks adherence to a given style, syntax errors and possible semantic issues.
#library(tidyverse) # is brimming with easy-to-pipe functions specifically built for common data manipulation tasks.
#lint("/Users/alf/Documents/GitHub/Covid-19/script/covid_source_data.R")

library(mclust)
library(leaflet)
library(spatstat)
library(htmltools)
library(tidyverse)

landkreis_path <-
  "/Users/alf/Documents/GitHub/Covid-19/RKI_COVID19_georeference/Landkreise-Tabelle\ 1.csv"
rki_path <-
  "/Users/alf/Documents/GitHub/Covid-19/RKI_COVID19_georeference/RKI_COVID19_0-Tabelle\ 1.csv"

data_path <-
  "/Users/alf/Documents/GitHub/Covid-19/RKI_COVID19_georeference/data.csv"

lankreis_data <-
  read.csv(landkreis_path, head = FALSE, sep = ";")

rki_data <- read.csv(rki_path, head = FALSE, sep = ";")

covid_data_nine_weeks <- read.csv(data_path, head = TRUE, sep = ",")

covid_week_visualization <- function(){
  
  html <- list()
  for (i in 3:length(covid_data_nine_weeks)) {
    cd_week <-
      data.frame(covid_data_nine_weeks[, c(1:2, 5)]) %>% na.omit()
    cd_week[cd_week == 0] <- NA
    cd_week <- na.omit(cd_week)
    names(cd_week)[3] <- "Woche"
   # cd_week$Woche <- NULL
    #print(paste0("covid data original/before : ", length(cd_week[,1])))
    rows_to_add <- c()
    for (j in 1:length(cd_week[,1])) {
      number_of_infected <- cd_week[j,3] - 1
      if (number_of_infected > 0) {
        
        rows <- do.call("rbind", replicate(number_of_infected ,cd_week[j, ], simplify = FALSE))
        rows_to_add <- rbind(rows, rows_to_add)
      }
    }
    #print(paste0("rows_to_add: ",length(rows_to_add[,1])))
    cd_week <- rbind(rows_to_add, cd_week)
    #print(paste0("covid data after: ", length(cd_week[,1])))
    df_as_temporarily <- 
      data.frame("LONGITUDE" = cd_week$LONGITUDE,
                 "LATITUDE" = cd_week$LATITUDE)
    em_clusts <-  Mclust(df_as_temporarily, G = 2)
    
    cd_week$type <-  c(em_clusts[["classification"]])
    
    heatmap_covid <<-  cd_week
    
    pal <- colorFactor(c("red", "navy", "green"), domain = c("1", "2", "3"))
    
    html <- c(html,
              list(
                h3(paste0("Week ", 5 - 2)),
                leaflet(cd_week) %>% addTiles() %>%
                  addCircleMarkers(
                    data = cd_week,
                    lat = ~ LATITUDE,
                    lng = ~ LONGITUDE,
                    radius = ~ ifelse(type == "1", 6, 6),
                    color = ~ pal(type),
                    stroke = FALSE,
                    fillOpacity = 0.5,
                    popup = ~ as.character(Woche),
                    label = ~ as.character(Woche)
                  ) %>%  
                  addProviderTiles(providers$Stamen.TonerLite) %>%  
                addProviderTiles(providers$Stamen.TonerLines)
              ))
    
  }
  #return(tagList(html))
}

covid_week_visualization()


# table(mod[["classification"]])
# sum(table(mod[["classification"]]) )
# #length(unique(egg_convex_hull[["bdry"]][[1]][["x"]]))
# 
# #library(geosphere)
# A <- cbind(unique(egg_convex_hull[["bdry"]][[1]][["x"]]), unique(egg_convex_hull[["bdry"]][[1]][["y"]]))
# length(A[,1])
# areaPolygon(A) / 1000000 ## sq km
# 
# A <- cbind(unique(w[["bdry"]][[1]][["x"]]), unique(w[["bdry"]][[1]][["y"]]))
# 
# 
# leaflet(data = cd_week) %>% addTiles() %>%
#   addCircleMarkers(~LONGITUDE, ~LATITUDE, popup = ~as.character(Woche), label = ~as.character(Woche))














