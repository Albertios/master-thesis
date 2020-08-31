##################################################
## Project: MAster Thesis
## Script purpose:
## Date:  03.04.2020
## Author: Albert Hamzin
##################################################


#source("R_code/sourceData.R")


library(mclust)



em_shape <<- NA

# create_EM_plot_Boko_Haram <- function() {
#   for (i in 1:length(years_Boko_Haram)) {
df <- get_year_Boko_Haram(years_Boko_Haram[8])

year <- as.numeric(years_Boko_Haram[8])

df <- df[which(df$YEAR == year), ]

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
  
  em_shape <<- ret.em
  plotem(
    ret.em,
    df_as_temporarily,
    main = paste0("year ", year , " with ", ret.em$n , " points") ,
    xlab = "lat",
    ylab = "lon"
  )
  
}
#   }
# }

assign.class(uniqueData)


ret.new <-
  assign.class(df_as_temporarily, ret.em, return.all = FALSE)
LTSigma2variance(df_as_temporarily)





mod <- Mclust(df_as_temporarily,G = 2)
summary(mod)
plot(mod, what = "classification", main = FALSE)
    lines(nigerX,nigerY)

plot(nigerX,nigerY, type = "l")#erster Plot
  points(mod, what = "classification", main = FALSE)


ggplot(mod)


plot(dens, what = "density", data = df_as_temporarily, grid = 100, points.cex = 0.5)
plot(dens, what = "density", type = "image", col = "steelblue", grid = 200)
plot(dens, what = "density", type = "persp", theta = -25, phi = 20,
       border = adjustcolor(grey(0.1), alpha.f = 0.3))

dens <- densityMclust(df_as_temporarily, G = 2)






#https://cran.r-project.org/web/packages/dlstats/vignettes/dlstats.html
library("ggplot2")
library("dlstats")

x <- cran_stats(c("mclust", "EMCluster", "mixtools"))

if (!is.null(x)) {
  head(x)
  p<-ggplot(x, aes(end, downloads, group = package, color = package)) +
    geom_line() + geom_point(aes(shape = package) )
  p+ theme_bw()
}




















