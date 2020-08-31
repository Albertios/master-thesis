##################################################
## Project: Master Thesis
## Script purpose: Discrete Curve Evolution
## Date: 2020-03-19
## Author: Albert Hamzin
##################################################

library(sf)
library(raster)
library(dplyr)
library(spData)
library(tmap)
library(leaflet)
library(cartogram)
library(aspace)
library(matconv)
library(spatstat)
#Vector norms
#R does not have a norm function for vectors; https://cran.r-project.org/doc/contrib/Hiebeler-matlabR.pdf p.9
#https://de.wikipedia.org/wiki/Polarkoordinaten
# https://de.wikipedia.org/wiki/Komplexe_Zahl#Komplexe_Zahlenebene
# https://www.tf.uni-kiel.de/matwis/amat/mw1_ge/kap_2/basics/b2_1_5.html
# https://cis.temple.edu/~latecki/software.php

winkel <- function(Z) {
  His <- array()
  
  n <- length(Z) 

  for (j in 1:n) {
    
    LM <- sum(abs(Z[j - 1] - Z[j + 1])^1)^(1/1)

    LR <-  sum(abs(Z[j] - Z[j + 1])^1)^(1/1)

    LL <-  sum(abs(Z[j - 1] - Z[j])^1)^(1/1)
    
    
    # LM <- norm(as.matrix(Z[j-1]-Z[j+1]))                   
    # LR <- norm(as.matrix(Z[j]-Z[j+1])   )                       
    # LL <- norm(as.matrix(Z[j-1]-Z[j]) )

    alpha_value <- acos((LR^2 + LL^2 - LM^2) / (2 * LR * LL)) #^(-1)
    #alpha_value <- cos((LR^2 + LL^2 - LM^2) / (2 * LR * LL))^(-1)
   #print( which(is.nan(cos((LR^2 + LL^2 - LM^2) / (2 * LR * LL))^(-1))))
    
    # first_part <- (LR^2 + LL^2 - LM^2)
    # 
    # second_part <- (2 * LR * LL)
    # 
    # third_part <- first_part / second_part
    # 
    # alpha_value <- acos(third_part) #^-1

    
    a <-  180 - (alpha_value * 180 / pi)
    
    
    His[j] <-  a * LR * LL / (LR + LL)
    #His <- na.omit(His)
    
    
  }
  
  His[1] <- max(na.omit(His)) + 1
  His[n] <-  max(na.omit(His)) + 1
  K <-  His

  return(K)
}

#discrete_curve_evolution(A, 6)



 A<- cbind(x, y)
# A<- cbind(w[["bdry"]][[1]][["x"]], w[["bdry"]][[1]][["y"]])
# A <- cbind(unique(egg_convex_hull[["bdry"]][[1]][["x"]]), unique(egg_convex_hull[["bdry"]][[1]][["y"]]))
# A <- rbind(A, A[1,])


 

discrete_curve_evolution <- function(A, points_left) {
  
  z <- sqrt(as.complex(-1))
  
  Z <-  A[, 1]+as.complex(-1) * A[ , 2]
  X1 = Re(Z)
  Y1 = Im(Z)
  
  Z = A[, 1]+z * A[ , 2]
  X = Re(Z) #real
  Y = Im(Z) #imaginary
  
  X1 = Re(Z)
  Y1 = Im(Z)
  
  n = length(X1) - points_left #-1

  
  Zneu=Z;

if (n > 0) {
  
  for (i in 1:n) {
    #print(paste0("i: ", i))
    Winkelmass <- winkel(Zneu)
    sort_maass <- order(Winkelmass)
    #print(sort_maass)
    index_maass <- sort_maass[1] 
    Zneu[index_maass] <- NA
    Zneu <- na.omit(Zneu)

  }

}
  Zneu <- na.omit(Zneu)

  #Zneu <- replace(Zneu, Zneu==0, NA)

  #View(Zneu)
  
 # Zneu_length <- length(Zneu) + 1
#  Zneu[Zneu_length] <- Zneu[1]
  
  # xmax <- max(X)+0.1*max(X)
  # xmin <- min(X)-0.1*min(X)
  # ymax <- max(Y)+0.1*max(Y)
  # ymin <- min(Y)-0.1*min(Y)#Size of Axis

  plot(X1,Y1, type = "l")#erster Plot
    lines(Re(Zneu), Im(Zneu), col="red")

  #plot(Re(Zneu), Im(Zneu), type = "l")
 
  x_y_shape_coord <- list("long"= Im(Zneu), "lat"= Re(Zneu))

 return(x_y_shape_coord)
}

discrete_curve_evolution(A, 7)










