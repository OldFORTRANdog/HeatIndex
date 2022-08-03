# Libraries -----
suppressPackageStartupMessages({
  # library(tidyverse)
})

# Constants ----
C1 <- -42.379
C2 <- 2.04901523
C3 <- 10.14333127
C4 <- -0.22475541
C5 <- -6.83783E-3
C6 <- -5.481717E-2
C7 <- 1.22874E-3
C8 <- 8.5282E-4
C9 <- -1.99E-6


# Functions ----
corrFactor <- function(TF,RH){
  CF <- 0.0
  if(TF >= 80. & TF <=112 & RH <= 13.0) {
    CF <- -((13-RH) / 4) * ((17- abs(TF -95)/17)^0.5)
  }
  else {
    if (TF >= 80.0 & TF <= 87.0 & RH >=85.0 ) {
      CF <- 0.02 * (RH -85.0) * (87 - TF)
    }
  }
  print(paste0("Coorection Factor is ", CF))
  return(CF)
}

heatindexF <- function(TF,RH) {
  hiF <- C1 + (C2 * TF) + (C3 * RH) + 
    (C4 * TF * RH) + (C5 * TF^2) + (C6 * RH^2) +
    (C7 * TF^2 * RH) + (C8 * TF * RH^2) +
    (C9 * TF^2 * RH^2) #+
    # corrFactor(TF, RH)
    
  return(hiF)
}


# Check Correction Factor Range ----

TRange <- c(80:87)
for (Tval in TRange) {
  print(paste0("TF is ", Tval))
  print(heatindexF(Tval,100))
}
