# -----------------------------------------------------------------------------
# Purchase String Visualization (global.R)
# By : Edward Huh 
# Date: August 31, 2018
# -----------------------------------------------------------------------------
# install.packages("dplyr")
# require("dplyr")
# install.packages("gplots")
# install.packages("shinydashboard")
# install.packages("plotly")
# install.packages("tibble")
# install.packages("colourpicker")
# Initializations -------------------------------------------------------------

## Library Load ---------------------------------------------------------------

library("colourpicker")
library("data.table")
library("RColorBrewer")
library("shiny")
library("glue")
library("tibble")
library("stringr")
library("glue")
library("tibble")
library("stringr")
require("gplots")
library("shiny")
library("shinydashboard")
library("plotly")
library("tibble")

# -----------------------------------------------------------------------------
# General
# -----------------------------------------------------------------------------
# clolorScale: Basic color scale that inputs an integer
# and outputs that many distinct colors. Used for graphs.
colorScale <- function(count){
  if (count < 8) {
    gplots::col2hex(brewer.pal(count, "Set2"))
  } else{
    cl <- colors(distinct = TRUE)
    set.seed(15887)
    gplots::col2hex(sample(cl, count))
  }
}

# -----------------------------------------------------------------------------
# Visualization version 3
# -----------------------------------------------------------------------------
# easylife: An iterative process of building the three'layers' necessary 
# for sankey.
# If a layer needs to be modified, change this!
easylife <- function(level, n, subset){
  npdf  <- subset %>%
    filter(get(glue('SW.{level}')) == 'NO PURCHASE')
  swdf  <- subset %>%
    filter(get(glue('SW.{level}')) == 'SWITCH')
  repdf <- subset %>%
    filter(get(glue('SW.{level}')) == 'REPEAT')
  assign(glue('np{n}'), c(npdf[1,level], glue('NP{n}'), 
                          sum(npdf[5]), sum(npdf[6])))
  assign(glue('sw{n}'), c(swdf[1,level], glue('SW{n}'), 
                          sum(swdf[5]), sum(swdf[6])))
  assign(glue('rep{n}'), c(repdf[1,level],  glue('REP{n}'), 
                           sum(repdf[5]), sum(repdf[6])))
  
  result = data_frame(get(glue('np{n}')), get(glue('rep{n}')), 
                      get(glue('sw{n}')))
  colnames(result) <- NULL
  rownames(result) <- c("SOURCE", "END", "COUNT", "PERCENT")
  return(result)
}

# builder : After easylife builds the layers, this function
# combines them into a single dataframe.
builder <- function(subset){
  part1     <- easylife(1, 1, subset)
  part2     <- easylife(2, 2, filter(subset, SW.1 == 'REPEAT'))
  part2[1,] <- c("REP1")
  part3 <- easylife(2, 3, filter(subset, SW.1 == 'SWITCH'))
  part3[1,] <- c("SW1")
  part4 <- easylife(3, 4, filter(subset, SW.1 == 'REPEAT', SW.2 == 'REPEAT'))
  part4[1,] <- c("REP2")
  part5 <- easylife(3, 5, filter(subset, SW.1 == 'REPEAT', SW.2 == 'SWITCH'))
  part5[1,] <- c("SW2")
  part6 <- easylife(3, 6, filter(subset, SW.1 == 'SWITCH', SW.2 == 'REPEAT'))
  part6[1,] <- c("REP3")
  part7 <- easylife(3, 7, filter(subset, SW.1 == 'SWITCH', SW.2 == 'SWITCH'))
  part7[1,] <- c("SW3")
  
  final     <- as_tibble(t(cbind(part1, part2, part3, part4, part5, part6, part7)))
  return(final)
}

# update the label in sankey diagram to include ": %"
nameupdate <- function(name_vec, val_vec){
  length <- length(name_vec)
  i      <- 1
  length = length + 1
  while (i < length) {
    assign(name_vec[i], glue("{name_vec[i]}: {val_vec[i]}%"))
    i = i + 1
  }
  return(name_vec)
}

# Font specification for all diagrams -----------------------------------------
t <- list(
  family = "serif",
  size = 13)
