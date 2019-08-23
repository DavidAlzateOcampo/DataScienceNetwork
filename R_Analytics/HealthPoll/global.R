
library(ggplot2)
library(plotly)
library(dplyr)
source("UI_fld_names.R")
source("source.R")
data<-get.data.frm.csv(data.file)
data.2010 <- load(ends.2010.file)
data.2010 <- get(data.2010)

data.2015<-load(ends.2015.file)
data.2015 <- get(data.2015)

data.ALL <- load(ends.ALL.file)
data.ALL <- get(data.ALL)

data.TMP<-data
data<-data.2015
source("source_dynamics.R")
library(shiny)
library(shinythemes)
library(shinydashboard)
if(is.null(glbl.last.dimension.1.stat)){
glbl.last.dimension.1.stat<-""
}