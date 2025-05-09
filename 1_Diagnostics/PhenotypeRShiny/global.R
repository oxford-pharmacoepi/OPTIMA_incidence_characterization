# Renv
#renv::activate()
renv::restore()
# .rs.restartR()

# Libraries
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(shinydashboard)
library(DT)
library(gt)
library(ggplot2)
library(plotly)
library(here)
library(dplyr)
library(stringr)
library(tidyr)
library(fresh)
library(rclipboard)
library(bit)
library(bit64)
library(rsconnect)
library(packrat)
library(readr)
library(rjson)
library(kableExtra)

# Variables
dataFolder <- "data"

# Functions
source(here("functions_shiny.R"))

# Data
source(here("prepare_data.R"))

# App
source(here("ui.R"))
source(here("server.R"))
shinyApp(ui, server)
