# PREDICT Actigraphy analysis:
#
# Script to pull up a previously parsed PREDICT data file, and add actigraphy
# focused data into it for further analysis
#
# Input: data pulled using PREDICT_2022-05-29.R
#
# Output: plots?
#
# Reference Files:
#
# History:
#   2022-10-07 RCH wrote it
#   2022-10-XX Aaron put your contributions here!

###############################
# I. Set up
###############################

# Set things that may vary depending on the person running the code
userid="AFR" # RCH or AFR
if(userid=="RCH"){
  # Library paths, as pulling deidentified data means working on the a VA computer
  .libPaths("C:/LocalR/R-4.0.5/library")
  # Import directories and options
  importdirectories<-list(
    github="C:\\Users\\vhapughendrr\\GitHub\\",
    workingdatasaves="R:\\MIRECC\\STUDIES\\PREDICT 01656\\Study Data\\Data\\Analyses\\2022-10-07\\",
    plotsaves="R:\\MIRECC\\STUDIES\\PREDICT 01656\\Study Data\\Data\\Analyses\\2022-10-07\\"
    
  )
}else if(userid=="AFR"){ # Aaron, update below to match your setup... at least the libpath and github (others can leave unless you want to change them, I set them up for this analysis)
  # Library paths, as pulling deidentified data means working on the a VA computer
  .libPaths("C:/LocalR/R-4.2.1/Library")
  # Import directories and options
  importdirectories<-list(
    github="U:\\My Documents\\GitHub\\",
    ActigraphyData="U:\\E070 Experiment R\\",
    ActigraphyProcessingSavesSubDirectory="ProcessingSaves\\",
    workingdatasaves="U:\\E070 Experiment R\\",
    plotsaves="U:\\E070 Experiment R\\"
  )
}else(
  error("UserID unrecognized... if you're someone besides RCH or AFR, add yourself!")
)

# Invarient settings:
importdirectories$startingdatafile="R:\\MIRECC\\STUDIES\\Hendrickson_1587955-01656_PREDICT\\Study Data\\Data\\Analyses\\2022-05-19\\"
importfilenames<-list(
  workingdata= "PREDICT_2022-05-26b.rds"
)

# Stuff to adjust for this particular run:
saveoptions <- list(
  resave=TRUE,
  savetag="Actigraphy"
)

# Libraries
library(devtools)
library(data.table)
library(ggplot2)
library(readr)
library(readxl)
library(openxlsx)
library(jsonlite)
library(edfReader)
library(ggpubr)
library(jtools)
library(lubridate)
library(nlme)

library(htmlTable)
library(ggpubr)
library(rvest) # for stripping html tags
library(flextable)
library(officer)
library(nlme)
library(psych)
library(psychTools)
library(ggstance) # needed by jtools
library(broom.mixed) # needed by ggstance!
library(gridExtra)

# Just source these, because package version never up to date:
source(paste0(importdirectories$github,"HendricksonLab\\HLUtilities\\R\\Utilities.R"))
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\GeneralDataFunctions.R"))
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\qualtricsFunctions.R"))
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\REDCapDataFunctions.R"))
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\VarMapFunctions.R"))
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\RCDoubleDataEntryManagement.R"))
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\WSAPfunctions.R"))
source(paste0(importdirectories$github,"HendricksonLab\\CoreInfastructure\\R\\actigraphyFunctions.R"))
source("~/GitHub/HendricksonLab/CoreInfastructure/R/actigraphyFunctions.R")

#################################
# II. Load the prepared data set, initial prep
#################################

dp <- readRDS(paste0(importdirectories$startingdatafile,importfilenames$workingdata))

# Aaron, put code to add actigraphy to this dp here

dp <- loadBasicActigraphyData(dp, importdirectories)#, overviewsavename="actigraphy_processing_overviewfile_2022-10-19.rds")

#run components of functions at one line at a time
#returns almost everything as NA

#################################
# III. Put analysis itself below!
#################################


