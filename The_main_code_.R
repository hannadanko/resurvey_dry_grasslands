
rm(list = ls(all = TRUE))
graphics.off()
shell("cls")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#--------------------------------------------------------------------------------#
# Here we load our vegetation tables and do some cleaning and formatting, preparing them for the further analysis.
source("1. Intro. Loading and processing the dataset.R")                      # for instant performing
# file.edit("1. Intro. Loading and processing the dataset.R")                   # for row-by-row performing



# Here we calculate mean Grazing Intensity (at the managed territories of the Festuco-Brometeae dataset) per site per 2012-2017.
source("2. Process data.R")
# file.edit("2. Process data.R")
 

# Processing and attaching data on species traits, ellenbergs, land use intensity.
source("3. Attaching species and sites data.R")
# file.edit("3. Attaching species and sites data.R")


# lmer models
source("4. Mod.R")
# file.edit("4. Mod.R")

# lmer models with grazing activities
source("5. Graz_model.R")
# file.edit("5. Graz_model.R")
