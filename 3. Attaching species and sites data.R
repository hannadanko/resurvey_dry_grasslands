
# Attaching species and sites data ####

# Further processing and attaching data on species traits, ellenbergs, land use intensity
library(vegan)
library(tidyr)
library(dplyr)
#--------------------------------------------------------------------------------------------#

# German EIVE (Ellenbergs) loading/processing ####
EIVE_Paper_1.0 <- as.data.frame(read_xlsx("attaching_files/EIVE_Paper_1.0_SM_02.xlsx", sheet = 2))
EIVE_Paper_1.0 <- EIVE_Paper_1.0[EIVE_Paper_1.0$Source=="Germany",]
EIVE_Paper_1.0 <- EIVE_Paper_1.0[, c(2:4,6,11,16,21,26)]
colnames(EIVE_Paper_1.0)[4:8]
colnames(EIVE_Paper_1.0)[4:8] <- c("M", "N", "R", "L", "T")

# deleting duplicates (according to taxon concept). species with the same name and same Ellenbergs
duplicates <- duplicated(EIVE_Paper_1.0[, 3:ncol(EIVE_Paper_1.0)])
# removing the duplicate rows
EIVE_Paper_1.01 <- EIVE_Paper_1.0[!duplicates, ]

# species with the SAME NAME and DIFFERENT ELLENBERGS (for checking)
duplicated_values <- EIVE_Paper_1.01[duplicated(EIVE_Paper_1.01[, 3]), 3]
result <- duplicated_values[duplicated_values %in% SPP_info$standardized_name]

# deleting ALL duplicated taxon concept species with averaging their Ellenbergs
EIVE_Paper_1.02 <- aggregate(EIVE_Paper_1.01[c(4:8)], EIVE_Paper_1.01[3], mean)

# moss Ellenbergs loading/processing ####
mosses_lichens_ellenberg <- read_xls("attaching_files/Ellenberg_2001_Moose_Flechten_group.xls", sheet = 2)
mosses_lichens_ellenberg$N <- gsub("§", "", mosses_lichens_ellenberg$N)
mosses_lichens_ellenberg[4:9] <- apply(mosses_lichens_ellenberg[4:9], 2, as.numeric)

# Checking of duplicated moss species                            
duplicates <- duplicated(mosses_lichens_ellenberg[2])
dupli <- mosses_lichens_ellenberg[duplicates,2]
dupli <- dupli[dupli$Name %in% SPP_info$standardized_name,]
dupli

# aggregating moss Ellenberg table by apecies names (appointing means to Ellenberg values)
mosses_lichens_ellenberg2 <- aggregate(mosses_lichens_ellenberg[c(4:9)], mosses_lichens_ellenberg[2], mean)

# subsetting releve table into the moss and not moss parts for the next adding of the Ellenbergs 
TABLO_moss <- SPP_info[SPP_info$standardized_name %in% 
                         mosses_lichens_ellenberg2$Name,]
TABLO_not_moss <- setdiff(SPP_info, TABLO_moss)

# merging parts with Ellenbergs ####
TABLO_moss_ellenbergs <- merge(TABLO_moss, mosses_lichens_ellenberg2, by.x = "standardized_name", 
                               by.y = "Name", all.x = TRUE)
TABLO_vasc_ellenbergs <- merge(TABLO_not_moss, EIVE_Paper_1.02, by.x = "standardized_name", 
                               by.y = "TaxonConcept", all.x = TRUE)

# unification of the "moisture" colnames
colnames(TABLO_moss_ellenbergs)[which(colnames(TABLO_moss_ellenbergs)=="F")] <- "M"
TABLO_moss_ellenbergs$K <- NULL

# co-ordering Ellenberg columns in the vascular and moss tables
TABLO_vasc_ellenbergs <- TABLO_vasc_ellenbergs[colnames(TABLO_moss_ellenbergs)]

all(colnames(TABLO_vasc_ellenbergs)==colnames(TABLO_moss_ellenbergs))

# row-binding of the moss and vascular plant tables
TABLO_ellenbergs <- rbind(TABLO_moss_ellenbergs,TABLO_vasc_ellenbergs)

# alphabetical ordering
SPP_info <- TABLO_ellenbergs[order(TABLO_ellenbergs$standardized_name), ]

canopy_height <- read.csv("attaching_files/canopy_heigh.csv", sep=";")
leaf_size <- read.csv("attaching_files/leaf_size.csv", sep=";")
seed_mass <- read.csv("attaching_files/seed_mass.csv", sep=";")
SLA_und_geo_neu2 <- read.csv("attaching_files/SLA_und_geo_neu2.csv", sep=";")

# canopy_height ####
# aggregation of single values in canopy_height by SBS.name (by averaging)
names(canopy_height)
agg_canopy <-aggregate(single.value..m. ~ SBS.name, data = canopy_height, mean)
# merging with SPP_info
SPP_info <- merge(SPP_info, agg_canopy,  by.y = "SBS.name", by.x = "standardized_name", all.x = TRUE)

# seed_mass part ####
names(seed_mass)
# aggregation of the seed_mass table by SBS.name (by choosing means)
agg_seed_mass <-aggregate(mean.SM..mg. ~ SBS.name, data = seed_mass, mean)
SPP_info <- merge(SPP_info, agg_seed_mass,  by.y = "SBS.name", by.x = "standardized_name", all.x = TRUE)

seed_weight_Ecoflora <- read.csv("attaching_files/seed_weight_Ecoflora.csv")

# 2_word_sp_names for correct merging
words <- strsplit(SPP_info$standardized_name, " ")
first_two_words <- sapply(words, function(x) paste(x[1:2], collapse = " "))
SPP_info$Correct_name_without_authors <- first_two_words

SPP_info <- merge(SPP_info, seed_weight_Ecoflora[c(1,3)], by.x = "Correct_name_without_authors", by.y = "X", all.x = T)
SPP_info <- SPP_info[-1]

SPP_info$mean.SM..mg. <- ifelse(is.na(SPP_info$mean.SM..mg.), SPP_info$avg_seed_wght,
                                SPP_info$mean.SM..mg.)
SPP_info <- dplyr::select(SPP_info, -avg_seed_wght)

# leaf_size part ####
names(leaf_size)
agg_leaf_size <-aggregate(mean.LS..mm.2. ~ SBS.name, data = leaf_size, mean)
SPP_info <- merge(SPP_info, agg_leaf_size,  by.y = "SBS.name", by.x = "standardized_name", all.x = TRUE)

# Adding data from Ecoflora
traits_dataframe <- read.csv("attaching_files/traits_Ecoflora_leaf_area.csv")
# 2_word_sp_names for correct merging
words <- strsplit(SPP_info$standardized_name, " ")
first_two_words <- sapply(words, function(x) paste(x[1:2], collapse = " "))
SPP_info$Correct_name_without_authors <- first_two_words

SPP_info <- merge(SPP_info, traits_dataframe[c(1,3)], by.x = "Correct_name_without_authors", by.y = "X", all.x = T)
SPP_info <- SPP_info[-1]

SPP_info$mean.LS..mm.2. <- ifelse(is.na(SPP_info$mean.LS..mm.2.),SPP_info$le_area_2*100,
                                  SPP_info$mean.LS..mm.2.)
SPP_info <- dplyr::select(SPP_info, -le_area_2)
# SLA part. SLA_und_geo_neu2
# Adding data 
# 2_word_sp_names for correct merging
words <- strsplit(SPP_info$standardized_name, " ")
first_two_words <- sapply(words, function(x) paste(x[1:2], collapse = " "))
SPP_info$Correct_name_without_authors <- first_two_words


sla_short <- SLA_und_geo_neu2[SLA_und_geo_neu2$SBS.name %in% SPP_info$Correct_name_without_authors,]
agg_sla_short <-aggregate(mean.SLA..mm.2.mg. ~ SBS.name, data = sla_short, mean)
SPP_info <- merge(SPP_info, agg_sla_short, by.x = "Correct_name_without_authors", by.y = "SBS.name", all.x = T)
SPP_info <- SPP_info[-1]

# giving more reasonable names to column
names(SPP_info)
SPP_info <- SPP_info %>%
  rename(canopy_height = single.value..m.,
         seed_mass = mean.SM..mg.,
         leaf_size = mean.LS..mm.2.,
         sla = mean.SLA..mm.2.mg.)

#--------------------------------------------------------------------------------------------#
# Disturbance file processing ####
disturbance <- read.csv("attaching_files/disturbance_indicator_values.csv")
SPP_info <- merge(SPP_info, disturbance[c(1,2,5:ncol(disturbance))], by.x = "standardized_name", by.y = "species", all.x = T)

#--------------------------------------------------------------------------------------------#
Families <- read.csv("input_data/SPP_Families.csv")[-1]
SPP_info$standardized_name==Families$standardized_name # checking for names coincidence, sp 250 -- "Medicago ×varia"
SPP_info$family <- Families$family

#--------------------------------------------------------------------------------------------#
# adding information about N deposition to the HEADER (plot info) ####
Nitr <- read_xlsx("attaching_files/N_dep.xlsx")
HEADER <- cbind(HEADER, Nitr[5])

#--------------------------------------------------------------------------------------------#
# looking for alien species ####
aliens <- read.csv("attaching_files/esy_data.csv", sep=";")
aliens_Brandenburg <- aliens[which(aliens$region_id=="1288"),]

# checking SPP_info for species which are alien
alien_spp_list <- SPP_info[which(SPP_info$standardized_name %in% aliens_Brandenburg$standardized_name),1]
aliens_Brandenburg[aliens_Brandenburg$standardized_name %in% SPP_info$standardized_name, ]
# adding information about alien spp.
SPP_info$aliens <- NA
SPP_info$aliens[which(SPP_info$standardized_name %in% alien_spp_list)] <- "+"

# putting columns into more convenient places
SPP_info <- SPP_info[c(1:2, ncol(SPP_info), 3:(ncol(SPP_info)-1))]
SPP_info$standardized_name[!is.na(SPP_info$aliens)]
#--------------------------------------------------------------------------------------------#
# Adding info about aliens to the HEADER ####

HEADER$alien_sp <- NA

for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  sum_values <- 0  # initial value
  for (j in 1:nrow(TABLO)) {
    if (!is.na(TABLO[j, col]) && !is.na(SPP_info[j, "aliens"]) && TABLO[j, col] != "0") {
      sum_values <- sum_values + 1   
    }
  }
  # write it in HEADER
  HEADER$alien_sp[match(col, HEADER$New_plot_ID)] <- sum_values
}

#--------------------------------------------------------------------------------------------#
# Adding Ellenberg and traits means to the HEADER #### (and other traits means ?)

# L_mean
for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  valid_rows <- which(!is.na(SPP_info[["L"]]) & TABLO[[col]] != 0)
  average_value <- mean(SPP_info[["L"]][valid_rows], na.rm = TRUE)
  # Writing mean L-value to a table
  HEADER$L_mean[match(col, HEADER$New_plot_ID)] <- average_value
}

# T_mean
for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  valid_rows <- which(!is.na(SPP_info[["T"]]) & TABLO[[col]] != 0)
  average_value <- mean(SPP_info[["T"]][valid_rows], na.rm = TRUE)
  # Writing mean T-value to a table
  HEADER$T_mean[match(col, HEADER$New_plot_ID)] <- average_value
}

# M_mean
for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  valid_rows <- which(!is.na(SPP_info[["M"]]) & TABLO[[col]] != 0)
  average_value <- mean(SPP_info[["M"]][valid_rows], na.rm = TRUE)
  # Writing mean F-value to a table
  HEADER$M_mean[match(col, HEADER$New_plot_ID)] <- average_value
}

# R_mean
for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  valid_rows <- which(!is.na(SPP_info[["R"]]) & TABLO[[col]] != 0)
  average_value <- mean(SPP_info[["R"]][valid_rows], na.rm = TRUE)
  # Writing mean R-value to a table
  HEADER$R_mean[match(col, HEADER$New_plot_ID)] <- average_value
}

# N_mean
for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  valid_rows <- which(!is.na(SPP_info[["N"]]) & TABLO[[col]] != 0)
  average_value <- mean(SPP_info[["N"]][valid_rows], na.rm = TRUE)
  # Writing mean N-value to a table
  HEADER$N_mean[match(col, HEADER$New_plot_ID)] <- average_value
}

#--------------------------------------------------------------------------------------------#
# diversity indexes computation ####

# shannon index
shannon <- diversity(t(TABLO[2:ncol(TABLO)]), "shannon")

# evenness
specnumber_new <- specnumber(t(TABLO[2:ncol(TABLO)]))
evenness <- shannon/log(specnumber_new)

# Adding achieved data to the HEADER/ or rewriting previous data
HEADER$Shannon <- shannon            
HEADER$Specnumber <- specnumber_new # species appointment changed
HEADER$Evenness <- evenness          



which(HEADER$Evenness=="NaN") # plot 2095.Old KC has no value because the richness is 1 
TABLO$standardized_name[TABLO["2095.Old"]>0] # only Corynephorus canescens present

# replacing a NaN value in plot 2095.Old KC with 1, because the shannon diversity is 0 and therefore the evenness is 1 
HEADER$Evenness[HEADER$Evenness == 'NaN'] <- '1' 
HEADER$Evenness <- as.numeric(HEADER$Evenness)

# Adding cryptogam specnumber column
HEADER$Specnumber.crypt <- specnumber(t(TABLO[2:ncol(TABLO)][SPP_info$life_form=="cryptogam",]))

# Adding phanerogam specnumber column
HEADER$Specnumber.phan <- specnumber(t(TABLO[2:ncol(TABLO)][SPP_info$life_form!="cryptogam",]))


# log10 to SPP_info adding ####
SPP_info <- data.frame(SPP_info, canopy_log10 = log10(SPP_info$canopy_height), 
                       seed_mass_log10 = log10(SPP_info$seed_mass),
                       sla_log10 = log10(SPP_info$sla), 
                       dsev_log10 = log10(SPP_info$Disturbance.Severity.herblayer), 
                       check.names = F)                                 

# canopy_log10_mean
for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  valid_rows <- which(!is.na(SPP_info[["canopy_log10"]]) & TABLO[[col]] != 0)
  average_value <- mean(SPP_info[["canopy_log10"]][valid_rows], na.rm = TRUE)
  HEADER$canopy_log10_mean[match(col, HEADER$New_plot_ID)] <- average_value
}

# seed_mass_log10_mean
for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  valid_rows <- which(!is.na(SPP_info[["seed_mass_log10"]]) & TABLO[[col]] != 0)
  average_value <- mean(SPP_info[["seed_mass_log10"]][valid_rows], na.rm = TRUE)
  HEADER$seed_mass_log10_mean[match(col, HEADER$New_plot_ID)] <- average_value
}

# sla_log10_mean
for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  valid_rows <- which(!is.na(SPP_info[["sla_log10"]]) & TABLO[[col]] != 0)
  average_value <- mean(SPP_info[["sla_log10"]][valid_rows], na.rm = TRUE)
  HEADER$sla_log10_mean[match(col, HEADER$New_plot_ID)] <- average_value
}

# disturb_severity_mean
for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  valid_rows <- which(!is.na(SPP_info[["Disturbance.Severity.herblayer"]]) & TABLO[[col]] != 0)
  average_value <- mean(SPP_info[["Disturbance.Severity.herblayer"]][valid_rows], na.rm = TRUE)
  HEADER$disturb_sev_herb_mean[match(col, HEADER$New_plot_ID)] <- average_value
}

# # dsev_log10_mean
# for (col in colnames(TABLO)[2:ncol(TABLO)]) {
#   valid_rows <- which(!is.na(SPP_info[["dsev_log10"]]) & TABLO[[col]] != 0)
#   average_value <- mean(SPP_info[["dsev_log10"]][valid_rows], na.rm = TRUE)
#   HEADER$dsev_log10_mean[match(col, HEADER$New_plot_ID)] <- average_value
# }


#--------------------------------------------------------------------------------------------#
# calculating CWMs ####

# changing column names as F and T are very inconvenient for many operations

start_col <- which(colnames(SPP_info) == "L")
end_col <- which(colnames(SPP_info) == "N")

colnames(SPP_info)[start_col:end_col] <- 
  paste(colnames(SPP_info)[start_col:end_col], "Ell", sep = "_")

# creating the long table necessary for some further operations
SPP_info_short <- SPP_info[c("standardized_name","M_Ell","N_Ell","R_Ell" ,"L_Ell","T_Ell",
                             "canopy_height", "leaf_size", "seed_mass", 
                             "Disturbance.Severity.herblayer","Mowing.Frequency" ,
                             "Grazing.Pressure","Soil.Disturbance",
                             "Disturbance.Frequency.herblayer", 
                             "canopy_log10", "seed_mass_log10", 
                             "sla", "sla_log10")]

TABLO_long <- TABLO %>%
  pivot_longer(
    cols = -standardized_name, 
    names_to = "plot",  
    values_to = "cover" 
  )

# getting rid of zero rows
TABLO_long2 <- TABLO_long[!TABLO_long$cover==0,]

# merging with spp. info
TABLO_spp_info_short <- merge(TABLO_long2,SPP_info_short, by = "standardized_name",
                              all.x = T)

# Calculating CWM using dplyr and tidyr functions
summarize.nutnet.cwm <-       # New dataframe where we can inspect the result
  TABLO_spp_info_short %>%    # First step in the next string of statements
  group_by(plot) %>%          # Groups the summary file by Plot number
  summarize(                  # Coding for how we want our CWMs summarized

    canopy_height_cwm = weighted.mean(canopy_height, cover, na.rm = T),   # Actual calculation of CWMs 
    seed_mass_cwm = weighted.mean(seed_mass, cover, na.rm = T),
    disturb_sev_herb_cwm = weighted.mean(Disturbance.Severity.herblayer, cover, na.rm = T),
    mowing_cwm = weighted.mean(Mowing.Frequency, cover, na.rm = T),
    grazing_cwm = weighted.mean(Grazing.Pressure, cover, na.rm = T),
    soil_dist_cwm = weighted.mean(Soil.Disturbance, cover, na.rm = T),
    leaf_size_cwm = weighted.mean(leaf_size, cover, na.rm = T),
    disturbance_freq_herblayer_cwm = weighted.mean(Disturbance.Frequency.herblayer, cover, na.rm = T),

    log10_canopy_height_cwm = weighted.mean(canopy_log10, cover, na.rm = T),   
    log10_seed_mass_cwm = weighted.mean(seed_mass_log10, cover, na.rm = T),
    sla_cwm = weighted.mean(sla, cover, na.rm = T),
    log10_sla_cwm = weighted.mean(sla_log10, cover, na.rm = T)
  )

# adding cwms to the HEADER
HEADER <- merge(HEADER, summarize.nutnet.cwm, by.x = "New_plot_ID", by.y = "plot", all.x = T, all.y = T, sort = FALSE) 

#--------------------------------------------------------------------------------------------#
# looking for specialist species####
# EVCL -- European vegetation check list (Mucina et al., 2016)
EVCL2016 <- read.csv("attaching_files/EVCL2016.csv", sep = "\t", header = F)
colnames(EVCL2016) <- c("class", "species")


# the Festuco-Brometeae class specialists ####
spec_FES <- EVCL2016[EVCL2016$class=="FES" | EVCL2016$class=="TRI", ]

# some data preparations
spec_FES$species <- sub("\\s+$", "", spec_FES$species) # deleting possible spaces after species names
spec_FES$species <- gsub(" {2}", " ", spec_FES$species) # deleting double spaces between words
spec_FES$species <- gsub("\u00A0", " ", spec_FES$species) # replacing fixed spaces by usual ones
tools::showNonASCII(spec_FES$species) # looking for not standard symbols
spec_FES$species <- gsub("<ff>", " ", spec_FES$species) # deleting not standard symbols
spec_FES$species <- gsub("\n", "", spec_FES$species)  # replacing of the new row symbols by spaces
spec_FES$species <- gsub("\r", "", spec_FES$species)  # replacing of the return symbols by spaces 

# here a new frame is creating for our species list and a column with showing whether they are characteristic for FB
characteristic_species <- data.frame(characteristic_species=SPP_info$standardized_name)

# removing possible non-readable symbols
characteristic_species$characteristic_species <- gsub("\n", "", characteristic_species$characteristic_species)  
characteristic_species$characteristic_species <- gsub("\r", "", characteristic_species$characteristic_species)  

#-------------------------------------------------------------------------------------#
spec_FES$species <- gsub("ssp.", "subsp.", spec_FES$species)

# merging Festuco-Brometeae class characteristic species with our species
characteristic_species <- merge(characteristic_species, spec_FES, by.x = "characteristic_species", by.y="species", all.x = T)

# removing non-characteristic species/subspecies
characteristic_speciesFB <- characteristic_species[-which(is.na(characteristic_species$class)),]

#---------------------------------------------------------------------------------------#
# the Koelerio-Corynephoreteae class specialists ####
spec_COR <- EVCL2016[EVCL2016$class=="COR", ]

# some data preparations (the same as for FB)
spec_COR$species <- sub("\\s+$", "", spec_COR$species) 
spec_COR$species <- gsub(" {2}", " ", spec_COR$species) 
spec_COR$species <- gsub("\u00A0", " ", spec_COR$species) 
tools::showNonASCII(spec_COR$species)

spec_COR$species <- gsub("\n", "", spec_COR$species) 
spec_COR$species <- gsub("\r", "", spec_COR$species) 

# here a new frame is creating for our species list and a column with showing whether they are characteristic for kc
characteristic_species <- data.frame(characteristic_species=SPP_info$standardized_name)

# removing possible non-readable symbols
characteristic_species$characteristic_species <- gsub("\n", "", characteristic_species$characteristic_species)  
characteristic_species$characteristic_species <- gsub("\r", "", characteristic_species$characteristic_species)  

spec_COR$species <- gsub("ssp.", "subsp.", spec_COR$species)

# merging Festuco-Brometeae class characteristic species with our species
characteristic_species <- merge(characteristic_species, spec_COR, by.x = "characteristic_species", by.y="species", all.x = T)

# removing duplicated and non-characteristic species/subspecies
characteristic_speciesKC <- characteristic_species[-which(is.na(characteristic_species$class)),]

#------------------------------------------------------------------------------------#
# creating "specialists" column in SPPinfo ####
SPP_info$specialists <- ifelse(SPP_info$standardized_name %in% characteristic_speciesKC$characteristic_species & 
                                 SPP_info$standardized_name %in% characteristic_speciesFB$characteristic_species, "FB_KC",
                               ifelse(SPP_info$standardized_name %in% characteristic_speciesKC$characteristic_species, "KC",
                                      ifelse(SPP_info$standardized_name %in% characteristic_speciesFB$characteristic_species, "FB", NA)))

intersect(characteristic_speciesKC$characteristic_species,characteristic_speciesFB$characteristic_species)

# the mesophilic specialists ####
spec_mezophil <- EVCL2016[EVCL2016$class=="MOL", ]

# some data preparations
spec_mezophil$species <- sub("\\s+$", "", spec_mezophil$species) # deleting possible spaces after species names
spec_mezophil$species <- gsub(" {2}", " ", spec_mezophil$species) # deleting double spaces between words
spec_mezophil$species <- gsub("\u00A0", " ", spec_mezophil$species) # replacing fixed spaces by usual ones
tools::showNonASCII(spec_mezophil$species) # looking for not standard symbols
spec_mezophil$species <- gsub("<ff>", " ", spec_mezophil$species) # deleting not standard symbols
spec_mezophil$species <- gsub("\n", "", spec_mezophil$species)  # replacing of the new row symbols by spaces
spec_mezophil$species <- gsub("\r", "", spec_mezophil$species)  # replacing of the return symbols by spaces 

# here a new frame is creating for our species list and a column with showing whether they are characteristic for FB
characteristic_species <- data.frame(characteristic_species=SPP_info$standardized_name)

# removing possible non-readable symbols
characteristic_species$characteristic_species <- gsub("\n", "", characteristic_species$characteristic_species)  
characteristic_species$characteristic_species <- gsub("\r", "", characteristic_species$characteristic_species)  

#-------------------------------------------------------------------------------------#
spec_mezophil$species <- gsub("ssp.", "subsp.", spec_mezophil$species)

# merging Festuco-Brometeae class characteristic species with our species
characteristic_species <- merge(characteristic_species, spec_mezophil, by.x = "characteristic_species", by.y="species", all.x = T)

# removing non-characteristic species/subspecies
spec_mezophil <- characteristic_species[-which(is.na(characteristic_species$class)),]


# creating "specialists" column in SPPinfo ####
SPP_info$mezophilic <- ifelse(SPP_info$standardized_name %in% spec_mezophil$characteristic_species, "mezo", NA)

not_specialists <- intersect(SPP_info$standardized_name[!is.na(SPP_info$mezophilic)], SPP_info$standardized_name[!is.na(SPP_info$specialists)])

# Deleting of data on characteristic species of more than 1 class simultaneously
SPP_info$specialists[SPP_info$standardized_name %in% not_specialists] <- NA
SPP_info$mezophilic[SPP_info$standardized_name %in% not_specialists] <- NA

HEADER$MesoSpp <- NA
for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  sum_values <- 0  # start value of meso sp per plot
  for (j in 1:nrow(TABLO)) {
    if (!is.na(TABLO[j, col]) && 
        TABLO[j, col] != "0"  && 
        !is.na(SPP_info[j, "mezophilic"])) {
      sum_values <- sum_values + 1  # plus 1 if value is not "0" and do not NA
    }
  }
  HEADER$MesoSpp[match(col, HEADER$New_plot_ID)] <- sum_values
}

#------------------------------------------------------------------------------------#
# adding specialists per plot columns to HEADER ####

# creating "specialists" column in HEADER
HEADER$specialists <- rep(0, nrow(HEADER))

# summarizing and adding to HEADER cycle
for (col in colnames(TABLO)) {
  # searching for non-zero values (i.e., specialist species in SPP_info) and at the same time 
  # for non-zero values in the site releves (i.e., in TABLO)
  valid_rows <- which(!is.na(SPP_info$specialists) & TABLO[[col]] != 0)
  # summing up results
  sum_value <- sum(!is.na(SPP_info$specialists[valid_rows]))
  # writing sums down into HEADER
  HEADER$specialists[match(col, HEADER$New_plot_ID)] <- sum_value
}

#------------------------------------------------------------------------------------#
#FB-specialists ####
# creating "specialists" column in HEADER
HEADER$specialists_FB <- rep(0, nrow(HEADER))

# summarizing and writing down into HEADER
for (col in colnames(TABLO)) {
  valid_rows <- which((SPP_info$specialists=="FB" | SPP_info$specialists=="FB_KC") & TABLO[[col]] != 0)
  sum_value <- sum(!is.na(SPP_info$specialists[valid_rows]))
  HEADER$specialists_FB[match(col, HEADER$New_plot_ID)] <- sum_value
}

#------------------------------------------------------------------------------------------#
#KC-specialists ####
# creating "specialists" column in HEADER
HEADER$specialists_KC <- rep(0, nrow(HEADER))

# summarizing and writing down into HEADER
for (col in colnames(TABLO)) {
  valid_rows <- which((SPP_info$specialists=="KC" | SPP_info$specialists=="FB_KC") & TABLO[[col]] != 0)
  sum_value <- sum(!is.na(SPP_info$specialists[valid_rows]))
  HEADER$specialists_KC[match(col, HEADER$New_plot_ID)] <- sum_value
}

# -----------------------------------------------------------------------------------------#
library(ggplot2)
library(ggpubr)

# working with grazing matrix
# reordering HEADER rows to a more convenient view
row.names(HEADER) <- HEADER$New_plot_ID
HEADER <- HEADER[colnames(TABLO[2:ncol(TABLO)]),]
row.names(HEADER) <- c(1:nrow(HEADER))

#------------------------------------------------------------------------------# 
# grazing intensity (GI) part ####

HEADER$GI <- NA
HEADER$Toponym_short <- NA
HEADER <- HEADER[c(1:3, ncol(HEADER), 4:(ncol(HEADER)-1))]

#------------------------------------------------------------------------------# 
# Greiffenberg (Bahnlinie) ####

# toponym names unification
HEADER$Toponym_short[HEADER$Toponym=='Trockenstandorte beiderseits der Bahnlinie, 2,8km westlich von Greiffenberg' | 
                       HEADER$Toponym == 'Trockenstandorte beiderseits der Bahnlinie bei Greiffenberg' |
                       HEADER$Toponym == 'Trockenhänge, ehemalige Sandgruben und Ackerbrachen, 1,2km nordwestlich von Greiffenberg' |
                       HEADER$Toponym == 'ehemalige Sandgrube, Trockenhügel'] <- 
  'not_prot_Bahnlinie_Greiffenberg'

# checking spatial interlocation
plot(HEADER$Longitude[HEADER$Toponym_short=='not_prot_Bahnlinie_Greiffenberg'], 
     HEADER$Latitude[HEADER$Toponym_short=='not_prot_Bahnlinie_Greiffenberg'])

#------------------------------------------------------------------------------# 
# Mühlenberg ####

# toponym name unification
HEADER$Toponym_short[HEADER$Toponym=='GLB: "Mühlenberg"' | 
                       HEADER$Toponym == 'GLB "Mühlenberg"' |
                       HEADER$Toponym=='GLB Mühlenberg' |
                       HEADER$Toponym=='LB Mühlenberg'] <- "prot_Mühlenberg"



# grazing mean calculation on 2015-2017 with na.rm=T
HEADER$GI[HEADER$Toponym_short=="prot_Mühlenberg" ] <- 
  mean(nutz_tab$GI_2012_2017[which(nutz_tab$Site=="Mühlenberg"& 
                                            nutz_tab$Year %in% c("2015", "2016", "2017"))], na.rm=T)

HEADER$GI6[HEADER$Toponym_short=="prot_Mühlenberg" ] <-
  mean(nutz_tab$GI[which(nutz_tab$Site=="Mühlenberg")], na.rm=T)


#------------------------------------------------------------------------------# 
# Kleiner Rummelsberg ####

# toponym name unification
HEADER$Toponym_short[HEADER$Toponym=='GLB: "Kleiner Rummelsberg"' | 
                       HEADER$Toponym == 'GLB "Kleiner Rummelsberg"' |
                       HEADER$Toponym == 'GLB Kleiner Rummelsberg'] <- "prot_Kl_Rummelsberg"

# grazing mean calculation on 2015-2017 with na.rm=T
HEADER$GI[HEADER$Toponym_short=="prot_Kl_Rummelsberg"] <- 
  mean(nutz_tab$GI_2012_2017[which(nutz_tab$Site=="Kl. Rummelsberg" & 
                                            nutz_tab$Year %in% c("2015", "2016", "2017"))], na.rm=T)

HEADER$GI6[HEADER$Toponym_short=="prot_Kl_Rummelsberg" ] <-
  mean(nutz_tab$GI[which(nutz_tab$Site=="Kl. Rummelsberg")], na.rm=T)

#------------------------------------------------------------------------------# 
# Nordende_Wesensee ####

# spacial interrelation of plots
plot(HEADER$Longitude[c(13:15, 24:26)], HEADER$Latitude[c(13:15, 24:26)]) # lower points - newer

# toponym name unification
HEADER$Toponym_short[HEADER$Toponym=='Trockenhügel am Nordende des Wesensees' | 
                       HEADER$Toponym == 'Nordende Wesensee'] <- 'not_prot_Nordende_Wesensee'

#------------------------------------------------------------------------------# 
# Pimpinellenberg ####

# toponym name unification
HEADER$Toponym_short[HEADER$Toponym=='NSG "Pimpinellenberg" sowie angrenzende Flächen außerhalb des NSG'|
                       HEADER$Toponym == 'NSG "Pimpinellenberg" sowie angrenzende Trockenrasenflächen außerhalb des NSG'|
                       HEADER$Toponym == 'NSG Pimpinellenberg'|
                       HEADER$Toponym == 'Südhänge außerhalb des NSG Pimpinellenberg' |
                       HEADER$Toponym == 'Heckensaum nördlich des NSG Pimpinellenberg'] <- "prot_Pimpinellenberg"

# grazing mean calculation with na.rm=T. 3 years
HEADER$GI[HEADER$Toponym_short=="prot_Pimpinellenberg" & !(HEADER$Old_plot_ID %in% c("277", "281", "283", "284"))] <- 
  mean(nutz_tab$GI_2012_2017[which(nutz_tab$Site=="Pimpinellenberg"& 
                                            nutz_tab$Year %in% c("2015", "2016", "2017"))], na.rm=T)

HEADER$GI6[HEADER$Toponym_short=="prot_Pimpinellenberg" & !(HEADER$Old_plot_ID %in% c("277", "281", "283", "284"))] <-
  mean(nutz_tab$GI[which(nutz_tab$Site=="Pimpinellenberg")], na.rm=T)

#------------------------------------------------------------------------------# 
# Gabower_Hangkante ####

# toponym name unification
HEADER$Toponym_short[HEADER$Toponym=='Randhänge des Oderbruchs zwischen Gabow und Altglietzen (Granitberg u.a.)' | 
                       HEADER$Toponym == 'Trockenhänge zwischen Gabow und Altglietzen'] <- "prot_Gabower_Hangkante"

# grazing mean calculation on 2015-2017 with na.rm=T
HEADER$GI[HEADER$Toponym_short=="prot_Gabower_Hangkante"] <- 
  mean(nutz_tab$GI_2012_2017[which(nutz_tab$Site=="Gabower Hangkante"& 
                                            nutz_tab$Year %in% c("2015", "2016", "2017"))], na.rm=T)

HEADER$GI6[HEADER$Toponym_short=="prot_Gabower_Hangkante" ] <-
  mean(nutz_tab$GI[which(nutz_tab$Site=="Gabower Hangkante")], na.rm=T)

# spacial interrelation of plots
plot(HEADER$Longitude[c(42:53,132)], HEADER$Latitude[c(42:53,132)])

#------------------------------------------------------------------------------# 
# National Park “Unteres Odertal” (NSG Krähen- und Jungfernberge) #### 

# coordinates: NSG Krähen - 52.957382 14.101062; Jungfernberge - 52.95853 14.093976
# we believe Krähen is to the south-east from Jungfernberge
plot(HEADER$Longitude[34:41], HEADER$Latitude[34:41]) # old plots
plot(HEADER$Longitude[123:130], HEADER$Latitude[123:130]) # new plots

# spacial interrelation of plots
# Schäferberge is a part of the Natura 2000 site and national 
# park "Unteres Odertal" in the district "Uckermark", but we decided to consider Schäferberge separately 
# due to its spatial distance from other sites 
# ('Schäferberge (bei Gartzer Schrey)' and 'NP "Unteres Odertal": Odertalrandhänge zwischen 
# Gartz und dem Gartzer Schrey' -- topright)
plot(HEADER$Longitude[which(HEADER$Toponym %in% c(
  'NP "Unteres Odertal": Krähen- und jungfernberge', 
  'NSG Krähen- und Jungfernberge',
  'NP "Unteres Odertal": Krähen- und jungfernberge',
  'NSG Krähen- und Jungfernberge', 
  'NP "Unteres Odertal": Odertalrandhänge zwischen Gartz und dem Gartzer Schrey',
  'Schäferberge (bei Gartzer Schrey)')
)],
HEADER$Latitude[which(HEADER$Toponym %in% c(
  'NP "Unteres Odertal": Krähen- und jungfernberge', 
  'NSG Krähen- und Jungfernberge',
  'NP "Unteres Odertal": Krähen- und jungfernberge',
  'NSG Krähen- und Jungfernberge', 
  'NP "Unteres Odertal": Odertalrandhänge zwischen Gartz und dem Gartzer Schrey',
  'Schäferberge (bei Gartzer Schrey)')
)]) 

# toponym name unification
HEADER$Toponym_short[HEADER$Latitude<52.958 & (HEADER$Toponym=='NP "Unteres Odertal": Krähen- und jungfernberge' | 
                                                 HEADER$Toponym == 'NSG Krähen- und Jungfernberge')] <- 
  "prot_Krähen- & Jungfernberge TF Enzian"

HEADER$Toponym_short[HEADER$Latitude>52.958 & (HEADER$Toponym=='NP "Unteres Odertal": Krähen- und jungfernberge' | 
                                                 HEADER$Toponym == 'NSG Krähen- und Jungfernberge') ] <-
  "prot_Krähen- & Jungfernberge TF Jungfernspitze"

# grazing mean calculation on 2015-2017 with na.rm=T
HEADER$GI[HEADER$Toponym_short=="prot_Krähen- & Jungfernberge TF Enzian"] <- 
  mean(nutz_tab$GI_2012_2017[which(nutz_tab$Site=="Krähen- & Jungfernberge TF Enzian"& 
                                            nutz_tab$Year %in% c("2015", "2016", "2017"))], na.rm=T)

HEADER$GI[HEADER$Toponym_short=="prot_Krähen- & Jungfernberge TF Jungfernspitze"] <- 
  mean(nutz_tab$GI_2012_2017[which(nutz_tab$Site=="Krähen- & Jungfernberge TF Jungfernspitze"& 
                                            nutz_tab$Year %in% c("2015", "2016", "2017"))], na.rm=T)

HEADER$GI6[HEADER$Toponym_short=="prot_Krähen- & Jungfernberge TF Enzian" ] <-
  mean(nutz_tab$GI[which(nutz_tab$Site=="Krähen- & Jungfernberge TF Enzian")], na.rm=T)
HEADER$GI6[HEADER$Toponym_short=="prot_Krähen- & Jungfernberge TF Jungfernspitze" ] <-
  mean(nutz_tab$GI[which(nutz_tab$Site=="Krähen- & Jungfernberge TF Jungfernspitze")], na.rm=T)

#------------------------------------------------------------------------------# 
# National Park “Unteres Odertal” Schäferberge (bei Gartzer Schrey) #### 

# toponym name unification
HEADER$Toponym_short[HEADER$Toponym == 'NP "Unteres Odertal": Odertalrandhänge zwischen Gartz und dem Gartzer Schrey' |
                       HEADER$Toponym == 'Schäferberge (bei Gartzer Schrey)'] <- "prot_Schäferberge"

#------------------------------------------------------------------------------# 
# Geesower Hügel ####

# there were no information on one plot toponym which obviously should be Geesower (basing on spacial interrelations)
which(is.na(HEADER$Toponym))
HEADER$Toponym[which(is.na(HEADER$Toponym))] <- "NSG Geesower Hügel"

# toponym name unification
HEADER$Toponym_short[HEADER$Toponym=='NSG "Geesower Hügel"' | 
                       HEADER$Toponym == 'NSG Geesower Hügel' |
                       HEADER$Toponym == 'NSG Gessower Hügel'] <- "prot_Geesower_Hügel"

# grazing mean calculation on 2015-2017 with na.rm=T
HEADER$GI[HEADER$Toponym_short=="prot_Geesower_Hügel"] <- 
  mean(nutz_tab$GI_2012_2017[which(nutz_tab$Site=="Geesower Hügel"& 
                                            nutz_tab$Year %in% c("2015", "2016", "2017"))], na.rm=T)

HEADER$GI6[HEADER$Toponym_short=="prot_Geesower_Hügel" ] <-
  mean(nutz_tab$GI[which(nutz_tab$Site=="Geesower Hügel")], na.rm=T)

#------------------------------------------------------------------------------# 
# Kanonen- und Schloßberg, Schäfergrund ####

# spacial interrelation of plots
plot(HEADER$Longitude[72:80], HEADER$Latitude[72:80]) # old plots
plot(HEADER$Longitude[161:169], HEADER$Latitude[161:169]) # new plots

# toponym name unification
HEADER$Toponym_short[HEADER$Latitude>52.825 & (HEADER$Toponym=='NSG "Kanonen- und Schloßberg, Schäfergrund": Nördliche Teilfläche' | 
                                                 HEADER$Toponym == 'NSG: "Kanonen- und Schloßberg, Schäfergrund": nördliche Teilfläche' |
                                                 HEADER$Toponym == 'NSG Kanonen- & Schlossberg'|
                                                 HEADER$Toponym == 'NSG "Kanonen- und Schloßberg, Schäfergrund: nördliche Teilfläche')] <- 
  "prot_Kanonenberg"
HEADER$Toponym_short[HEADER$Latitude<52.825 & (HEADER$Toponym=='NSG "Kanonen- und Schloßberg, Schäfergrund": Nördliche Teilfläche' | 
                                                 HEADER$Toponym == 'NSG: "Kanonen- und Schloßberg, Schäfergrund": nördliche Teilfläche' |
                                                 HEADER$Toponym == 'NSG Kanonen- & Schlossberg'|
                                                 HEADER$Toponym == 'NSG "Kanonen- und Schloßberg, Schäfergrund: nördliche Teilfläche')] <- 
  "prot_Schloßberg"

# grazing mean calculation on 2015-2017 with na.rm=T
HEADER$GI[HEADER$Latitude>52.825 & HEADER$Toponym_short=="prot_Kanonenberg"] <- 
  mean(nutz_tab$GI_2012_2017[which(nutz_tab$Site=="Kanonenberg"& 
                                            nutz_tab$Year %in% c("2015", "2016", "2017"))], na.rm=T)

HEADER$GI[HEADER$Latitude<52.825 & HEADER$Toponym_short=="prot_Schloßberg"] <- 
  mean(nutz_tab$GI_2012_2017[which(nutz_tab$Site=="Schlossberg"& 
                                            nutz_tab$Year %in% c("2015", "2016", "2017"))], na.rm=T)

HEADER$GI6[HEADER$Toponym_short=="prot_Kanonenberg" ] <-
  mean(nutz_tab$GI[which(nutz_tab$Site=="Kanonenberg")], na.rm=T)
HEADER$GI6[HEADER$Toponym_short=="prot_Schloßberg" ] <-
  mean(nutz_tab$GI[which(nutz_tab$Site=="Schlossberg")], na.rm=T)



get_consonants <- function(text) {
  text <- gsub("^(prot_|not_prot_)", "", text)
  consonants <- gsub("[aeiouäüöAEIOUÄÜÖ_]", "", toupper(text))
  return(substr(consonants, 1, 3))
}
HEADER$Top_3letters <- ifelse(!is.na(HEADER$GI6),
                              sapply(HEADER$Toponym_short, get_consonants), NA)


HEADER$GI_Toponym <- ifelse(!is.na(HEADER$GI6),
                            paste0(
                              round(HEADER$GI, 2), 
                              ", ", 
                              gsub("_", " ", sub("^prot_", "", HEADER$Top_3letters))
                            ), NA
)

# not protected sites:
#------------------------------------------------------------------------------# 
# Schwedenschanze ####

# toponym name unification
HEADER$Toponym_short[HEADER$Toponym=='Bahnböschung und Schwedenschanze, 1,5km östlich von Lossow' | 
                       HEADER$Toponym == 'Schwedenschanze'] <- 'not_prot_Schwedenschanze'

#------------------------------------------------------------------------------# 
# Westnordwestlich_von_Flieth ####

# toponym name unification
HEADER$Toponym_short[HEADER$Toponym =='3 Trockenhügel am Mühlgraben, 2,0km westnordwestlich von Flieth' | 
                       HEADER$Toponym == 'Trockenhügel zwischen Flieth & Kaakstedt'] <- 
  'not_prot_westnordwestlich_von_Flieth'

# spacial interrelation of plots ####
# Westnordwestlich_von_Flieth is blue
ggplot(HEADER[1:178,], aes(Longitude, Latitude)) +
  geom_point(aes(color = ifelse(Old_plot_ID %in% c(92/398, 398), "red", "blue"))) +
  scale_color_manual(values = c("red", "blue"), name = "Group")

HEADER$Toponym_short <- as.factor(HEADER$Toponym_short) 
ggplot(HEADER[1:178,], aes(Longitude, Latitude)) +
  geom_point(aes(color = Toponym_short, size=2)) +
  scale_color_manual(values = c("red", "blue", "darkred", "darkblue","green", "darkgreen","yellow",
                                "gray", "violet", "darkviolet", "aquamarine","orange", "goldenrod",
                                "steelblue"), name = "Group")+
  theme_pubr() +
  guides(color = guide_legend(override.aes = list(size = 6)))


range(HEADER$GI6, na.rm = T)


#------------------------------------------------------------------------------#
# unification of KC toponyms

HEADER$Toponym_short <- as.character(HEADER$Toponym_short)

HEADER[179:314,]$Toponym_short <- ifelse(HEADER[179:314,]$Toponym=="Baumberge (Heiligensee)", "prot_Baumberge",
                                         ifelse(HEADER[179:314,]$Toponym=="Ehemalige Sandgrube am Rand des Odertales, 1,4 km 
                                  südöstlich von Wüste Kunersdorf", "not_prot_Wüste_Kunersdorf",
                                                ifelse(HEADER[179:314,]$Toponym=='FND "Pätzer Tongrube"', "not_prot_Pätzer_Tongrube",                                             # did not find, need for checking
                                                       ifelse(HEADER[179:314,]$Toponym=="Gehölzfreie Steilhänge östlich von Niederlehme am 
                                  Rand der Kiesgrube", "not_prot_Niederlehme",
                                                              ifelse(HEADER[179:314,]$Toponym=='GLB "Mühlenberg"', "prot_Mühlenberg",
                                                                     ifelse(HEADER[179:314,]$Toponym=="Krogberg", "prot_Krogberg",
                                                                            ifelse(HEADER[179:314,]$Toponym=='NSG "Biesenthaler Becken": Trockenrasen am Weg von 
                                  der Langerönner Mühle nach Biesenthal', "prot_Biesenthaler_Becken", 
                                                                                   ifelse(HEADER[179:314,]$Toponym=='NSG "Binnendüne Waltersberge"', 
                                                                                          "prot_Binnendüne_Waltersberge",
                                                                                          ifelse(HEADER[179:314,]$Toponym=="NSG \"Klobichsee\": Sandtrockenrasen im Wald, 
                                  1,0 km nordwestlich von Münchehofe (Flugsanddüne, Waldränder, Waldschneise 
                                  unter Stromleitung)", "prot_NSG_'Klobichsee'",
                                                                                                 ifelse(HEADER[179:314,]$Toponym=='NSG "Lange Dammwiesen und Unteres Annatal"', 
                                                                                                        "prot_NSG_Lange_Dammwiesen_und_Unteres_Annatal",
                                                                                                        ifelse(HEADER[179:314,]$Toponym=='NSG "Oderberge": Südliche Teilfläche bei der 
                                  Bruckmühle', "prot_NSG_'Oderberge'",
                                                                                                               ifelse(HEADER[179:314,]$Toponym=='NSG "Pätzer Kiesgrube" sowie angrenzende 
                                  Trockenstandorte', "prot_NSG_'Pätzer Kiesgrube'",
                                                                                                                      ifelse(HEADER[179:314,]$Toponym=='NSG "Püttberge (Wilhelmshagen-Woltersdorfer 
                                  Dünenzug)"', "prot_NSG_'Püttberge_(Wilhelmshagen-Woltersdorfer_Dünenzug)'",
                                                                                                                             ifelse(HEADER[179:314,]$Toponym=="Schiefer Berg", "not_prot_Schiefer_Berg",                                                            # could not find, need for checking
                                                                                                                                    ifelse(HEADER[179:314,]$Toponym=="Trockenhänge und Wegränder nördlich der Straße 
                                  Steinhöfel - Peetzig, 1,3 km nordnordöstlich von Peetzig", 
                                                                                                                                           "not_prot_Straße_Steinhöfel-Peetzig,nördlich",
                                                                                                                                           ifelse(HEADER[179:314,]$Toponym=="Trockenrasen auf der Ostseite der Bahnstrecke 
                                  zwischen Bahnhof Eisenhüttenstadt und Haltepunkt Vogelsang", 
                                                                                                                                                  "not_prot_zwischen_Bahnhof_Eisenhüttenstadt_und_Haltepunkt_Vogelsang",
                                                                                                                                                  ifelse(HEADER[179:314,]$Toponym=="Trockenstandorte beiderseits der Bahnlinie, 
                                  2,8 km westlich von Greiffenberg", "not_prot_Bahnlinie,_westlich_von_Greiffenberg", # the same as in FB
                                                                                                                                                         ifelse(HEADER[179:314,]$Toponym=="Trockenstandorte im Bereich des ehemaligen 
                                  Sportplatzes unterhalb des Schlesingsberges", "not_prot_unterhalb_des_Schlesingsberges",
                                                                                                                                                                ifelse(HEADER[179:314,]$Toponym=="Trockenstandorte in Hoppegarten nördlich der 
                                  B 1(Kiesgrube, Waldränder, Brachen am Rand des Roten Luches)", "not_prot_in_Hoppegarten,nördlich",
                                                                                                                                                                       ifelse(HEADER[179:314,]$Toponym=="Trockenstandorte in Hoppegarten südlich der 
                                  B 1(Sandtrockenrasen, Ackerbrachen)", "not_prot_in_Hoppegarten,südlich", 
                                                                                                                                                                              ifelse(HEADER[179:314,]$Toponym=="NSG \"Biesenthaler Becken\": Trockenrasen am Weg von der Langerönner Mühle nach Biesenthal", 
                                                                                                                                                                                     "prot_NSG_Biesenthaler_Becken", 
                                                                                                                                                                                     ifelse(HEADER[179:314,]$Toponym=="NSG \"Püttberge (Wilhelmshagen-Woltersdorfer Dünenzug)\"", "prot_NSG_Püttberge",  
                                                                                                                                                                                            ifelse(HEADER[179:314,]$Toponym=="Ehemalige Sandgrube am Rand des Odertales, 1,4 km südöstlich von Wüste Kunersdorf", 
                                                                                                                                                                                                   "not_prot_Sandgrube_von_Wüste_Kunersdorf",  
                                                                                                                                                                                                   ifelse(HEADER[179:314,]$Toponym=="NSG \"Pätzer Kiesgrube\" sowie angrenzende Trockenstandorte",
                                                                                                                                                                                                          "prot_NSG_Pätzer_Kiesgrube",                                                 
                                                                                                                                                                                                          ifelse(HEADER[179:314,]$Toponym=="NSG \"Klobichsee\": Sandtrockenrasen im Wald, 1,0 km nordwestlich von Münchehofe (Flugsanddüne, Waldränder, Waldschneise unter Stromleitung)",
                                                                                                                                                                                                                 "prot_NSG_Klobichsee", 
                                                                                                                                                                                                                 ifelse(HEADER[179:314,]$Toponym=="NSG \"Oderberge\": Südliche Teilfläche bei der Bruckmühle",
                                                                                                                                                                                                                        "prot_NSG_Oderberge", 
                                                                                                                                                                                                                        ifelse(HEADER[179:314,]$Toponym=="Gehölzfreie Steilhänge östlich von Niederlehme am Rand der Kiesgrube",
                                                                                                                                                                                                                               "not_prot_östlich_von_Niederlehme", 
                                                                                                                                                                                                                               ifelse(HEADER[179:314,]$Toponym=="Trockenstandorte beiderseits der Bahnlinie, 2,8 km westlich von Greiffenberg",
                                                                                                                                                                                                                                      "not_prot_Bahnlinie_westlich_von_Greiffenberg", 
                                                                                                                                                                                                                                      ifelse(HEADER[179:314,]$Toponym=="Trockenhänge und Wegränder nördlich der Straße Steinhöfel - Peetzig, 1,3 km nordnordöstlich von Peetzig",
                                                                                                                                                                                                                                             "not_prot_Straße_nordnordöstlich_von_Peetzig",                                                  
                                                                                                                                                                                                                                             ifelse(HEADER[179:314,]$Toponym=="Trockenstandorte in Hoppegarten nördlich der B 1(Kiesgrube, Waldränder, Brachen am Rand des Roten Luches)",
                                                                                                                                                                                                                                                    "not_prot_Hoppegarten_nördlich_der_B1",                                                  
                                                                                                                                                                                                                                                    ifelse(HEADER[179:314,]$Toponym=="Trockenstandorte in Hoppegarten südlich der B 1(Sandtrockenrasen, Ackerbrachen)",
                                                                                                                                                                                                                                                           "not_prot_Hoppegarten_südlich_der_B1",                                                  
                                                                                                                                                                                                                                                           ifelse(HEADER[179:314,]$Toponym=="Trockenstandorte im Bereich des ehemaligen Sportplatzes unterhalb des Schlesingsberges",
                                                                                                                                                                                                                                                                  "not_prot_Sportplatzes_Schlesingsberges",                                                  
                                                                                                                                                                                                                                                                  ifelse(HEADER[179:314,]$Toponym=="Trockenrasen auf der Ostseite der Bahnstrecke zwischen Bahnhof Eisenhüttenstadt und Haltepunkt Vogelsang",
                                                                                                                                                                                                                                                                         "not_prot_Eisenhüttenstadt_Vogelsang",                                                  
                                                                                                                                                                                                                                                                         "no"
                                                                                                                                                                                                                                                                  )))))))))))))))))))))))))))))))))                                                 

#------------------------------------------------------------------------------#

HEADER$protection <- NA
HEADER$protection <- ifelse(grepl("^prot", HEADER$Toponym_short), "protected",
                            ifelse(grepl("^not_", HEADER$Toponym_short), "unprotected", NA))

HEADER$protection <- as.factor(HEADER$protection)

table(HEADER$protection, HEADER$class)

#------------------------------------------------------------------------------#
# red list part ####
Red_list_new <- as.data.frame(read_xlsx("Input_data/Red_list_new.xlsx")) 

TABLO$standardized_name[TABLO$standardized_name=="Medicago ×varia"] <- 
  "Medicago x varia"

SPP_info$standardized_name[SPP_info$standardized_name=="Medicago ×varia"] <- 
  "Medicago x varia"

any(SPP_info$standardized_name==Red_list_new$standardized_name) # check if names are the same
SPP_info$red_cat <- Red_list_new$red_cat


#-----------------------------------------------------------------------------------------#
# red lichen sub part ####
lichens <- read.csv("attaching_files/lichens.csv", header = TRUE, sep = ";", check.names = FALSE)
colnames(lichens) <- c("text")

lichens_split <- lichens %>%
  mutate(
    col1 = sub("^(\\S+)\\s.*", "\\1", text), 
    col2 = ifelse(
      grepl("subsp\\.", text),
      sub("^\\S+\\s((\\S+\\s\\S+\\s\\S+\\s\\S+)).*", "\\1", text), # choosing 4 words if there is "subsp."
      sub("^\\S+\\s((\\S+\\s\\S+)).*", "\\1", text) # choosing 2 words if there is no "subsp." 
    ),
    col3 = sub("^\\S+\\s(\\S+\\s\\S+\\s\\S+\\s\\S+\\s)?(.*)", "\\2", text) # the rest
  ) %>%
  dplyr::select(col1, col2, col3) 


colnames(lichens_split) <- c("RedCode", "species", "miscellanious")
red_sp_lichens <- lichens_split[lichens_split$species %in% SPP_info$standardized_name &
                                  lichens_split$RedCode %in% c("0", "1", "2", "3"),]
red_sp_lichens

SPP_info$red_cat[SPP_info$standardized_name=="Cladonia cariosa"] <- "3 - Endangered"
SPP_info$red_cat[SPP_info$standardized_name=="Cladonia cornuta"] <- "2 - Critically Endangered"
SPP_info$red_cat[SPP_info$standardized_name=="Cladonia rangiferina"] <- "2 - Critically Endangered"
SPP_info$red_cat[SPP_info$standardized_name=="Cladonia rangiformis"] <- "3 - Endangered"
SPP_info$red_cat[SPP_info$standardized_name=="Peltigera canina"] <- "2 - Critically Endangered"
SPP_info$red_cat[SPP_info$standardized_name=="Peltigera praetextata"] <- "3 - Endangered"

# removing invisible symbols
SPP_info$red_cat <- gsub("\u00A0", " ", SPP_info$red_cat)  # Замінюємо Unicode-пробіли

# deleting ambiguous categories (for clearer counting and more strict results)
SPP_info$red_cat[SPP_info$red_cat %in% c("D - Data not sufficient for a risk assessment",
                                         "G - Endangered without any exact category",
                                         "V - advance warning level")] <- NA
SPP_info$red_cat <- factor(SPP_info$red_cat)
table(SPP_info$red_cat)


# attaching column with number of Red species of 0-3 categories in each plot to the header  
HEADER$red_sp <- NA

for (col in colnames(TABLO)[2:ncol(TABLO)]) {
  sum_values <- 0  # start value of red sp per plot
  for (j in 1:nrow(TABLO)) {
    if (!is.na(TABLO[j, col]) && !is.na(SPP_info[j, "red_cat"]) && TABLO[j, col] != "0") {
      sum_values <- sum_values + 1  # plus 1 if value is not "0" and do not NA
    }
  }
  HEADER$red_sp[match(col, HEADER$New_plot_ID)] <- sum_values
}


#-------------------------------------------------------------------------------#
site_to_toponym <- c("Mühlenberg" = "prot_Mühlenberg", 
                     "Kl. Rummelsberg" = "prot_Kl_Rummelsberg",
                     "Pimpinellenberg"="prot_Pimpinellenberg",
                     "Geesower Hügel"="prot_Geesower_Hügel",
                     "Krähen- & Jungfernberge TF Jungfernspitze"="prot_Krähen- & Jungfernberge TF Jungfernspitze",
                     "Krähen- & Jungfernberge TF Enzian"="prot_Krähen- & Jungfernberge TF Enzian",
                     "Gabower Hangkante"="prot_Gabower_Hangkante",
                     "Kanonenberg"="prot_Kanonenberg",
                     "Schlossberg"="prot_Schloßberg",
                     "Gabower Hangkante"="prot_Gabower_Hangkante"
)

years <- 1993:2017

for (year in years) {
  col_name <- paste0("y", year)
  HEADER[[col_name]] <- NA
  for (toponym in unique(HEADER$Toponym_short)) {
    matching_site <- names(site_to_toponym[site_to_toponym == toponym])
    if (length(matching_site) > 0) {
      matching_site <- matching_site[1]
      gve_value <- nutz_tab$GI_2012_2017[nutz_tab$Site == matching_site & nutz_tab$Year == year]
      if (length(gve_value) > 0) {
        HEADER[[col_name]][HEADER$Toponym_short == toponym] <- gve_value
      }
    }
  }
}


#-----------------------------------------------------------------------------------#
# alliances ####
HEADER$Alliance[which(is.na(HEADER$Alliance))] <- HEADER$Alliance[HEADER$class=="KC" & HEADER$plot_age=="Old"]

HEADER$Alliance <- ifelse(HEADER$Alliance=="C", "Corynephorion canescentis",
                          ifelse(HEADER$Alliance== "A", "Armerion elongatae",
                                 ifelse(HEADER$Alliance=="K", "Koelerion glaucae", 
                                        ifelse(HEADER$Alliance=="CBP", "Cirsio-Brachypodion pinnati",    
                                               ifelse(HEADER$Alliance=="FV", "Festucion valesciacae",
                                                      ifelse(HEADER$Alliance=="GS", "Geranion sanguinei",
                                                             "3"))))))


table(HEADER$plot_age, HEADER$Alliance)

#--------------------------------------------------------------------------------------#
# shares of vasculars which has trait values

length(SPP_info$standardized_name[SPP_info$life_form != "cryptogam" & !is.na(SPP_info$canopy_height)])/
  length(SPP_info$standardized_name[SPP_info$life_form != "cryptogam"])*100

length(SPP_info$standardized_name[SPP_info$life_form != "cryptogam" & !is.na(SPP_info$seed_mass)])/
  length(SPP_info$standardized_name[SPP_info$life_form != "cryptogam"])*100

length(SPP_info$standardized_name[SPP_info$life_form != "cryptogam" & !is.na(SPP_info$sla)])/
  length(SPP_info$standardized_name[SPP_info$life_form != "cryptogam"])*100


range(HEADER$Specnumber[HEADER$class=="FB"])
range(HEADER$Specnumber[HEADER$class=="KC"])
