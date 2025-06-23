
# Intro ####

# Loading and processing the dataset
#--------------------------------------------------------------------------------------------# 
# Attaching necessary packages#### 
if (!require(pacman)) install.packages(pacman)
pacman::p_load(readxl,labdsv,dplyr)

#--------------------------------------------------------------------------------------------#      
# Data loading and refining #### 
# KC part ####
# reading Koelerio-Corynephoretea file
Koelerio_Corynephoreteae_data <- as.data.frame(read_xlsx("Input_data/ReSurveyEurope_Dengler_DE_Brandenburg_Koelerio-Corynephoretea.xlsx", 
                                                         sheet = "Species and header", col_names = TRUE))

# dividing into header and vegetation data parts
header_Koelerio_Corynephoreteae <- Koelerio_Corynephoreteae_data[1:31,]
Koelerio_Corynephoreteae_data <- Koelerio_Corynephoreteae_data[32:nrow(Koelerio_Corynephoreteae_data),]

# transposing of the header, renaming header columns, deleting rubbish rows
header_Koelerio_Corynephoreteae <- as.data.frame(t(header_Koelerio_Corynephoreteae))
colnames(header_Koelerio_Corynephoreteae) <- header_Koelerio_Corynephoreteae[1,]
header_Koelerio_Corynephoreteae <- header_Koelerio_Corynephoreteae[-c(1:2),]

# working on the plot_age categorical column in the header (to unify names for surveys)
header_Koelerio_Corynephoreteae$plot_age <- header_Koelerio_Corynephoreteae$`Date [Turboveg]`
header_Koelerio_Corynephoreteae$plot_age <- gsub("^1997.*", "Old", header_Koelerio_Corynephoreteae$plot_age)
header_Koelerio_Corynephoreteae$plot_age <- gsub("^1996.*", "Old", header_Koelerio_Corynephoreteae$plot_age)
header_Koelerio_Corynephoreteae$plot_age <- gsub("^1993.*", "Old", header_Koelerio_Corynephoreteae$plot_age)
header_Koelerio_Corynephoreteae$plot_age <- gsub("^2017.*", "New", header_Koelerio_Corynephoreteae$plot_age)

# sorting vegetation data according to alphabetical species order
Koelerio_Corynephoreteae_data <- Koelerio_Corynephoreteae_data[order(Koelerio_Corynephoreteae_data$Sequential.Plot.ID),]
koel_spp <- Koelerio_Corynephoreteae_data$Sequential.Plot.ID
# deleting species names and layer information (later corrected species names will be added)
Koelerio_Corynephoreteae_data <- Koelerio_Corynephoreteae_data[-c(1,2)]

# loading corrected species names
KC_spec_names <- as.data.frame(read_excel("Input_data/ReSurveyEurope_Dengler_DE_Brandenburg_Koelerio-Corynephoretea.xlsx", 
                                          sheet = "correctnames"))
colnames(KC_spec_names)[1:3] <- c("life_form", "Koel_Cor_orname", "standardized_name_new_aggr.")

# changing abundance scale from the Brown-Blanquet scale to percents ####
Koelerio_Corynephoreteae_data[is.na(Koelerio_Corynephoreteae_data)] <- 0
code <- c("0","r","+","1","2m","2a","2b","3","4","5", "v")
value <- c(0,0.1,0.5,1,3,10,20,37.5,62.5,87.5, 0.01)
Koelerio_Corynephoreteae_data <- abundtrans(Koelerio_Corynephoreteae_data,code,value)

#--------------------------------------------------------------------------------------------#    
# FB part ####
# reading Festuco-Brometea file
Festuco_Brometea_data <- as.data.frame(read_xlsx("Input_data/ReSurveyEurope_Dengler_DE_Brandenburg_Festuco-Brometea.xlsx", 
                                                 sheet = "Old and new plots", col_names = TRUE))
# dividing into header and vegetation data parts
header_Festuco_Brometea <- Festuco_Brometea_data[1:20,]
Festuco_Brometea_data <- Festuco_Brometea_data[21:nrow(Festuco_Brometea_data),]
# transposing of the header, renaming header columns, deleting rubbish rows
header_Festuco_Brometea <- as.data.frame(t(header_Festuco_Brometea))
colnames(header_Festuco_Brometea) <- header_Festuco_Brometea[1,]
header_Festuco_Brometea <- header_Festuco_Brometea[-1,]

# standardization of alliance names
header_Festuco_Brometea$Alliance <- ifelse(header_Festuco_Brometea$Alliance=="CBp", "CBP",
                                           ifelse(header_Festuco_Brometea$Alliance=="CBP", "CBP",
                                                  ifelse(header_Festuco_Brometea$Alliance=="FV", "FV",
                                                         ifelse(header_Festuco_Brometea$Alliance=="Gs", "GS", "GS"))))

# loading corrected species names
FB_spec_names <- as.data.frame(read_excel("Input_data/ReSurveyEurope_Dengler_DE_Brandenburg_Festuco-Brometea.xlsx", 
                                          sheet = "correctnames"))

xx <- data.frame(FB_original=Festuco_Brometea_data$`Plot ID`, FB_orig_2=FB_spec_names$Fes_Brom_orname, FB_new=FB_spec_names$standardized_name_new_aggr.)
all(xx$FB_original==xx$FB_orig_2)
rm(xx)

Festuco_Brometea_data <- Festuco_Brometea_data[-1]
# Changing not numerical data ("v") into 0.01 in vegetation data table
Festuco_Brometea_data[which(Festuco_Brometea_data=="v", arr.ind = TRUE)] <- 0.01

# Changing column type into numeric in the vegetation data table
Festuco_Brometea_data[is.na(Festuco_Brometea_data)] <- 0
Festuco_Brometea_data <- as.data.frame(apply(Festuco_Brometea_data, 2, as.numeric))

#--------------------------------------------------------------------------------------------# 

# adding corrected species names to the vegetation dataframes ####
# FB
Festuco_Brometea_data <- cbind(FB_spec_names[c("standardized_name_new_aggr.","life_form")], Festuco_Brometea_data)

#KC
Koelerio_Corynephoreteae_data <- cbind(KC_spec_names[c("standardized_name_new_aggr.","life_form")], Koelerio_Corynephoreteae_data)

#--------------------------------------------------------------------------------------------# headers union
# header column names unification
names(header_Festuco_Brometea)
colnames(header_Festuco_Brometea) <- c("Old_plot_ID", "plot_age", "Toponym", "District",                  
                                       "Municipality",   "Longitude",   "Latitude", "Area_m2",            
                                       "Day", "Month", "Year", "Alliance",                  
                                       "Slope_aspect", "Slope_inclination", "Tree_layer", "Shrub_layer",
                                       "Herb_layer", "Moss_layer",  "Specnumber", "Specnumber_Phanero")           

names(header_Koelerio_Corynephoreteae)
colnames(header_Koelerio_Corynephoreteae) <- c("Old_plot_ID", "Toponym", "Municipality",                       
                                               "District", "Latitude", "Longitude",      
                                               "Precision_m", "Source_coordinate", "Area_m2",                     
                                               "Author", "Date [Turboveg]", "Aspect",                             
                                               "Aspect [°]", "Inklination [°]", "Inclination [%]",                    
                                               "Tree_layer", "Shrub_layer", "Height_shrub_layer_cm",            
                                               "Herb_layer", "Height_herb_layer_cm", "Moss_layer",          
                                               "Cover_litter [%]", "Specnumber", "Woddy_species_richness",             
                                               "Herbaceous_species_richness", "specnumber_crypt", "Association",                        
                                               "Alliance", "Vegetation_type", "Land_use",                           
                                               "Remarks", "plot_age")     

(common_names <- intersect(names(header_Koelerio_Corynephoreteae),names(header_Festuco_Brometea)))

# creation of the united  Festuco-Brometeae and Koelerio-Corynephoretea header (with common columns)
HEADER <- rbind(header_Festuco_Brometea[,common_names], header_Koelerio_Corynephoreteae[,common_names])


#--------------------------------------------------------------------------------------------#
# creation of the unique species name list
standname_FB <- Festuco_Brometea_data$standardized_name_new_aggr.
standname_KC <- Koelerio_Corynephoreteae_data$standardized_name_new_aggr.
standardized_name <- c(standname_FB, standname_KC)
standardized_name <- (aggregate(standardized_name, by=list(standardized_name), FUN=function(x) x[1]))[1]
colnames(standardized_name) <- "standardized_name"

#--------------------------------------------------------------------------------------------#
# Festuco-Brometeae data preparation (aggregation) ####

TABLO_a <- merge(standardized_name, Festuco_Brometea_data, by.x="standardized_name", 
                 by.y="standardized_name_new_aggr.", all.x = TRUE)

# aggregation by species names 
TABLO_A <- (cbind((aggregate(TABLO_a[1:2], 
                             by = list(TABLO_a$standardized_name), function(x) x[1]))[-1], 
                  (aggregate(TABLO_a[3:ncol(TABLO_a)], 
                             by = list(TABLO_a$standardized_name), sum))[-1]))

# alphabetical ordering
TABLO_A <- TABLO_A[ordered(TABLO_A$standardized_name),]

# Koelerio-Corynephoretea data preparation (aggregation) ####
TABLO_b <- merge(standardized_name,Koelerio_Corynephoreteae_data, by.x="standardized_name", 
                 by.y="standardized_name_new_aggr.", all.x = TRUE)

# aggregation by species names (percentage of synonymous/different layer species names)
TABLO_B <- (cbind((aggregate(TABLO_b[1:2], 
                             by = list(TABLO_b$standardized_name), function(x) x[1]))[-1], 
                  (aggregate(TABLO_b[3:ncol(TABLO_b)], 
                             by = list(TABLO_b$standardized_name), sum))[-1]))

# checking for identical species names order (in rows)
all(TABLO_B$standardized_name==TABLO_A$standardized_name)

#--------------------------------------------------------------------------------------------#

# creating united vegetation data table ####
TABLO <- cbind(TABLO_A[1:2], TABLO_B[2], TABLO_A[3:ncol(TABLO_A)], TABLO_B[3:ncol(TABLO_B)])

# life_form column forming
colnames(TABLO)[3] <- "life_form.1"
TABLO <- TABLO %>%
  mutate(life_form = coalesce(life_form, life_form.1)) %>%
  dplyr::select(-life_form.1)

# substitution of NAs by 0s
TABLO[, 3:ncol(TABLO)][is.na(TABLO[, 3:ncol(TABLO)])] <- 0

# checking for releve names identity
identical(rownames(HEADER), colnames(TABLO)[c(3:ncol(TABLO))])

# dividing TABLO dataframe into SPP_info dataframe (with the information on species) and a vegetation table (in the strict meaning)
SPP_info <- TABLO[1:2]
SPP_info$standardized_name[SPP_info$standardized_name %in% paste()]
(filtered_names <- SPP_info$standardized_name[grep(" sp\\.", SPP_info$standardized_name)])


TABLO <- TABLO[c(1,3:ncol(TABLO))]

# adding information on the time of surveys into the vegetation table (TABLO) column names
for (j in 2:ncol(TABLO)) {
  if (HEADER[j-1, "plot_age"] == "Old") {
    new_name <- paste0(HEADER[j-1,1], ".Old")
    colnames(TABLO)[j] <- new_name
  } else if (HEADER[j-1, "plot_age"] == "New") {
    new_name <- paste0(HEADER[j-1,1], ".New")
    colnames(TABLO)[j] <- new_name
  }
}

#--------------------------------------------------------------------------------------------#

# adding the class column and New_plot_ID with the information on the time of surveys into HEADER ####
HEADER <- data.frame(Old_plot_ID=HEADER$Old_plot_ID, New_plot_ID=paste(HEADER$Old_plot_ID, HEADER$plot_age, sep="."), 
                     class=c(rep("FB", 178), rep("KC",136)), 
                     HEADER[which(colnames(HEADER)=="Toponym"):which(colnames(HEADER)=="plot_age")])

# changing HEADER column types into the factor type
HEADER$Old_plot_ID <- as.factor(HEADER$Old_plot_ID)
HEADER$plot_age <- as.factor(HEADER$plot_age)
HEADER$class <- as.factor(HEADER$class)
HEADER$New_plot_ID <- as.factor(HEADER$New_plot_ID)

#--------------------------------------------------------------------------------------------#