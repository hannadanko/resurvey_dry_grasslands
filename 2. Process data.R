
# processing of the grazing management data
nutz_tab <- as.data.frame(read_xlsx("input_data/nutz_tab.xlsx")[1:60,])
colnames(nutz_tab)

colnames(nutz_tab) <- c("Year","Site","Area size [ha]", 
                             "ewes1", "sheep1","lambs (under 1 year)1","mother goats1","goats1","young goats1", 
                        "cows over 1 year old1","young cows under 1 year old1","donkeys1","ponies1","period 1","day number1", 
                             "ewes2", "sheep2","lambs (under 1 year)2","mother goats2","goats2","young goats2", 
                        "cows over 1 year old2","young cows under 1 year old2","donkeys2","ponies2","period 2","day number2",
                             "ewes3", "sheep3","lambs (under 1 year)3","mother goats3","goats3","young goats3",
                        "cows over 1 year old3","young cows under 1 year old3","donkeys3","ponies3","period 3","day number3",
                             "ewes4", "sheep4","lambs (under 1 year)4","mother goats4","goats4","young goats4",
                        "cows over 1 year old4","young cows under 1 year old4","donkeys4","ponies4","period 4","day number4",
                             "winter grazing", "grazer number", "day number5", 
                        "Grazing duration [d]", "Grazing courses", "Grazing period")

nutz_tab[c(4:13, 16:25, 28:37, 40:49)] <- sapply(nutz_tab[c(4:13, 16:25, 28:37, 40:49)], as.numeric)
nutz_tab[c(15, 27, 39, 51)] <- sapply(nutz_tab[c(15, 27, 39, 51)], as.numeric)

# to substitute NA by 0 (to denote that animals of the category were absent)
nutz_tab[c(4:13, 16:25, 28:37, 40:49, 53)] <- lapply(nutz_tab[c(4:13, 16:25, 28:37, 40:49, 53)], function(x) {
  x[is.na(x)] <- 0
  return(x)
})

nutz_tab[c(15, 27, 39, 51, 54)] <- lapply(nutz_tab[c(15, 27, 39, 51, 54)], function(x) {
  x[is.na(x)] <- 0
  return(x)
})



#-------------------------------------------------------------------------------------#
# LU were appointed according to the site: https://bravors.brandenburg.de/sixcms/media.php/66/Anlage_4.pdf
# LU for lambs and young goats gve from: https://bab.gv.at/index.php?option=com_rsfiles&task=rsfiles.download&path=Gruener_Bericht/2022/Tab_2022_6030003_Umrechnungsschluessel_landw_Nutztiere.xlsx&Itemid=514&lang=en

nutz_tab$GI_2012_2017 <- (
  
  # period 1    
  rowSums(cbind(
    nutz_tab$ewes1 *                              0.15, 
    nutz_tab$sheep1 *                             0.1, 
    nutz_tab$`lambs (under 1 year)1` *            0.07,
    nutz_tab$goats1 *                             0.15, 
    nutz_tab$`young goats1` *                     0.07, 
    nutz_tab$`cows over 1 year old1` *            1, 
    nutz_tab$`young cows under 1 year old1` *     0.6, 
    nutz_tab$donkeys1 *                           1, 
    nutz_tab$ponies1 *                            1
  ), na.rm = TRUE) * nutz_tab$`day number1` +
    
    # period 2      
    rowSums(cbind(
      nutz_tab$ewes2 *                            0.15, 
      nutz_tab$sheep2 *                           0.1, 
      nutz_tab$`lambs (under 1 year)2` *          0.07,
      nutz_tab$goats2 *                           0.15, 
      nutz_tab$`young goats2` *                   0.07, 
      nutz_tab$`cows over 1 year old2` *          1, 
      nutz_tab$`young cows under 1 year old2` *   0.6, 
      nutz_tab$donkeys2 *                         1, 
      nutz_tab$ponies2 *                          1
    ), na.rm = TRUE) * nutz_tab$`day number2` +
    
    # period 3          
    rowSums(cbind(
      nutz_tab$ewes3 *                            0.15, 
      nutz_tab$sheep3 *                           0.1, 
      nutz_tab$`lambs (under 1 year)3` *          0.07,
      nutz_tab$goats3 *                           0.15, 
      nutz_tab$`young goats3` *                   0.07, 
      nutz_tab$`cows over 1 year old3` *          1, 
      nutz_tab$`young cows under 1 year old3` *   0.6, 
      nutz_tab$donkeys3 *                         1, 
      nutz_tab$ponies3 *                          1
    ), na.rm = TRUE) * nutz_tab$`day number3` +
    
    # period 4        
    rowSums(cbind(
      nutz_tab$ewes4 *                            0.15, 
      nutz_tab$sheep4 *                           0.1, 
      nutz_tab$`lambs (under 1 year)4` *          0.07,
      nutz_tab$goats4 *                           0.15, 
      nutz_tab$`young goats4` *                   0.07, 
      nutz_tab$`cows over 1 year old4` *          1, 
      nutz_tab$`young cows under 1 year old4` *   0.6, 
      nutz_tab$donkeys4 *                         1, 
      nutz_tab$ponies4 *                          1
    ), na.rm = TRUE) * nutz_tab$`day number4` +
    
    # winter grazing   
    nutz_tab$`grazer number` *                    1 *  
    nutz_tab$`day number5`
  
) / nutz_tab$`Area size [ha]`


nutz_tab <- nutz_tab[c(1:3, ncol(nutz_tab), 4:(ncol(nutz_tab)-1))]


desired_order <- c("Gabower_Hangkante", "Pimpinellenberg", "Kl_Rummelsberg", 
                   "Mühlenberg", "Schloßberg", 
                   "Geesower_Hügel", "Kanonenberg", 
                   "Krähen- & Jungfernberge TF Jungfernspitze", 
                   "Krähen- & Jungfernberge TF Enzian")

nutz_tab$Site[nutz_tab$Site=="Krähen- & Jungfernberge TF ENZIAN"] <- 
  "Krähen- & Jungfernberge TF Enzian"
nutz_tab$Site[nutz_tab$Site=="Krähen- & Jungfernberge TF JUNGFERNSPITZE"] <- 
  "Krähen- & Jungfernberge TF Jungfernspitze"

