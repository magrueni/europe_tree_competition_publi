### creating initital forest states from simulation database ----

library(dplyr)
library(DBI)

path <- "/.../"

### load data --------------------------------

expls <- read_csv(paste0(path, "/data/dnn_training_samples/training_samples.csv"))
expls_aug <- read_csv(paste0(path, "/data/dnn_training_samples/augmentation_training_samples.csv"))

expls_all <- rbind(expls %>% dplyr::select(svd_state), expls_aug %>% dplyr::select(svd_state))

species <- c("Fagus_sylvatica", "Pinus_sylvestris", "Abies_alba", "Picea_abies", 
             "Larix_decidua", "Betula_pendula", "Pinus_halepensis", "Quercus_robur",
             "Quercus_ilex")

# loop over all species to analyse predictions
lookup_table_single <- list()
lookup_table_mixed <- list()


for(s in 1:length(species)){
  
  
  nam <- gsub("_", " ", species[s])
  nam2 <- species[s]
  # print(nam2)
  nam_short <- toupper(paste0(substring(strsplit(nam2, "_")[[1]][1], 1, 2), substring(strsplit(nam2, "_")[[1]][2], 1, 2)))
  if(nam == "Pinus pinea"){nam_short <- "PIPN"}
  if(nam == "Fagus sylvatica"){nam2 <- "Fagus_sylvatica_sylvatica"}
  if(nam == "Larix decidua"){nam2 <- "Larix_decidua_decidua"}
  if(nam == "Quercus ilex"){nam2 <- "Quercus_ilex_ilex"}
  
  
  
  #For species "ABAL"
  if (nam_short == "ABAL") {
    
    height_classes <- c("10_12", "12_14", "14_16", "16_18", "18_20")
    abal <- expls_all %>% filter(grepl("ABAL", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(grepl("ABAL_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      filter(n >= 10)
    
    lookup_table_single[["ABAL"]] <- c(abal$svd_state)
    
    abal <- expls_all %>% filter(grepl("ABAL", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(!grepl("ABAL_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      top_n(10) %>% 
      filter(n >= 5)
    lookup_table_mixed[["ABAL"]] <- c(abal$svd_state)
  }
  
  
  # then PIAB
  if (nam_short == "PIAB"){
    
    
    height_classes <- c("10_12", "12_14", "14_16", "16_18")
    
    piab <- expls_all %>% filter(grepl("PIAB", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(grepl("PIAB_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      filter(n >= 10)
    lookup_table_single[["PIAB"]] <- c(piab$svd_state)
    
    
    piab <- expls_all %>% filter(grepl("PIAB", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(!grepl("PIAB_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      top_n(10) %>% 
      filter(n >= 5)
    lookup_table_mixed[["PIAB"]] <- c(piab$svd_state)
    
  }
  
  
  
  # then PISY
  if (nam_short == "PISY"){
    
    
    height_classes <- c("6_8", "8_10", "10_12", "12_14")
    
    pisy <- expls_all %>% filter(grepl("PISY", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(grepl("PISY_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      filter(n >= 10)
    lookup_table_single[["PISY"]] <- c(pisy$svd_state)
    
    
    pisy <- expls_all %>% filter(grepl("PISY", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(!grepl("PISY_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      top_n(10) %>% 
      filter(n >= 5)
    lookup_table_mixed[["PISY"]] <- c(pisy$svd_state)
    
  }
  
  
  
  # then FASY
  if (nam_short == "FASY"){
    
    
    height_classes <- c("6_8", "8_10", "10_12", "12_14")
    
    fasy <- expls_all %>% filter(grepl("FASY", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(grepl("FASY_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      filter(n >= 10)
    lookup_table_single[["FASY"]] <- c(fasy$svd_state)
    
    
    fasy <- expls_all %>% filter(grepl("FASY", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(!grepl("FASY_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      top_n(10) %>% 
      filter(n >= 5)
    lookup_table_mixed[["FASY"]] <- c(fasy$svd_state)
    
  }
  
  
  
  # then LADE
  if (nam_short == "LADE"){
    
    
    height_classes <- c("8_10", "10_12", "12_14", "14-16")
    
    lade <- expls_all %>% filter(grepl("LADE", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(grepl("LADE_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      filter(n >= 10)
    lookup_table_single[["LADE"]] <- c(lade$svd_state)
    
    
    lade <- expls_all %>% filter(grepl("LADE", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(!grepl("LADE_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      top_n(10) %>% 
      filter(n >= 5)
    lookup_table_mixed[["LADE"]] <- c(lade$svd_state)
    
  }
  
  
  
  
  # then BEPE
  if (nam_short == "BEPE"){
    
    
    height_classes <- c("4_6", "6_8", "8_10")
    
    bepe <- expls_all %>% filter(grepl("BEPE", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(grepl("BEPE_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      filter(n >= 10)
    lookup_table_single[["BEPE"]] <- c(bepe$svd_state)
    
    
    bepe <- expls_all %>% filter(grepl("BEPE", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(!grepl("BEPE_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      top_n(10) %>% 
      filter(n >= 5)
    lookup_table_mixed[["BEPE"]] <- c(bepe$svd_state)
    
  }
  
  
  
  # then PIHA
  if (nam_short == "PIHA"){
    
    
    height_classes <- c("4_6", "6_8")
    
    piha <- expls_all %>% filter(grepl("PIHA", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(grepl("PIHA_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      filter(n >= 10)
    lookup_table_single[["PIHA"]] <- c(piha$svd_state)
    
    
    piha <- expls_all %>% filter(grepl("PIHA", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(!grepl("PIHA_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      top_n(10) %>% 
      filter(n >= 5)
    lookup_table_mixed[["PIHA"]] <- c(piha$svd_state)
    
  }
  
  
  
  # then QURO
  if (nam_short == "QURO"){
    
    
    height_classes <- c("6_8", "8_10", "10_12", "12_14")
    
    quro <- expls_all %>% filter(grepl("QURO", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(grepl("QURO_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      filter(n >= 10)
    lookup_table_single[["QURO"]] <- c(quro$svd_state)
    
    
    quro <- expls_all %>% filter(grepl("QURO", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(!grepl("QURO_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      top_n(10) %>% 
      filter(n >= 5)
    lookup_table_mixed[["QURO"]] <- c(quro$svd_state)
    
  }
  
  
  # then QUIL
  if (nam_short == "QUIL"){
    
    
    height_classes <- c("4_6", "6_8")
    
    quil <- expls_all %>% filter(grepl("QUIL", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(grepl("QUIL_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      filter(n >= 10)
    lookup_table_single[["QUIL"]] <- c(quil$svd_state)
    
    
    quil <- expls_all %>% filter(grepl("QUIL", svd_state)) %>% 
      group_by(svd_state) %>% 
      summarize(n = n()) %>% arrange(-n) %>% 
      filter(!grepl("QUIL_", svd_state)) %>% 
      filter(grepl(paste(height_classes, collapse = "|"), svd_state)) %>%
      top_n(10) %>% 
      filter(n >= 5)
    lookup_table_mixed[["QUIL"]] <- c(quil$svd_state)
    
  }
  
}


