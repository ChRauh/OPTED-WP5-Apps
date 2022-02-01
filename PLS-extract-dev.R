###################################################################
# Project:  OPTED WP5 Apps
# Task:     Develop and design desired outputs for the "extractor" app
# Author:   Christian Rauh (01.02.2022)
###################################################################

# Packages ####
library(tidyverse) # 1.3.0
library(quanteda) # 3.2.0
library(plotly) # 4.10.0
library(gmodels)


# Example data from ParlSpeech V2 ####
# Include here all parliaments that shall be part of the app

bt <- read_rds("D:/Dropbox/ParlSpeechV2/Corpora/V2-Corpora/Corp_Bundestag_V2.rds") %>% 
  filter(!chair) %>% # Drop speeches from the chair
  select(-chair) %>% # Drop chair marker
  select(-iso3country) %>% 
  mutate(speechnumber = as.numeric(speechnumber)) %>%     # Attention!
  mutate(id = row_number(),                              # ID to enable restoring of order later
           count = 1,                                      # Simple counter for aggregation purposes
           month = str_remove(date, "-[0-9]{2}$")) %>%     # Month of speech, for aggregation
  relocate(parliament, id, count, date, month) %>%
  select(-speechnumber)

hc <- read_rds("D:/Dropbox/ParlSpeechV2/Corpora/V2-Corpora/Corp_HouseOfCommons_V2.rds") %>% 
  filter(!chair) %>% # Drop speeches from the chair
  select(-chair) %>% # Drop chair marker
  select(-iso3country) %>% 
  mutate(speechnumber = as.numeric(speechnumber)) %>%     # Attention!
  mutate(id = row_number(),                              # ID to enable restoring of order later
         count = 1,                                      # Simple counter for aggregation purposes
         month = str_remove(date, "-[0-9]{2}$")) %>%     # Month of speech, for aggregation
  relocate(parliament, id, count, date, month) %>%
  select(-speechnumber)


# Sample for app testing ####

bt <- bt[sample(nrow(bt), 10000, replace = F) ,]
hc <- hc[sample(nrow(hc), 10000, replace = F) ,]


# Add mock law identifiers ####
bt$law <- NA
for (i in 1:nrow(bt)) {
  bt$law[i] <- sample(c("Gesetz 1", "Gesetz 2", "Gesetz 3", "Gesetz 4", "Gesetz 5"), 1, replace = T)
}
table(bt$law)

hc$law <- NA
for (i in 1:nrow(hc)) {
  hc$law[i] <- sample(c("Law 1", "Law 2", "Law 3", "Law 4", "Law 5"), 1, replace = T)
}
table(hc$law)

# Export data to app folder ####
write_rds(bt, "./PLS-extract/Data/bt-mock.rds")
write_rds(hc, "./PLS-extract/Data/hc-mock.rds")


# Autocompletion lists ####

ac_speakers <- c(unique(bt$speaker), unique(hc$speaker)) %>% 
  sort() %>% 
  write_rds("./PLS-extract/Data/ac-speakers.rds")

ac_laws <- c(unique(bt$law), unique(hc$law)) %>% 
  sort() %>% 
  write_rds("./PLS-extract/Data/ac-laws.rds")

ac_parties <- c(unique(bt$party), unique(hc$party)) %>% 
  sort() %>% 
  write_rds("./PLS-extract/Data/ac-parties.rds")
